## INTERNAL

#' Function to save final processed data as CSV and Rdata files to output
#' directory
#'
#' @param df data frame of processed data
#' @param state string - state abbreviation
#'
#' @keywords internal
#' @noRd
format_data <- function(df, state){

  # Initialize empty list
  final_df.list <- c()

  for (name_index in 1:length(df)){

    # Pull county name for index
    county_name <- names(df)[name_index]

    # Pull df with specific county
    pm_data <- df[[county_name]]

    # Correct format
    pm_data <- as.data.frame(pm_data)

    # Select desired columns
    pm_data <-
      if (state != "DC") {
        pm_data %>%
          dplyr::select(.data$GEOID, .data$NAMELSAD, .data$Particulate.Matter)
      } else {
        pm_data %>%
          dplyr::select(.data$GEOID, .data$NAME, .data$Particulate.Matter)
      }

    # Adds column with county and state
    pm_data <- pm_data %>%
      tibble::add_column(county_state = rep(paste0(county_name, "_", state), nrow(pm_data)), .before = "GEOID")

    # Add data as new row to list of dataframes
    final_df.list <- append(final_df.list, list(pm_data))
  }

  # Compile into one dataframe for state
  final_df <- dplyr::bind_rows(final_df.list)

  return(final_df)
}

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' census tract
#'
#' @param census, dataframe of features for a given census tract
#' @param new_acag, rasterLayer object with PM2.5 levels
#'
#' @return Modified census dataframe with mean area-weighted PM2.5 levels
#'
#' @keywords internal
#' @noRd
get_census_pm <- function(census, new_acag) {

  sf::sf_use_s2(use_s2 = FALSE)

  # Convert shapefile to spatial object
  new_geo_sp <- as(census, "Spatial")

  # Crop acag data to match census tract boundary
  acag_crop <- raster::crop(new_acag, new_geo_sp, snap = "out")

  # Converts spatial object with acag data to dataframe, including spatial coordinates
  acag_df <- data.frame(raster::rasterToPoints(acag_crop))

  # Convert spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  acag_sp <- as(acag_crop, "SpatialPolygonsDataFrame")

  # Convert to an sf object
  acag_sf <- sf::st_as_sf(acag_sp)

  # Pulls geometry from spatial object converted to sf
  new_geo_sf <- sf::st_as_sf(new_geo_sp)$geometry

  # Combines objects together to have PM2.5 levels with geometries
  census_acag_int <- sf::st_intersection(acag_sf, new_geo_sf)

  # Area of each polygon
  census_acag_int$grid_area <- as.numeric(sf::st_area(census_acag_int$geometry))


  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(sf::st_area(new_geo_sf))
  census_acag_int$weight <- census_acag_int$grid_area / ct_area

  # Tract value is weighted average of PM2.5 levels
  this_ct_val <- sum(census_acag_int$Value * census_acag_int$weight) / sum(census_acag_int$weight)

  census$Particulate.Matter <- this_ct_val

  gc()

  return(census)
}

#' Helper function to pull census tract shapefile for a county and initialize
#' particulate matter column to be passed to get_census_pm
#'
#' @param cty_row, dataframe of single county with geometry
#' @param st, character string representing a state
#'
#' @return Dataframe of tract level data for county and state with empty PM column
#'
#' @keywords internal
#' @noRd
get_census_geo <- function(cty_row, st, level, tract_df.st) {

  # Pull county name
  cty <- cty_row$COUNTYFP

  # Get specific county shape info
  county_geo <- tigris::tracts(state = st, county = cty, year = 2020) %>%
    dplyr::mutate(INTPTLAT = as.numeric(.data$INTPTLAT),
           INTPTLON = as.numeric(.data$INTPTLON))

  if (level == "Tract"){
    county_geo <- county_geo %>%
      dplyr::filter(.data$GEOID %in% tract_df.st$tract_county_state)
  }

  # Initialize particulate matter column
  new_geo <- county_geo %>%
    dplyr::mutate(Particulate.Matter = NA_real_) %>%
    dplyr::filter(.data$AWATER / as.numeric(sf::st_area(.data$geometry)) < .9) # Remove tracts that are over 90

  return(new_geo)
}


#' Function to compute census tract PM2.5 levels for each county
#'
#' @param st, character string representing a state's GEOID
#' @param acag, rasterLayer object containing PM2.5 levels for the USA
#'
#' @return Dataframe object containing GEOID, census tract, and PM2.5 levels for
#' census tracts in a given county
#'
#' @keywords internal
#' @noRd
get_county_geo <- function(st, tract_df, level, acag) {

  # Get Counties in state, filter to cty_df only in state
  if (!is.null(tract_df)){
    tract_df.st <- tract_df %>%
      dplyr::filter(.data$state == st)
  }

  # Load shapefile for counties in given state

  geo <-
    if (st != "11") {
      tigris::counties(st, year = 2020) %>%
        dplyr::mutate(INTPTLAT = as.numeric(.data$INTPTLAT),
                INTPTLON = as.numeric(.data$INTPTLON))
    } else {
      tigris::counties(st, cb = T, year = 2020)
    }


  # Convert to a list to be able to use with llply
  cty_row.list <-
    setNames(split(geo, seq(nrow(geo))), geo[["NAMELSAD"]])

  # Filter to selected counties
  if (level == "County" || level == "Tract"){
    cty_row.list <-
      cty_row.list[unlist(lapply(names(cty_row.list), function(x){
        x %in% tract_df.st$county.name}))]
  }

  # Get census tract shape info
  new_geo <- lapply(cty_row.list, get_census_geo, st = st, level, tract_df.st)

  # CRS needs to line up
  new_acag <- raster::projectRaster(acag, crs = raster::crs(new_geo[[1]]))
  new_acag@data@names <- "Value"

  # Assign rownames
  new_geo.list <- lapply(new_geo, function(df){
    setNames(split(df, seq(nrow(df))), df$GEOID)
  })

  # Compute mean area concentration for CTs
  nodes <- parallel::detectCores()
  cl <- parallel::makeCluster(nodes)
  doParallel::registerDoParallel(cl)

  final_geo <- lapply(new_geo.list, function(df){
    plyr::llply(df, get_census_pm, new_acag = new_acag, .parallel = TRUE, .paropts = list(.packages = c("raster", "sf")))
  })

  # Combine into single df
  final_geo.df <- lapply(final_geo, dplyr::bind_rows)

  state.name <- tigris::states() %>%
    dplyr::filter(.data$STATEFP == st) %>%
    dplyr::pull(.data$STUSPS)

  # Save
  return(format_data(df = final_geo.df, state = state.name))
}

## EXTERNAL

#' Tract level particulate matter data
#'
#' Pulls PM2.5 data at a census tract granularity with level selections, either
#' internally or as a new pull. Years 2015 through 2018 are pre-available within
#' the package, but years 2014 and earlier are available as a pull combining
#' PM2.5 raster files and tigris shape files. More information on external pulls
#' contained in the vignette. Data for Alaska and Hawaii is not available.
#'
#' @param pull_type, character string representing whether internal data is pulled or new processing is performed. "Internal" or "External"
#' @param year, numeric object representing a selected year. Used if pull_type is "Internal"
#' @param level, character string representing desired level of pull. "National", "State", "County", or "Tract"
#' @param state, character vector of selected states via GEOID. Used if level is "State"
#' @param county_state, character vector of selected counties via GEOID. Used if level is "County"
#' @param tract_county_state, character vector of selected tracts via GEOID. Used if level is "Tract"
#' @param acag, particulate matter raster object. Used if pull_type is "External"
#'
#' @return Dataframe object broken down by census tracts with
#' mean area-weighted PM2.5 values in micro-grams per cubic meter.
#'
#' @examples
#' acag_pm_dat_tract <- pull_tract_ACAG(pull_type = "Internal",
#'                                      year = 2016,
#'                                      level = "National")
#' acag_pm_dat_tract <- pull_tract_ACAG(pull_type = "Internal",
#'                                      year = 2016,
#'                                      level = "State",
#'                                      state = c("01", "05", "04"))
#' acag_pm_dat_tract <- pull_tract_ACAG(pull_type = "Internal",
#'                                      year = 2016,
#'                                      level = "County",
#'                                      county_state = c("45001", "22001", "51001"))
#' acag_pm_dat_tract <- pull_tract_ACAG(pull_type = "Internal",
#'                                      year = 2016,
#'                                      level = "Tract",
#'                                      tract_county_state = c("45001950200", "45001950300"))
#' @export
pull_tract_ACAG <- function(pull_type, year = NULL, level, state = c(), county_state = c(), tract_county_state = c(), acag = NULL){

  # Pre-available years of data
  available_years <- as.numeric(list.dirs(path = system.file(file.path("extdata", "output"), package = "ACAGPM"),
                                          full.names = FALSE,
                                          recursive = FALSE))

  tract_lookup <- NULL
  load(system.file(file.path("extdata", "input", "tract_lookup.RData"), package = "ACAGPM"))

  tract_df <- NULL

  if (level == "National"){
    st <- unique(tract_lookup$STATEFP)

    county_state <- tract_lookup %>%
      dplyr::filter(.data$STATEFP %in% st) %>%
      dplyr::pull(.data$COUNTYFP)

    county_state.name <- unique(tract_lookup %>% dplyr::mutate(temp = paste0(.data$NAMELSAD.COUNTY, "_", .data$STUSPS)) %>% dplyr::pull(.data$temp))
    county_state.name <- gsub(" ", "-", county_state.name)
  } else if (level == "State"){
    if (class(state) != "character"){
      stop("Improper input, must be character")
    }
    if (all(state %in% unique(tract_lookup$STATEFP))){
      st <- state

      county_state <- tract_lookup %>%
        dplyr::filter(.data$STATEFP %in% st) %>%
        dplyr::pull(.data$COUNTYFP)

      county_state.name <- unique(tract_lookup %>% dplyr::filter(.data$STATEFP %in% st) %>% dplyr::mutate(temp = paste0(.data$NAMELSAD.COUNTY, "_", .data$STUSPS)) %>% dplyr::pull(.data$temp))
      county_state.name <- gsub(" ", "-", county_state.name)
    } else{
      stop("Improper input, unrecognized state")
    }
  } else if (level == "County"){
    if (class(county_state) != "character"){
      stop("Improper input, must be character")
    }
    if (all(county_state %in% unique(tract_lookup$GEOID.COUNTY))){

      # Dataframe of user input and additional information
      tract_df <- tract_lookup %>%
        dplyr::filter(.data$GEOID.COUNTY %in% county_state) %>%
        dplyr::select(.data$COUNTYFP, .data$STATEFP, .data$GEOID.COUNTY, .data$NAMELSAD.COUNTY) %>%
        dplyr::rename(county = .data$COUNTYFP, state = .data$STATEFP, county_state = .data$GEOID.COUNTY, county.name = .data$NAMELSAD.COUNTY)

      st <- unique(tract_df$state)

      county_state.name <- unique(tract_lookup %>% dplyr::filter(.data$GEOID.COUNTY %in% county_state) %>% dplyr::mutate(temp = paste0(.data$NAMELSAD.COUNTY, "_", .data$STUSPS)) %>% dplyr::pull(.data$temp))
      county_state.name <- gsub(" ", "-", county_state.name)
    } else{
      stop("Improper input, unrecognized county")
    }
  } else if (level == "Tract"){
    if (class(tract_county_state) != "character"){
      stop("Improper input, must be character")
    }
    if (all(tract_county_state %in% tract_lookup$GEOID.TRACT)){

      # Dataframe of user input and additional information
      tract_df <- tract_lookup %>%
        dplyr::filter(.data$GEOID.TRACT %in% tract_county_state) %>%
        dplyr::select(.data$COUNTYFP, .data$STATEFP, .data$GEOID.TRACT, .data$NAMELSAD.COUNTY) %>%
        dplyr::rename(county = .data$COUNTYFP, state = .data$STATEFP, tract_county_state = .data$GEOID.TRACT, county.name = .data$NAMELSAD.COUNTY)

      st <- unique(tract_df$state)

      county_state.name <- unique(tract_lookup %>% dplyr::filter(.data$GEOID.TRACT %in% tract_county_state) %>% dplyr::mutate(temp = paste0(.data$NAMELSAD.COUNTY, "_", .data$STUSPS)) %>% dplyr::pull(.data$temp))
      county_state.name <- gsub(" ", "-", county_state.name)
    } else{
      stop("Improper input, unrecognized tract")
    }
  } else{
    stop("Improper input, unrecognized level")
  }

  # If available year selected, returns csv corresponding to year as a dataframe.
  # Else, returns an object pulled from selected year of data as a dataframe.
  # Returns an error if unrecognized state is selected as input.
  if (pull_type == "Internal"){

    if (year %in% available_years){
    } else{
      stop("Improper input, invalid year")
    }
    # Pulls selected data as list of dataframes
    dflist <- lapply(county_state.name, function(x){
      if (x == "Do\u00F1a-Ana-County_NM"){
        x = "Dona-Ana-County_NM"
      }

      pm_data <- NULL
      load(system.file(file.path("extdata", "output", year, "tract", paste0("acag_pm_dat_", x, ".RData")), package = "ACAGPM"))
      tempdf <- pm_data

      if (level == "Tract"){
        tempdf <- tempdf %>%
         dplyr::filter(.data$GEOID %in% (tract_county_state))
      }

      # Add column with name of county and state
      tempdf <- tempdf %>%
        tibble::add_column(county_state = rep(x, nrow(tempdf)), .before = "GEOID")

      return(tempdf)
    })

    # Convert list of dataframes to dataframe
    ACAG_pm_dat_County <- dplyr::bind_rows(dflist)

    return(ACAG_pm_dat_County)

  } else if(pull_type == "External"){

    # Pull PM2.5 for the given year
    acag <- acag
    if (class(acag) != "RasterLayer"){
      stop("Improper input, acag must be RasterLayer object")
    }
    acag@data@names <- "Value"

    #Perform a pull state by state
    dflist <- lapply(st, get_county_geo, tract_df, level, acag)

    # Convert list of dataframes to dataframe
    ACAG_pm_dat_County <- dplyr::bind_rows(dflist)

    return(ACAG_pm_dat_County)

  } else{
    stop("Improper input, pull_type must be Internal or External")
  }
}
