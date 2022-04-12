## INTERNAL

#' Function to save final processed data as CSV and Rdata files to output
#' directory
#'
#' @param df data frame of processed data
#' @param state string - state abbreviation
#'
#' @keywords internal
#' @noRd
save_data <- function(df, state){

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
          dplyr::select(GEOID, NAMELSAD, Particulate.Matter)
      } else {
        pm_data %>%
          dplyr::select(GEOID, NAME, Particulate.Matter)
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
  county_geo <- tigris::tracts(state = st, county = cty) %>%
    dplyr::mutate(INTPTLAT = as.numeric(INTPTLAT),
           INTPTLON = as.numeric(INTPTLON))

  if (level == "Tract"){
    county_geo <- county_geo %>%
      dplyr::filter(GEOID %in% tract_df.st$tract_county_state)
  }

  # Initialize particulate matter column
  new_geo <- county_geo %>%
    dplyr::mutate(Particulate.Matter = NA_real_) %>%
    dplyr::filter(AWATER / as.numeric(sf::st_area(geometry)) < .9) # Remove tracts that are over 90

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
      dplyr::filter(state == st)
  }

  # Load shapefile for counties in given state

  geo <-
    if (st != "11") {
      tigris::counties(st) %>%
        dplyr::mutate(INTPTLAT = as.numeric(INTPTLAT),
                INTPTLON = as.numeric(INTPTLON))
    } else {
      tigris::counties(st, cb = T)
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
    dplyr::filter(STATEFP == st) %>%
    dplyr::pull(STUSPS)

  # Save
  return(save_data(df = final_geo.df, state = state.name))
}

## EXTERNAL

#' Tract level particulate matter data
#'
#' Pulls PM2.5 data at a census tract granularity with level selections, either
#' internally or as a new pull. Years 2015 through 2018 are pre-available within
#' the package, but years 2014 and earlier are available as a pull combining
#' PM2.5 raster files and tigris shape files.
#'
#' @param year, numeric object representing a selected year
#' @param level, character string representing desired level of pull
#' @param state, character vector of selected states via GEOID
#' @param county_state, character vector of selected counties via GEOID
#' @param tract_county_state, character vector of selected tracts via GEOID
#'
#' @return Dataframe object broken down by census tracts with
#' mean area-weighted PM2.5 values.
#'
#' @examples
#' acag_pm_dat_tract <- pull_tract_ACAG(year = 2016, level = "National")
#' acag_pm_dat_tract <- pull_tract_ACAG(year = 2016, level = "State", state = c("01", "05", "04"))
#' acag_pm_dat_tract <- pull_tract_ACAG(year = 2016, level = "County", county_state = c("45001", "22001", "51001"))
#' acag_pm_dat_tract <- pull_tract_ACAG(year = 2016, level = "Tract", tract_county_state = c("45001950200", "45001950300", "45001950600"))
#' @export
pull_tract_ACAG <- function(year, level, state = c(), county_state = c(), tract_county_state = c()){

  # Pre-available years of data
  available_years <- c(2015, 2016, 2017, 2018)

  tract_lookup <- read.csv(system.file(file.path("data", "input", "tract_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8", colClasses = c("STATEFP" = "character", "COUNTYFP" = "character", "GEOID.TRACT" = "character", "NAME.TRACT" = "character", "GEOID.STATE" = "character", "GEOID.COUNTY" = "character"))

  tract_df <- NULL

  if (level == "National"){
    st <- unique(tract_lookup$STATEFP)

    county_state <- tract_lookup %>%
      dplyr::filter(STATEFP %in% st) %>%
      dplyr::pull(COUNTYFP)

    county_state.name <- unique(tract_lookup %>% dplyr::mutate(temp = paste0(NAMELSAD.COUNTY, "_", STUSPS)))
  } else if (level == "State"){
    if (all(state %in% unique(tract_lookup$STATEFP))){
      st <- state

      county_state <- tract_lookup %>%
        dplyr::filter(STATEFP %in% st) %>%
        dplyr::pull(COUNTYFP)

      county_state.name <- unique(tract_lookup %>% dplyr::filter(STATEFP %in% st) %>% dplyr::mutate(temp = paste0(NAMELSAD.COUNTY, "_", STUSPS)) %>% dplyr::pull(temp))
    } else{
      stop("Improper input, unrecognized state")
    }
  } else if (level == "County"){
    if (all(county_state %in% unique(tract_lookup$GEOID.COUNTY))){

      # Dataframe of user input and additional information
      tract_df <- tract_lookup %>%
        dplyr::filter(GEOID.COUNTY %in% county_state) %>%
        dplyr::select(COUNTYFP, STATEFP, GEOID.COUNTY, NAMELSAD.COUNTY) %>%
        dplyr::rename(county = COUNTYFP, state = STATEFP, county_state = GEOID.COUNTY, county.name = NAMELSAD.COUNTY)

      st <- unique(tract_df$state)

      county_state.name <- unique(tract_lookup %>% dplyr::filter(GEOID.COUNTY %in% county_state) %>% dplyr::mutate(temp = paste0(NAMELSAD.COUNTY, "_", STUSPS)) %>% dplyr::pull(temp))
    } else{
      stop("Improper input, unrecognized county")
    }
  } else if (level == "Tract"){
    if (all(tract_county_state %in% tract_lookup$GEOID.TRACT)){

      # Dataframe of user input and additional information
      tract_df <- tract_lookup %>%
        dplyr::filter(GEOID.TRACT %in% tract_county_state) %>%
        dplyr::select(COUNTYFP, STATEFP, GEOID.TRACT, NAMELSAD.COUNTY) %>%
        dplyr::rename(county = COUNTYFP, state = STATEFP, tract_county_state = GEOID.TRACT, county.name = NAMELSAD.COUNTY)

      st <- unique(tract_df$state)

      county_state.name <- unique(tract_lookup %>% dplyr::filter(GEOID.TRACT %in% tract_county_state) %>% dplyr::mutate(temp = paste0(NAMELSAD.COUNTY, "_", STUSPS)) %>% dplyr::pull(temp))
    } else{
      stop("Improper input, unrecognized tract")
    }
  } else{
    stop("Improper input, unrecognized level")
  }

  # Initialized dataframe; do we need this?
  ACAG_pm_dat_County <- data.frame(tract_county_state = character(),
                                  GEOID = character(),
                                  NAME = character(),
                                  Particulate.Matter = numeric())

  # If year 2015-2018 selected, returns csv corresponding to year as a dataframe.
  # Else, returns an object pulled from selected year of data as a dataframe.
  # Returns an error if unrecognized state is selected as input.
  if (year %in% available_years){

    # Pulls selected data as list of dataframes
    dflist <- lapply(county_state.name, function(x){
      # Pull csv for county
      tempdf <- read.csv(system.file(file.path("data", "output", year, "tract", paste0("acag_pm_dat_", x, ".csv")), package = "ACAGPM"), colClasses = c("GEOID" = "character"))

      if (level == "Tract"){
        tempdf <- tempdf %>%
         dplyr::filter(GEOID %in% (tract_county_state))
      }

      # Add column with name of county and state
      tempdf <- tempdf %>%
        tibble::add_column(county_state = rep(x, nrow(tempdf)), .before = "GEOID")

      return(tempdf)
    })

    # Convert list of dataframes to dataframe
    ACAG_pm_dat_County <- dplyr::bind_rows(dflist)

    return(ACAG_pm_dat_County)

  } else{

    # Pull PM2.5 for the given year
    acag <- raster::raster(system.file(file.path("data", "input", "acag_raw_data_files", paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")),
                                 package = "ACAGPM"))
    acag@data@names <- "Value"

    ## Keeping these in case we want to define new_acag once
    # crs_args <- "+proj=longlat +datum=NAD83 +no_defs"
    # new_acag <- raster::raster(
    #   file.path(
    #     "data",
    #     "input",
    #     "acag_raw_data_files",
    #     paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
    #   ),
    #   crs = crs_args)
    # new_acag@data@names <- "Value"

    #Perform a pull state by state
    dflist <- lapply(st, get_county_geo, tract_df, level, acag)

    # Convert list of dataframes to dataframe
    ACAG_pm_dat_County <- dplyr::bind_rows(dflist)

    return(ACAG_pm_dat_County)

  }
}
