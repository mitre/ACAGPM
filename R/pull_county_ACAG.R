## INTERNAL

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' county
#'
#' @param census, dataframe of features for a given county
#' @param new_acag, rasterLayer object with PM2.5 levels
#'
#' @return Modified census dataframe with mean area-weighted PM2.5 levels
#'
#' @keywords internal
#' @noRd
get_county_pm <- function(census, new_acag){

  sf::sf_use_s2(use_s2 = FALSE)

  # Convert shapefile to spatial object
  new_geo_sp <- as(census, "Spatial")

  # Crops nationwide PM2.5 raster file to within county's bounds
  acag_crop <- raster::crop(new_acag, new_geo_sp, snap = "out")

  # Converts spatial object with PM2.5 levels to dataframe, including spatial coordinates
  acag_df <- data.frame(raster::rasterToPoints(acag_crop))

  # Converts spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  acag_sp <- as(acag_crop, "SpatialPolygonsDataFrame")

  # Convert to an sf object
  acag_sf <- sf::st_as_sf(acag_sp)

  # Pulls geometry from spatial object converted to sf
  new_geo_sf <- sf::st_as_sf(new_geo_sp)$geometry

  # Combines objects together to have PM2.5 levels with geometries
  census_acag_int <- sf::st_intersection(acag_sf, new_geo_sf)

  # Adds column corresponding to area of each polygon
  census_acag_int$grid_area <- as.numeric(sf::st_area(census_acag_int$geometry))

  # Make weight = area/total county area -- i.e. percent coverage
  ct_area <- as.numeric(sf::st_area(new_geo_sf))
  census_acag_int$weight <- census_acag_int$grid_area/ct_area

  # Tract value is weighted average of PM2.5 levels
  this_ct_val <- sum(census_acag_int$Value * census_acag_int$weight)/sum(census_acag_int$weight)

  census$Particulate.Matter <- this_ct_val

  gc()

  return(census)
}

#' Function to compute county PM2.5 levels for each state
#'
#' @param st, character string representing a state's GEOID
#' @param acag, rasterLayer object containing PM2.5 levels for the USA
#' @param county_df, dataframe containing county data
#'
#' @return Dataframe object containing GEOID, county name, and PM2.5 levels for
#' counties in a given state
#'
#' @keywords internal
#' @noRd
get_state_geo <- function(st, acag, county_df){

  if (!is.null(county_df)){
    county_names <- county_df %>%
      dplyr::filter(.data$state == st) %>%
      dplyr::pull(.data$county_state)
  }

  # Load shapefile for counties in given state
  new_geo <-
    if (st != "11") {
      tigris::counties(st, year = 2019) %>%
        dplyr::mutate(INTPTLAT = as.numeric(.data$INTPTLAT),
               INTPTLON = as.numeric(.data$INTPTLON))
    } else {
      tigris::counties(st, cb = T, year = 2019)
    }

  if (!is.null(county_df)){
    new_geo <- new_geo %>%
      dplyr::filter(.data$GEOID %in% county_names)
  }

  # CRS needs to line up
  new_acag <- raster::projectRaster(acag, crs = raster::crs(new_geo))
  new_acag@data@names <- "Value"

  # Convert df to a list to be able to parallelize in mclapply
  new_geo.list <-
    setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))

  # Perform parallel computation, which pulls PM2.5 levels for each county in the state
  nodes <- parallel::detectCores()
  cl <- parallel::makeCluster(nodes)
  doParallel::registerDoParallel(cl)

  new_geo.list_PM <- plyr::llply(new_geo.list, get_county_pm, new_acag = new_acag, .parallel = TRUE, .paropts = list(.packages = c("raster", "sf")))
  pm_data <- do.call("rbind", new_geo.list_PM) # Back to df

  # Pull desired columns
  pm_data <- as.data.frame(pm_data)
  pm_data <-
    if (st != "11") {
      pm_data %>%
        dplyr::select(.data$STATEFP, .data$GEOID, .data$NAMELSAD, .data$Particulate.Matter)
    } else {
      pm_data %>%
        dplyr::select(.data$STATEFP, .data$GEOID, .data$NAME, .data$Particulate.Matter)
    }

  return(pm_data)
}

## EXTERNAL

#' County level particulate matter data
#'
#' Pulls PM2.5 data at a county granularity with level selections, either
#' internally or as a new pull. Years 2015 through 2018 are pre-available within
#' the package, but years 2014 and earlier are available as a pull combining
#' PM2.5 raster files and tigris shape files. More information on external pulls
#' contained in the vignette. Data for Alaska and Hawaii is not available.
#'
#' @param pull_type, character string representing whether internal data is pulled or new processing is performed. "Internal" or "External"
#' @param year, numeric object representing a selected year. Used if pull_type is "Internal"
#' @param level, character string representing desired level of pull. "National", "State", or "County"
#' @param state, character vector of selected states via GEOID. Used if level is "State"
#' @param county_state, character vector of selected counties via GEOID. Used if level "County"
#' @param acag, particulate matter raster object. Used if pull_type is "External"
#'
#' @return Dataframe object broken down by counties with mean
#' area-weighted PM2.5 values in micro-grams per cubic meter.
#'
#' @examples
#' acag_pm_dat_county <- pull_county_ACAG(pull_type = "Internal",
#'                                        year = 2016,
#'                                        level = "National")
#' acag_pm_dat_county <- pull_county_ACAG(pull_type = "Internal",
#'                                        year = 2016,
#'                                        level = "State",
#'                                        state = c("01", "05", "04"))
#' acag_pm_dat_county <- pull_county_ACAG(pull_type = "Internal",
#'                                        year = 2016,
#'                                        level = "County",
#'                                        county_state = c("45001", "22001", "51001"))
#' @export
pull_county_ACAG <- function(pull_type, year = NULL, level, state = c(), county_state = c(), acag = NULL){

  # Pre-available years of data
  available_years <- as.numeric(list.dirs(path = system.file(file.path("extdata", "output"), package = "ACAGPM"),
                               full.names = FALSE,
                               recursive = FALSE))

  county_lookup <- NULL
  load(system.file(file.path("extdata", "input", "county_lookup.RData"), package = "ACAGPM"))

  county_df = NULL

  if (level == "National"){
    st <- unique(county_lookup$STATEFP)
    st.STUSPS <- unique(county_lookup %>% dplyr::filter(.data$STATEFP %in% st) %>% dplyr::pull(.data$STUSPS))
  } else if (level == "State"){
    if (class(state) != "character"){
      stop("Improper input, must be character")
    }
    if (all(state %in% unique(county_lookup$STATEFP))){
      st <- state
      st.STUSPS <- unique(county_lookup %>% dplyr::filter(.data$STATEFP %in% st) %>% dplyr::pull(.data$STUSPS))
    } else{
      stop("Improper input, unrecognized state")
    }
  } else if (level == "County"){
    if (class(county_state) != "character"){
      stop("Improper input, must be character")
    }
    if (all(county_state %in% county_lookup$GEOID.COUNTY)){

      # Creates vector containing states chosen
      county_df <- county_lookup %>%
        dplyr::filter(.data$GEOID.COUNTY %in% county_state) %>%
        dplyr::select(.data$GEOID.COUNTY, .data$GEOID.STATE) %>%
        dplyr::rename(county_state = .data$GEOID.COUNTY, state = .data$GEOID.STATE)

      st <- unique(county_df$state)
      st.STUSPS <- unique(county_lookup %>% dplyr::filter(.data$GEOID.STATE %in% st) %>% dplyr::pull(.data$STUSPS))

    } else{
      stop("Improper input, unrecognized county")
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
      dflist <- lapply(st.STUSPS, function(x){
        pm_data <- NULL
        load(system.file(file.path("extdata", "output", year, "county", paste0("acag_pm_dat_", x, ".RData")), package = "ACAGPM"))
        tempdf <- pm_data

        if (level == "County"){
          tempdf <- tempdf %>%
            dplyr::filter(.data$GEOID %in% county_state)
        }

        # Add column with name of state
        tempdf <- tempdf %>%
          tibble::add_column(state = rep(x, nrow(tempdf)), .before = "GEOID")

        return(tempdf)
      })

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_State <- dplyr::bind_rows(dflist)

      return(ACAG_pm_dat_State)

  } else if (pull_type == "External"){

      # Pull PM2.5 for the given year
      acag <- acag
      if (class(acag) != "RasterLayer"){
        stop("Improper input, acag must be RasterLayer object")
      }
      acag@data@names <- "Value"

      # Perform a pull state by state
      dflist <- lapply(st, get_state_geo, acag, county_df)

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_State <- dplyr::bind_rows(dflist)

      return(ACAG_pm_dat_State)

  } else{
    stop("Improper input, pull_type must be Internal or External")
  }
}
