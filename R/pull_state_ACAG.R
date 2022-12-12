## INTERNAL

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' state
#'
#' @param state, dataframe of features for a given state
#' @param new_acag, rasterLayer object with PM2.5 levels
#'
#' @return Modified state dataframe with mean area-weighted PM2.5 levels
#'
#' @keywords internal
#' @noRd
get_pm_data <- function(state, new_acag){

  sf::sf_use_s2(use_s2 = FALSE)

  # Converts shapefile to spatial object
  new_geo_sp <- as(state, "Spatial")

  # Crops nationwide PM2.5 raster file to within state's bounds
  acag_crop <- raster::crop(new_acag, new_geo_sp, snap = "out")

  # Convert spatial object with PM2.5 levels to dataframe, including spatial coordinates
  acag_df <- data.frame(raster::rasterToPoints(acag_crop))

  # Convert spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  acag_sp <- as(acag_crop, "SpatialPolygonsDataFrame")

  # Convert to an sf object
  acag_sf <- sf::st_as_sf(acag_sp)

  # Pulls geometry from Spatial object converted to sf
  new_geo_sf <- sf::st_as_sf(new_geo_sp)$geometry

  # Combines objects together to have PM2.5 levels with geometries
  census_acag_int <- sf::st_intersection(acag_sf, new_geo_sf)

  # Adds column corresponding to area of each polygon
  census_acag_int$grid_area <- as.numeric(sf::st_area(census_acag_int$geometry))

  # Make weight = area/total state area -- i.e. percent coverage
  ct_area <- as.numeric(sf::st_area(new_geo_sf))
  census_acag_int$weight <- census_acag_int$grid_area/ct_area

  # State value is weighted average of PM2.5 levels
  this_ct_val <- sum(census_acag_int$Value * census_acag_int$weight)/sum(census_acag_int$weight)

  state$Particulate.Matter <- this_ct_val
  gc()
  return(state)
}


#' Helper function to compute PM2.5 concentrations for every census tract for each
#' state
#'
#' @param st, character string representing a state's GEOID
#' @param acag, rasterLayer object containing PM2.5 levels for the USA
#'
#' @return Dataframe object containing GEOID, state, and PM2.5 levels for a
#' given state
#'
#' @keywords internal
#' @noRd
get_national_geo <- function(st, acag){

  # Load shapefile for given state
  geo <- tigris::states(cb = T, year = 2019) %>%
    dplyr::filter(.data$GEOID %in% st)

  # CRS needs to line up
  new_acag <- raster::projectRaster(acag, crs = "+proj=longlat +datum=NAD83 +no_defs")
  new_acag@data@names <- "Value"

  # Initialize particulate matter column
  new_geo <- geo %>%
    dplyr::mutate(Particulate.Matter = NA_real_)

  # Convert df to a list to be able to parallelize in mclapply
  new_geo.list <-
    setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))

  # Perform parallel computation
  nodes <- parallel::detectCores()
  cl <- parallel::makeCluster(nodes)
  doParallel::registerDoParallel(cl)

  new_geo.list_PM <- plyr::llply(new_geo.list, get_pm_data, new_acag = new_acag, .parallel = TRUE, .paropts = list(.packages = c("raster", "sf")))
  pm_data <- new_geo.list_PM %>% dplyr::bind_rows() # Back to df

  # Pull desired columns
  pm_data <- as.data.frame(pm_data) %>%
    dplyr::select(.data$GEOID, .data$NAME, .data$Particulate.Matter)

  return(pm_data)
}


## EXTERNAL

#' State level particulate matter data
#'
#' Pulls PM2.5 data at a state granularity with level selections, either
#' internally or as a new pull. Years 2015 through 2018 are pre-available within
#' the package, but years 2014 and earlier are available as a pull combining
#' PM2.5 raster files and tigris shape files. More information on external pulls
#' contained in the vignette. Data for Alaska and Hawaii is not available.
#'
#' @param pull_type, character string representing whether internal data is pulled or new processing is performed. "Internal" or "External"
#' @param year, numeric object representing a selected year. Used if pull_type is "Internal"
#' @param level, character string representing desired level of pull. "National" or "State"
#' @param state, character vector of selected states via GEOID. Used if level is "State"
#' @param acag, particulate matter raster object. Used if pull_type is "External"
#'
#' @return Dataframe object broken down by state with mean area-weighted PM2.5
#' values in micro-grams per cubic meter.
#'
#' @examples
#' acag_pm_dat_state <- pull_state_ACAG(pull_type = "Internal",
#'                                      year = 2016,
#'                                      level = "National")
#' acag_pm_dat_state <- pull_state_ACAG(pull_type = "Internal",
#'                                      year = 2016,
#'                                      level = "State",
#'                                      state = c("01", "05", "04"))
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom dplyr %>%
#' @importFrom methods as
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @export
pull_state_ACAG <- function(pull_type, year = NULL, level, state = c(), acag = NULL){

  # Pre-available years
  available_years <- as.numeric(list.dirs(path = system.file(file.path("extdata", "output"), package = "ACAGPM"),
                                          full.names = FALSE,
                                          recursive = FALSE))

  # Does not include Alaska or Hawaii!
  state_lookup <- NULL
  load(system.file(file.path("extdata", "input", "state_lookup.RData"), package = "ACAGPM"))

  file_path <- system.file(file.path("extdata", "output", year, "state", "acag_pm_dat_US.RData"),
                           package = "ACAGPM")

  # If available year selected, returns csv corresponding to year as a dataframe.
  # Else, returns an object pulled from selected year of data as a dataframe.
  if (pull_type == "Internal"){
    if (year %in% available_years){
    } else{
      stop("Improper input, invalid year")
    }

    pm_data <- NULL
    load(file_path)
    acag_pm_dat_US <- pm_data

    # If level is state, pulls selected states from data. If invalid state is
    # entered, an error is thrown.
    if (level == "National"){

    } else if (level == "State"){
      if (class(state) != "character"){
        stop("Improper input, must be character")
      }


      if (all(state %in% state_lookup$GEOID)){
        acag_pm_dat_US <- acag_pm_dat_US %>%
          dplyr::filter(.data$GEOID %in% state)
      } else{
        stop("Improper input, unrecognized state")
      }
    } else{
      stop("Improper input, unrecognized level")
    }
    return(acag_pm_dat_US)
  } else if (pull_type == "External"){
    # Pull PM2.5 data for the given year
    acag <- acag
    if (class(acag) != "RasterLayer"){
      stop("Improper input, acag must be RasterLayer object")
    }
    acag@data@names <- "Value"

    # If level is state, sets pull to selected states. If invalid state is
    # entered, an error is thrown.
    if (level == "National"){
      st <- state_lookup$GEOID
    } else if (level == "State"){
      if (class(state) != "character"){
        stop("Improper input, must be character")
      }
      if (all(state %in% state_lookup$GEOID)){
        st <- state
      } else{
        stop("Improper input, unrecognized state")
      }
    } else{
      stop("Improper input, unrecognized level")
    }

    # Perform a pull, state by state
    acag_pm_dat_US <- get_national_geo(st = st, acag = acag)

    return(acag_pm_dat_US)
  } else{
    stop("Improper input, pull_type must be Internal or External")
  }

}
