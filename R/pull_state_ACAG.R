## INTERNAL

#' Function to perform mean area weighting within state to summarise PM2.5
#' concentrations from gridded data
#'
#' @param state, dataframe of features for a given state
#' @param new_acag, rasterLayer object with PM2.5 levels
#'
#' @return Modified state dataframe with mean area-weighted PM2.5 levels
#'
#' @keywords internal
#' @noRd
get_pm_data <- function(state, new_acag){

  # Converts shapefile to spatial object
  new_geo_sp <- as(state, "Spatial")

  # Crops nationwide PM2.5 raster file to within state's bounds
  acag_crop <- crop(new_acag, new_geo_sp, snap = "out")

  # Convert spatial object with PM2.5 levels to dataframe, including spatial coordinates
  acag_df <- as.data.frame(acag_crop, xy = T)

  # Convert spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  acag_sp <- as(acag_crop, "SpatialPolygonsDataFrame")

  # Convert to an sf object
  acag_sf <- st_as_sf(acag_sp)

  # Pulls geometry from Spatial object converted to sf
  new_geo_sf <- st_as_sf(new_geo_sp)$geometry

  # Combines objects together to have PM2.5 levels with geometries
  census_acag_int <- st_intersection(acag_sf, new_geo_sf)

  # Adds column corresponding to area of each polygon
  census_acag_int$grid_area <- as.numeric(st_area(census_acag_int$geometry))

  # Make weight = area/total state area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
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
#' @param st, character string representing a state
#' @param acag, rasterLayer object containing PM2.5 levels for the USA
#'
#' @return Dataframe object containing GEOID, state, and PM2.5 levels for a
#' given state
#'
#' @keywords internal
#' @noRd
get_national_geo <- function(st, acag){

  # Load shapefile for given state
  geo <- tigris::states(cb = T) %>%
    filter(STUSPS %in% st)

  # CRS needs to line up
  new_acag <- projectRaster(acag, crs = crs(geo))
  new_acag@data@names <- "Value"

  # Initialize particulate matter column
  new_geo <- geo %>%
    mutate(Particulate.Matter = NA_real_)

  # Convert df to a list to be able to parallelize in mclapply
  new_geo.list <-
    setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))

  # Perform parallel computation
  new_geo.list_PM <- mclapply(new_geo.list, get_pm_data, new_acag = new_acag)
  pm_data <- new_geo.list_PM %>% bind_rows() # Back to df

  # Pull desired columns
  pm_data <- as.data.frame(pm_data) %>%
    dplyr::select(GEOID, NAME, Particulate.Matter)

  return(pm_data)
}


## EXTERNAL

#' State level particulate matter data
#'
#' Pulls PM2.5 data at a state level, either internally or externally. Years
#' 2015 through 2018 are pre-available within the package, but years 2014 and
#' earlier are available as an external pull.
#'
#' @param year, numeric object representing a selected year
#'
#' @return Dataframe object broken down by state with mean area-weighted PM2.5
#' values. If input year is unavailable, returns an error.
#'
#' @examples
#' acag_pm_dat_state <- pull_state_ACAG(year = 2016)
#' @export
pull_state_ACAG <- function(year){

  # Pre-available years
  available_years <- c(2015, 2016, 2017, 2018)

  # Does not include Alaska or Hawaii!
  states <- c(setdiff(state.abb, c("AK", "HI")), "DC")

  file_path <- file.path("..", "data", "output", year, "National", "acag_pm_dat_US.csv")

  # Returns csv corresponding to the available year as a dataframe object
  if (year %in% available_years){
    return(read.csv(file_path))
  }

  # Returns an object pulled from selected year of data as a dataframe
  else{
    # Pull PM2.5 data for the given year
    acag <- raster(
      file.path(
        "data",
        "input",
        "acag_raw_data_files",
        paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
      ))
    acag@data@names <- "Value"

    ## Keeping these in case we want to define new_acag once
    # crs_args <- "+proj=longlat +datum=NAD83 +no_defs"
    # new_acag <- raster(
    #   file.path(
    #     "data",
    #     "input",
    #     "acag_raw_data_files",
    #     paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
    #   ),
    #   crs = crs_args)
    # new_acag@data@names <- "Value"

    # Perform a pull, state by state
    acag_pm_dat_US <- get_national_geo(st = states, acag = acag)

    return(acag_pm_dat_US)
  }

}
