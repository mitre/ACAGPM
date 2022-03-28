## INTERNAL

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' county
#' @param census, dataframe of features for a given county
#' @param new_acag, rasterLayer object with PM2.5 levels
#'
#' @return Modified census dataframe with mean area-weighted PM2.5 levels
#'
#' @keywords internal
#' @noRd
get_county_pm <- function(census, new_acag){

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

  ## TODO: use sp intersect function to get areas within tracts and weight by area

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
#' @param st, character string representing a state
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
      dplyr::filter(state == st) %>%
      dplyr::pull(county_state)
  }

  # Load shapefile for counties in given state
  new_geo <-
    if (st != "DC") {
      tigris::counties(st) %>%
        dplyr::mutate(INTPTLAT = as.numeric(INTPTLAT),
               INTPTLON = as.numeric(INTPTLON))
    } else {
      tigris::counties(st, cb = T)
    }

  if (!is.null(county_df)){
    new_geo <- new_geo %>%
      dplyr::filter(GEOID %in% county_names)
  }

  # CRS needs to line up
  new_acag <- raster::projectRaster(acag, crs = crs(new_geo))
  new_acag@data@names <- "Value"

  # Convert df to a list to be able to parallelize in mclapply
  new_geo.list <-
    setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))

  # Perform parallel computation, which pulls PM2.5 levels for each county in the state
  new_geo.list_PM <- parallel::mclapply(new_geo.list, get_county_pm, new_acag = new_acag, mc.cores = 1)
  pm_data <- do.call("rbind", new_geo.list_PM) # Back to df

  # Pull desired columns
  pm_data <- as.data.frame(pm_data)
  pm_data <-
    if (st != "DC") {
      pm_data %>%
        dplyr::select(STATEFP, GEOID, NAMELSAD, Particulate.Matter)
    } else {
      pm_data %>%
        dplyr::select(STATEFP, GEOID, NAME, Particulate.Matter)
    }

  return(pm_data)
}

## EXTERNAL

#' County level particulate matter data
#'
#' Pulls PM2.5 data at a county granularity with level selections, either
#' internally or as a new pull. Years 2015 through 2018 are pre-available within
#' the package, but years 2014 and earlier are available as a pull combining
#' PM2.5 raster files and tigris shape files.
#'
#' @param year, numeric object representing a selected year
#' @param level, character string representing desired level of pull
#' @param state, numeric vector of selected states via GEOID
#' @param county_state, numeric vector of selected counties via GEOID
#'
#' @return Dataframe object broken down by counties with mean
#' area-weighted PM2.5 values.
#'
#' @examples
#' acag_pm_dat_county <- pull_county_ACAG(year = 2016, level = "National")
#' acag_pm_dat_county <- pull_county_ACAG(year = 2016, level = "State", state = c(1, 5, 4))
#' acag_pm_dat_county <- pull_county_ACAG(year = 2016, level = "County", county_state = c(45001, 22001, 51001))
#' @export
pull_county_ACAG <- function(year, level, state = c(), county_state = c()){

  # Pre-available years of data
  available_years <- c(2015, 2016, 2017, 2018)

  county_lookup <- read.csv(system.file(file.path("data", "input", "county_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8")

  county_df = NULL

  if (level == "National"){
    st <- unique(county_lookup$STATEFP)
    st.STUSPS <- unique(county_lookup %>% dplyr::filter(STATEFP %in% st) %>% dplyr::pull(STUSPS))
  } else if (level == "State"){
    if (all(state %in% unique(county_lookup$STATEFP))){
      st <- state
      st.STUSPS <- unique(county_lookup %>% dplyr::filter(STATEFP %in% st) %>% dplyr::pull(STUSPS))
    } else{
      stop("Improper input, unrecognized state")
    }
  } else if (level == "County"){
    if (all(county_state %in% county_lookup$GEOID.COUNTY)){

      # Creates vector containing states chosen
      state <- county_lookup %>%
        dplyr::filter(GEOID.COUNTY %in% county_state) %>%
        dplyr::pull(GEOID.STATE)

      # Dataframe of user input and additional information
      county_df <- cbind.data.frame(state, county_state)

      st <- unique(state)
      st.STUSPS <- unique(county_lookup %>% dplyr::filter(GEOID.STATE %in% st) %>% dplyr::pull(STUSPS))

    } else{
      stop("Improper input, unrecognized county")
    }
  } else{
    stop("Improper input, unrecognized level")
  }

  # Initialized dataframe; do we need this?
  ACAG_pm_dat_County <- data.frame(county_state = character(),
                                   GEOID = numeric(),
                                   NAME = character(),
                                   Particulate.Matter = numeric())

  # If year 2015-2018 selected, returns csv corresponding to year as a dataframe.
  # Else, returns an object pulled from selected year of data as a dataframe.
  # Returns an error if unrecognized state is selected as input.
  if (year %in% available_years){

      # Pulls selected data as list of dataframes
      dflist <- lapply(st.STUSPS, function(x){
        # Pull csv for state
        tempdf <- read.csv(system.file(file.path("data", "output", year, "county", paste0("acag_pm_dat_", x, ".csv")),
                                       package = "ACAGPM"))

        if (level == "County"){
          tempdf <- tempdf %>%
            dplyr::filter(GEOID %in% county_state)
        }

        # Add column with name of state
        tempdf <- tempdf %>%
          tibble::add_column(state = rep(x, nrow(tempdf)), .before = "GEOID")

        return(tempdf)
      })

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_State <- dplyr::bind_rows(dflist)

      return(ACAG_pm_dat_State)

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

      # Perform a pull state by state
      dflist <- lapply(st, get_state_geo, acag, county_df)

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_State <- dplyr::bind_rows(dflist)

      return(ACAG_pm_dat_State)

  }
}
