## INTERNAL


#Come back to this one
#' Function to save final processed data as CSV and Rdata files to output
#' directory
#'
#' @param df data frame of processed data
#' @param county_name vector of county names (as strings) in state
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
  final_df <- bind_rows(final_df.list)

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
get_census_geo <- function(cty_row, st) {

  # Pull county name
  cty <- cty_row$COUNTYFP

  # Get specific county shape info
  county_geo <- tigris::tracts(state = st, county = cty) %>%
    mutate(INTPTLAT = as.numeric(INTPTLAT),
           INTPTLON = as.numeric(INTPTLON))

  # Choose name column
  name_col <- if (st != "DC") {
    "NAMELSAD"
  } else {
    "NAME"
  }

  # Initialize particulate matter column
  new_geo <- county_geo %>%
    dplyr::mutate(Particulate.Matter = NA_real_) %>%
    dplyr::filter(AWATER / as.numeric(st_area(geometry)) < .9) # Remove tracts that are over 90

  return(new_geo)
}


#' Function to compute census tract PM2.5 levels for each county
#'
#' @param st, character string representing a state
#' @param acag, rasterLayer object containing PM2.5 levels for the USA
#'
#' @return Dataframe object containing GEOID, census tract, and PM2.5 levels for
#' census tracts in a given county
#'
#' @keywords internal
#' @noRd
get_county_geo <- function(st, cty_df, acag) {

  # Get Counties in state, filter to cty_df only in state
  cty_df.st <- cty_df %>%
    dplyr::filter(state == st)

  # Load shapefile for counties in given state
  geo <-
    if (st != "DC") {
      tigris::counties(st) %>%
        dplyr::mutate(INTPTLAT = as.numeric(INTPTLAT),
               INTPTLON = as.numeric(INTPTLON))
    } else {
      tigris::counties(st, cb = T)
    }

  # Selecting name column
  name_col <- if (st != "DC") {
    "NAMELSAD"
  } else {
    "NAME"
  }

  # Convert to a list to be able to use with mclapply
  cty_row.list <-
    setNames(split(geo, seq(nrow(geo))), geo[[name_col]])

  # Filter to selected counties
  cty_row.list <-
    cty_row.list[unlist(lapply(names(cty_row.list), function(x){
      x %in% cty_df.st$county}))]

  # Get census tract shape info
  new_geo <- lapply(cty_row.list, get_census_geo, st = st)

  # CRS needs to line up
  new_acag <- raster::projectRaster(acag, crs = raster::crs(new_geo[[1]]))
  new_acag@data@names <- "Value"

  # Assign rownames
  new_geo.list <- lapply(new_geo, function(df){
    setNames(split(df, seq(nrow(df))), df$GEOID)
  })

  # Compute mean area concentration for CTs
  final_geo <- lapply(new_geo.list, function(df){
    lapply(df, get_census_pm, new_acag)
  })

  # Combine into single df
  final_geo.df <- lapply(final_geo, bind_rows)

  # Save
  return(save_data(df = final_geo.df, state = st))
}

## EXTERNAL

#' Tract level particulate matter data
#'
#' Pulls PM2.5 data at a census tract level, either internally or as a new pull. Years
#' 2015 through 2018 are pre-available within the package, but years 2014 and
#' earlier are available as a pull combining PM2.5 raster files and tigris
#' shape files.
#'
#' @param year, numeric object representing a selected year
#' @param county_state, character vector of selected counties
#'
#' @return Dataframe object broken down by census tracts in each selected county with
#' mean area-weighted PM2.5 values. If the county_state field is empty, PM2.5
#' data for census tracts in all counties is returned. If input year is
#' unavailable, returns an error.
#'
#' @examples
#' acag_pm_dat_tract <- pull_tract_ACAG(year = 2016, state = c("Abbeville County_SC", "Acadia Parish_LA", "Accomack County_VA"))
#' acag_pm_dat_tract <- pull_tract_ACAG(year = 2016)
#' @export
pull_tract_ACAG <- function(year, county_state = c()){

  # Pre-available years of data
  available_years <- c(2015, 2016, 2017, 2018)

  # Dataframe of info related to US cities
  us_cities <- read.csv(system.file(file.path("data", "input", "US_cities.csv"), package = "ACAGPM"))

  # Does not include Alaska or Hawaii
  states <- c(setdiff(state.abb, c("AK", "HI")), "DC")

  # Dataframe of all available counties a user can choose; total count 3108
  every_county <- read.csv(system.file(file.path("data", "input", "all_counties.csv"), package = "ACAGPM"), encoding = "UTF-8") %>%
    dplyr::mutate(county_state = paste0(county, "_", state))

  # If no counties are entered, pull PM2.5 for al states
  if (length(county_state) == 0){
    county_state <- every_county$county_state
  }

  # Creates vector containing states chosen
  state <- sapply(county_state, function(x){
    unlist(strsplit(x, split = "_"))[2]
  })

  # Creates vector containing counties chosen
  county <- sapply(county_state, function(x){
    unlist(strsplit(x, split = "_"))[1]
  })

  # Dataframe of user input and additional information
  county_df <- cbind.data.frame(county, state, county_state)

  # Initialized dataframe; do we need this?
  ACAG_pm_dat_County <- data.frame(county_state = character(),
                                  GEOID = numeric(),
                                  NAME = character(),
                                  Particulate.Matter = numeric())

  # If year 2015-2018 selected, returns csv corresponding to year as a dataframe.
  # Else, returns an object pulled from selected year of data as a dataframe.
  # Returns an error if unrecognized state is selected as input.
  if (year %in% available_years){
    if (all(county_state %in% every_county$county_state)){

      # Pulls selected data as list of dataframes
      dflist <- lapply(county_state, function(x){
        # Pull csv for county
        tempdf <- read.csv(system.file(file.path("data", "output", year, "tract", paste0("acag_pm_dat_", x, ".csv")), package = "ACAGPM"))

        # Add column with name of county and state
        tempdf <- tempdf %>%
          tibble::add_column(county_state = rep(x, nrow(tempdf)), .before = "GEOID")

        return(tempdf)
      })

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_County <- dplyr::bind_rows(dflist)

      return(ACAG_pm_dat_County)
    } else{ # If an unrecognized county is entered, throw error
      stop("Improper input, unrecognized county")
    }
  } else{
    if (all(county_state %in% every_county$county_state)){
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
      dflist <- lapply(unique(state), get_county_geo, county_df, acag)

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_County <- bind_rows(dflist)

      return(ACAG_pm_dat_County)
    } else{
      stop("Improper input, unrecognized county")
    }
  }
}
