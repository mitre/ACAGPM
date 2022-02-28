## INTERNAL


#' #Come back to this one
#' #' Function to save final processed data as CSV and Rdata files to output
#' #' directory
#' #'
#' #' @param df data frame of processed data
#' #' @param county_name vector of county names (as strings) in state
#' #' @param state string - state abbreviation
#' #'
#' #' @keywords internal
#' #' @noRd
#' save_data <- function(df, state){
#'
#'   # Initialize empty list
#'   final_df.list <- c()
#'
#'   for (name_index in 1:length(df)){
#'
#'     # Pull county name for index
#'     county_name <- names(df)[name_index]
#'
#'     # Pull df with specific county
#'     pm_data <- df[[county_name]]
#'
#'     # Correct format
#'     pm_data <- as.data.frame(pm_data)
#'
#'     # Select desired columns
#'     pm_data <-
#'       if (state != "DC") {
#'         pm_data %>%
#'           dplyr::select(GEOID, NAMELSAD, Particulate.Matter)
#'       } else {
#'         pm_data %>%
#'           dplyr::select(GEOID, NAME, Particulate.Matter)
#'       }
#'
#'     # Adds column with county and state
#'     pm_data <- pm_data %>%
#'       add_column(county_state = rep(paste0(county_name, "_", state), nrow(pm_data)), .before = "GEOID")
#'
#'     # Add data as new row to list of dataframes
#'     final_df.list <- append(final_df.list, list(pm_data))
#'   }
#'
#'   # Compile into one dataframe for state
#'   final_df <- bind_rows(final_df.list)
#'
#'   return(final_df)
#' }

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' census tract
#'
#' @param census, dataframe of features for a given census tract
#' @param new_dal, rasterLayer object with PM2.5 levels
#'
#' @return Modified census dataframe with mean area-weighted PM2.5 levels
#'
#' @keywords internal
#' @noRd
get_census_pm <- function(census, new_dal) {

  # Convert shapefile to spatial object
  new_geo_sp <- as(census, "Spatial")

  # Crop dalhousie data to match census tract boundary
  dalhousie_crop <-
    crop(new_dal, new_geo_sp, snap = "out")

  # Converts spatial object with dalhousie data to dataframe, including spatial coordinates
  dalhousie_df <- data.frame(rasterToPoints(dalhousie_crop))

  # Convert spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  dalhousie_sp <-
    as(dalhousie_crop, "SpatialPolygonsDataFrame")

  # Convert to an sf object
  dalhousie_sf <- st_as_sf(dalhousie_sp)

  # Pulls geometry from spatial object converted to sf
  new_geo_sf <- st_as_sf(new_geo_sp)$geometry

  # Combines objects together to have PM2.5 levels with geometries
  census_dalhs_int <-
    st_intersection(dalhousie_sf, new_geo_sf)

  # Area of each polygon
  census_dalhs_int$grid_area <-
    as.numeric(st_area(census_dalhs_int$geometry))


  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <-
    census_dalhs_int$grid_area / ct_area

  # Tract value is weighted average of PM2.5 levels
  this_ct_val <-
    sum(census_dalhs_int$Value * census_dalhs_int$weight) / sum(census_dalhs_int$weight)

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
    mutate(Particulate.Matter = NA_real_) %>%
    filter(AWATER / as.numeric(st_area(geometry)) < .9) # Remove tracts that are over 90

  return(new_geo)
}


#' Function to compute census tract PM2.5 levels for each county
#'
#' @param st, character string representing a state
#' @param dal, rasterLayer object containing PM2.5 levels for the USA
#'
#' @return Dataframe object containing GEOID, census tract, and PM2.5 levels for
#' census tracts in a given county
#'
#' @keywords internal
#' @noRd
get_county_geo <- function(st, cty_df, dal) {

  # Get Counties in state, filter to cty_df only in state
  cty_df.st <- cty_df %>%
    filter(state == st)

  # Load shapefile for counties in given state
  print(paste0("Fetching county data for ", st, "..."))
  geo <-
    if (st != "DC") {
      tigris::counties(st) %>%
        mutate(INTPTLAT = as.numeric(INTPTLAT),
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
  print("Fetching census shape data for counties...")
  new_geo <- lapply(cty_row.list, get_census_geo, st = st)

  # CRS needs to line up
  new_dal <- projectRaster(dal, crs = crs(new_geo[[1]]))
  new_dal@data@names <- "Value"

  # Assign rownames
  new_geo.list <- lapply(new_geo, function(df){
    setNames(split(df, seq(nrow(df))), df$GEOID)
  })

  # Compute mean area concentration for CTs
  print("Computing mean area weighted PM2.5...")
  final_geo <- lapply(new_geo.list, function(df){
    lapply(df, get_census_pm, new_dal)
  })

  # Combine into single df
  final_geo.df <- lapply(final_geo, bind_rows)

  # Save
  paste0(paste0("Saving ", st, "..."))
  return(save_data(df = final_geo.df, state = st))
}

## EXTERNAL

#' County level particulate matter data
#'
#' Pulls PM2.5 data at a county level, either internally or externally. Years
#' 2015 through 2018 are pre-available within the package, but year ____ through
#' 2014 are available as an external pull.
#'
#' @param year, numeric object representing a selected year
#' @param county_state, character vector of selected counties
#'
#' @return Dataframe object broken down by census tracts in each county with
#' mean area-weighted PM2.5 values. If the county_state field is empty, PM2.5
#' data for census tracts in all counties is returned. If input year is
#' unavailable, returns an error.
#'
#' @examples
#' pull_county_ACAG(year = 2016, state = c("Abbeville County_SC", "Acadia Parish_LA", "Accomack County_VA"))
#' pull_county_ACAG(year = 2016)
#' @export
pull_county_ACAG <- function(year, county_state = c()){

  # Pre-available years of data
  available_years <- c(2015, 2016, 2017, 2018)

  # Dataframe of info related to US cities
  us_cities <- read.csv(file.path("..", "data", "input", "US_cities.csv"))

  # Does not include Alaska or Hawaii
  states <- c(setdiff(state.abb, c("AK", "HI")), "DC")

  # Dataframe of all available counties a user can choose; total count 3108
  every_county <- read.csv(file.path("..", "data", "input", "all_counties.csv"), encoding = "UTF-8") %>%
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

  # Returns csv corresponding to the available year as a dataframe
  if (year %in% available_years){
    # Returns csvs correspondiing to given year in selected counties as a combined dataframe object
    if (all(county_state %in% every_county$county_state)){

      # Pulls selected data as list of dataframes
      dflist <- lapply(county_state, function(x){
        # Pull csv for county
        tempdf <- read.csv(file.path("..", "data", "output", year, "County", paste0("Dalhousie_pm_dat_", x, ".csv")))

        # Add column with name of county and state
        tempdf <- tempdf %>%
          tibble::add_column(county_state = rep(x, nrow(tempdf)), .before = "GEOID")

        return(tempdf)
      })

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_County <- dplyr::bind_rows(dflist)

      return(ACAG_pm_dat_County)
    }
    # If an unrecognized county is entered, throw error
    else{
      stop("Improper input, unrecognized county")
    }
  }

  # Returns an object pulled from selected year of data as a dataframe
  else{
    # Pull PM2.5 data for the given year in selected counties
    if (all(county_state %in% every_county$county_state)){
      # Pull PM2.5 for the given year
      dalhousie <- raster(
        file.path(
          "data",
          "input",
          "acag_raw_data_files",
          paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
        ))
      dalhousie@data@names <- "Value"

      ## Keeping these in case we want to define new_dalhousie once
      # crs_args <- "+proj=longlat +datum=NAD83 +no_defs"
      # new_dalhousie <- raster(
      #   file.path(
      #     "data",
      #     "input",
      #     "acag_raw_data_files",
      #     paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
      #   ),
      #   crs = crs_args)
      # new_dalhousie@data@names <- "Value"

      #Perform a pull state by state
      dflist <- lapply(unique(state), get_county_geo, county_df, dalhousie)

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_County <- bind_rows(dflist)

      return(ACAG_pm_dat_County)
    }
    # If an unrecognized county is entered, throw error
    else{
      stop("Improper input, unrecognized county")
    }
  }
}
