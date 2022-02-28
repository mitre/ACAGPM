## INTERNAL

#' Function to get the census data shape based on a given level. Level gives the
#' information 1 level down.
#'
#' @param level, character string representing chosen level
#' @param st, character string representing a state
#' @param county, character string representing a county
#' @param city, character string representing a city
#'
#' @return Shapefile for a given city at the census tract level
#'
#' @keywords internal
#' @noRd
get_census_shape <- function(level, st = NULL, county = NULL, city = NULL){


  # if the level is city, we need more processing
  if (level == "city"){
    if (!city %in% c("Augusta", "Greater Kansas City")){
      init_ct <- get_decennial(
        geography = "tract",  state = st, county = get_city_county(city, st),
        year = 2010, geometry = T, variables = c("P001001", "H001001"),
        output = "wide"
      )
    } else {
      if (city == "Greater Kansas City"){
        # kansas city is in two states
        init_ct1 <- get_decennial(
          geography = "tract",  state = "MO",
          county = get_city_county(city, "MO"),
          year = 2010, geometry = T, variables = c("P001001", "H001001"),
          output = "wide"
        )
        init_ct2 <- get_decennial(
          geography = "tract",  state = "KS",
          county = get_city_county(city, "KS"),
          year = 2010, geometry = T, variables = c("P001001", "H001001"),
          output = "wide"
        )
      } else { # augusta is also in two states
        init_ct1 <- get_decennial(
          geography = "tract",  state = "GA",
          county = get_city_county(city, "GA"),
          year = 2010, geometry = T, variables = c("P001001", "H001001"),
          output = "wide"
        )
        init_ct2 <- get_decennial(
          geography = "tract",  state = "SC",
          county = get_city_county(city, "SC"),
          year = 2010, geometry = T, variables = c("P001001", "H001001"),
          output = "wide"
        )

      }
      init_ct <- rbind(init_ct1, init_ct2)
    }

    # remove empty geometries since spatial types can't handle them
    ct <- as(
      init_ct[!st_is_empty(init_ct),],
      "Spatial"
    )

  }

  geo_shp <-
    if (level == "city"){
      ct
    } else if (level == "county"){

      init_ct <- check_outdated_counties_county(current_county = county,
                                                current_st = st,
                                                chosen_variables = "P001001")

      # remove empty geometries since spatial types can't handle them
      as(
        init_ct[!st_is_empty(init_ct),],
        "Spatial"
      )
    } else if (level == "state"){
      init_ct <- get_decennial(
        geography = "county",  state = st,
        year = 2010, geometry = T, variables = "P001001"
      )

      init_ct <- check_outdated_counties_state(data = init_ct, current_st = st)



      # remove empty geometries since spatial types can't handle them
      as(
        init_ct[!st_is_empty(init_ct),],
        "Spatial"
      )

    } else { # us == all states
      init_ct <- get_decennial(
        geography = "state",
        year = 2010, geometry = T, variables = "P001001"
      )

      # remove empty geometries since spatial types can't handle them
      as(
        init_ct[!st_is_empty(init_ct),],
        "Spatial"
      )
    }

  return(geo_shp)
}


#' Function to get the county for a given city and state; needs to be a number
#'
#' @param city, character string representing a city
#' @param st, character string representing a state
#'
#' @return Character vector of counties a city resides in
#'
#' @keywords internal
#' @noRd
get_city_county <- function(city, st){
  us_cities <- read.csv(file.path("data", "input", "US_cities.csv"))
  specific_us_cities <- read.csv(file.path("data", "input", "Specific_US_cities.csv"))

  if (city %in% specific_us_cities$place.name){
    ct_codes <- unique(
      specific_us_cities$county.code[
        specific_us_cities$state.code == st &
          specific_us_cities$place.name == city
      ]
    )
  } else {
    ct_codes <- unique(
      us_cities$county.code[
        us_cities$state.code == st & us_cities$place.name == city
      ]
    )

  }

  return(ct_codes)
}


#' #' Function to save final processed data as CSV and Rdata files to output
#' #' directory
#' #' @param df data frame of processed data
#' #' @param county_name vector of county names (as strings) in state
#' #' @param state string - state abbreviation
#' #' @keywords internal
#' #' @noRd
#' save_data <- function(df, state){
#'   for (name_index in 1:length(df)){
#'     this_city <- names(df)[name_index]
#'     pm_data <- df[[this_city]]
#'     save(pm_data, file = file.path(save_dir,
#'                                    "Dalhousie_Particulate_Matter",
#'                                    year,
#'                                    "City",
#'                                    paste0("Dalhousie_pm_dat_", this_city, "_", state, ".RData")))
#'
#'     pm_data <- as.data.frame(pm_data) %>%
#'       dplyr::select(GEOID, NAME, Particulate.Matter)
#'     write.csv(pm_data, file.path(save_dir,
#'                                  "Dalhousie_Particulate_Matter",
#'                                  year,
#'                                  "City",
#'                                  paste0("Dalhousie_pm_dat_", this_city, "_", state, ".csv")),
#'               row.names = F)
#'   }
#'
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

  # convert shapefile to spatial object
  dalhousie_crop <- crop(new_dal, census, snap = "out")

  # Crop dalhousie data to match census tract boundary
  dalhousie_df <- as.data.frame(dalhousie_crop, xy = T)

  # Replace tracts that are all NA with 0.0
  if (all(is.na(dalhousie_df$Value))){
    next
    # dalhousie_crop@data@values <- 0.0
  }

  # Convert spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  dalhousie_sp <- as(dalhousie_crop, "SpatialPolygonsDataFrame")

  # Convert to an sf object
  dalhousie_sf <- st_as_sf(dalhousie_sp)

  # Pulls geometry from spatial object converted to sf
  new_geo_sf <- st_as_sf(census)$geometry

  # Combines objects together to have PM2.5 levels with geometries
  census_dalhs_int <- st_intersection(dalhousie_sf, new_geo_sf)

  # Area of each polygon
  census_dalhs_int$grid_area <- as.numeric(st_area(census_dalhs_int$geometry))

  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <- census_dalhs_int$grid_area/ct_area

  # Tract value is weighted average of PM2.5 levels
  this_ct_val <- sum(census_dalhs_int$Value * census_dalhs_int$weight)/sum(census_dalhs_int$weight)

  census@data$Particulate.Matter <- this_ct_val

  gc()

  return(census)
}


#' Helper function to pull city shapefiles
#'
#' @param this_city, character string representing a city
#' @param st, character string representing a state
#'
#' @return Shapefile object containing a city's geometry and other data
#'
#' @keywords internal
#' @noRd
get_census_geo <- function(this_city, st) {
  # Need the city boundaries
  new_geo <- get_census_shape("city", st = st, city = this_city)

  #return(city_shp)
  return(new_geo)
}


#' Function to compute census tract PM2.5 levels for each city
#'
#' @param city_name, character string representing a city
#' @param city_df, dataframe containing city metadata
#' @param dal, rasterLayer object containing PM2.5 levels for the USA
#'
#' @return Dataframe object containing GEOID, census tract, and PM2.5 levels for
#' census tracts in a given city
#'
#' @keywords internal
#' @noRd
#get_city_geo <- function(st) {
get_city_geo <- function(city_name, city_df, dal) {

  # Get state in which city is located
  st <- city_df %>%
    filter(city_state == city_name) %>%
    pull(state)

  # Pull city name
  these_cities <- city_df %>%
    filter(city_state == city_name) %>%
    pull(city)

  # Load shapefile for selected city
  print("Fetching census shape data for counties...")
  new_geo <- lapply(these_cities, function(this_city){
    get_census_geo(this_city, st = st)
  })

  #Make sure that CRS projections match
  new_dal <- projectRaster(dal, crs = crs(new_geo[[1]]))
  new_dal@data@names <- "Value"

  # Assign rownames
  new_geo.list <- lapply(new_geo, function(df){
    setNames(split(df, seq(nrow(df))), df$GEOID)
  })

  # Set name of rows to cities
  names(new_geo.list) = these_cities

  # Compute mean area concentration for CTs
  print("Computing mean area weighted PM2.5...")
  final_geo <- lapply(new_geo.list, function(df){
    mclapply(df, get_census_pm, new_dal = new_dal, mc.cores = 1)
  })

  # Combine into single df
  final_geo.sf <- lapply(final_geo, function(df){
    lapply(df, st_as_sf)
  })

  # Combine into single df
  final_geo.df <- lapply(final_geo.sf, bind_rows)

  # Pull city name
  this_city <- names(final_geo.df)[1]

  # Pull data corresponding only to city
  pm_data <- final_geo.df[[this_city]]

  # Pull desired columns
  pm_data <- as.data.frame(pm_data) %>%
    dplyr::select(GEOID, NAME, Particulate.Matter) %>%
    add_column(city_state = rep(city_name, nrow(pm_data)), .before = "GEOID")

  return(pm_data)

  # # Save
  # paste0(paste0("Saving ", st, "..."))
  # save_data(df = final_geo.df, state = st)
}

## EXTERNAL

#' City level particulate matter data
#'
#' Pulls PM2.5 data at a city level, either internally or externally. Years
#' 2015 through 2018 are pre-available within the package, but year ____ through
#' 2014 are available as an external pull.
#'
#' @param year, numeric object representing a selected year
#' @param city_state, character vector of selected cities
#'
#' @return Dataframe object broken down by census tracts in each city with
#' mean area-weighted PM2.5 values. If the city_state field is empty, PM2.5
#' data for census tracts in all cities is returned. If input year is
#' unavailable, returns an error.
#'
#' @examples
#' pull_city_ACAG(year = 2016, state = c("Akron_OH", "Albany_NY", "Altoona_PA"))
#' pull_city_ACAG(year = 2016)
#' @export
pull_city_ACAG <- function(year, city_state = c()){

  # Pre-available years of data
  available_years <- c(2015, 2016, 2017, 2018)

  # Dataframe of available cities
  cities <- read.csv(file.path("data", "input", "all_cities.csv")) %>%
    mutate(city_state = paste0(city, "_", state))

  # If no cities are entered, pull PM2.5 for all cities
  if (length(city_state) == 0){
    city_state = cities$city_state
  }

  # Creates vector containing states chosen
  state <- sapply(city_state, function(x){
    unlist(strsplit(x, split = "_"))[2]
  })

  # Creates vector containing cities chosen
  city <- sapply(city_state, function(x){
    unlist(strsplit(x, split = "_"))[1]
  })

  # Dataframe of user input and additional information
  city_df <- cbind.data.frame(city, state, city_state)

  # Initialized dataframe; do we need this?
  ACAG_pm_dat_City <- data.frame(city_state = character(),
                                 GEOID = numeric(),
                                 NAME = character(),
                                 Particulate.Matter = numeric())

  # Returns csv corresponding to the available year as a dataframe
  if (year %in% available_years){
    # Returns csvs corresponding to given year in selected cities as a combined dataframe object
    if (all(city_state %in% cities$city_state)){

      # Pulls selected data as list of dataframes
      dflist <- lapply(city_state, function(x){
        # Pull csv for city
        tempdf <- read.csv(file.path("data", "output", year, "City", paste0("Dalhousie_pm_dat_", x, ".csv")))

        # Add column with name of city and state
        tempdf <- tempdf %>%
          add_column(city_state = rep(x, nrow(tempdf)), .before = "GEOID")

        return(tempdf)
      })

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_City <- bind_rows(dflist)

      return(ACAG_pm_dat_City)
    }
    # If an unrecognized city is entered, throw error
    else{
      stop("Improper input, unrecognized city")
    }
  }

  # Returns an object pulled from selected year of data as a dataframe
  else{
    # Pull PM2.5 data fro the given year in selected cities
    if (all(city_state %in% cities$city_state)){
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

      #Perform a pull city by city
      dflist <- lapply(city_state, get_city_geo, city_df, dalhousie)

      # Convert list of dataframes to dataframe
      ACAG_pm_dat_City <- bind_rows(dflist)

      return(ACAG_pm_dat_City)
    }
    # If an unrecognized city is entered, throw error
    else{
      stop("Improper input, unrecognized city")
    }
  }

  #return(Dalhousie_pm_dat_US)
}
