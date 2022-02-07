## INTERNAL

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' county
#' @param census data frame - single county with geometry
#' @keywords internal
#' @noRd
get_county_pm <- function(census, new_dal){
  print(paste0("Calculating PM2.5 for ", census$NAME, ", ", Sys.time(), "..."))
  tic(census$NAME)
  
  # Convert shapefile to spatial object
  new_geo_sp <- as(census, "Spatial")
  
  # Crops nationwide PM2.5 raster file to within county's bounds
  dalhousie_crop <- crop(new_dal, new_geo_sp, snap = "out")
  
  # Converts spatial object with PM2.5 levels to dataframe, including spatial coordinates
  dalhousie_df <- as.data.frame(dalhousie_crop, xy = T)
  
  # Converts spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  dalhousie_sp <- as(dalhousie_crop, "SpatialPolygonsDataFrame")
  
  # Convert to an sf object
  dalhousie_sf <- st_as_sf(dalhousie_sp)
  
  # Pulls geometry from spatial object converted to sf
  new_geo_sf <- st_as_sf(new_geo_sp)$geometry
  
  # Combines objects together to have PM2.5 levels with geometries
  census_dalhs_int <- st_intersection(dalhousie_sf, new_geo_sf)
  
  # Adds column corresponding to area of each polygon
  census_dalhs_int$grid_area <- as.numeric(st_area(census_dalhs_int$geometry))
  
  ## TODO: use sp intersect function to get areas within tracts and weight by area
  
  # Make weight = area/total county area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <- census_dalhs_int$grid_area/ct_area
  
  # Tract value is weighted average of PM2.5 levels
  this_ct_val <- sum(census_dalhs_int$Value * census_dalhs_int$weight)/sum(census_dalhs_int$weight)
  
  census$Particulate.Matter <- this_ct_val
  
  gc()
  toc()
  
  return(census)
}

#' Function to compute county PM2.5 levels and save RData/csv files for each state
#' @param state - single state
#' @keywords internal
#' @noRd
get_state_geo <- function(st, dal){
  
  # Load shapefile for counties in given state
  new_geo <- 
    if (st != "DC") {
      tigris::counties(st) %>%
        mutate(INTPTLAT = as.numeric(INTPTLAT),
               INTPTLON = as.numeric(INTPTLON))
    } else {
      tigris::counties(st, cb = T)
    }
  
  # CRS needs to line up
  new_dal <- projectRaster(dal, crs = crs(new_geo))
  new_dal@data@names <- "Value"
  
  # Convert df to a list to be able to parallelize in mclapply
  new_geo.list <-
    setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))
  
  # Perform parallel computation, which pulls PM2.5 levels for each county in the state
  new_geo.list_PM <- mclapply(new_geo.list, get_county_pm, new_dal = new_dal, mc.cores = 1)
  pm_data <- do.call("rbind", new_geo.list_PM) # Back to df
  
  # Pull desired columns
  pm_data <- as.data.frame(pm_data)
  pm_data <- 
    if (st != "DC") {
      pm_data %>%
        dplyr::select(GEOID, NAMELSAD, Particulate.Matter)
    } else {
      pm_data %>%
        dplyr::select(GEOID, NAME, Particulate.Matter)
    }
  
  return(pm_data)
}

## EXTERNAL

#' Pulls PM data at state level, either internally or externally.
#' If state field is empty, returns PM data for all states.
#'
#' @param year
#' @param state
#'
#' @return dataframe, for PM2.5 of counties in each state
pull_state_ACAG <- function(year, state = c()){
  
  # Pre-available years of data
  available_years <- c(2015, 2016, 2017, 2018)
  
  # Does not include Alaska or Hawaii
  states <- c(setdiff(state.abb, c("AK", "HI")), "DC")
  
  # If no states are entered, pull PM2.5 for all states
  if (length(state) == 0){
    state = states
  }
  
  # Initialized dataframe; do we need this?
  ACAG_pm_dat_State <- data.frame(state = character(),
                                 GEOID = numeric(),
                                 NAME = character(),
                                 Particulate.Matter = numeric())
  
  # # Returns csv corresponding to the available year as a dataframe object
  if (year %in% available_years){
    # Returns csvs corresponding to given year in selected states as a combined dataframe object
    if (all(state %in% states)){
      
      # Pulls selected data as list of dataframes
      dflist <- lapply(state, function(x){
        # Pull csv for state
        tempdf <- read.csv(file.path("data", "output", year, "State", paste0("Dalhousie_pm_dat_", x, ".csv"))) 
        
        # Add column with name of state
        tempdf <- tempdf %>%
          add_column(state = rep(x, nrow(tempdf)), .before = "GEOID")
        
        return(tempdf)
      })
      
      # Convert list of dataframes to dataframe
      ACAG_pm_dat_State <- bind_rows(dflist)
      
      return(ACAG_pm_dat_State)
    }
    # If an unrecognized state is entered, throw error
    else{
      stop("Improper input, unrecognized state")
    }
  }
  
  # Returns an object pulled from selected year of data as a dataframe
  else{
    # Pull PM2.5 data for the given year in selected states
    if (all(state %in% states)){
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
      
      # Perform a pull state by state
      dflist <- lapply(state, get_state_geo, city_df, dalhousie)
      
      # Convert list of dataframes to dataframe
      ACAG_pm_dat_State <- bind_rows(dflist)
      
      return(ACAG_pm_dat_State)
    }
    # If an unrecognized state is entered, throw error
    else{
      stop("Improper input, unrecognized state")
    }
  }
}
