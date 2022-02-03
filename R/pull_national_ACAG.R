## INTERNAL

#' Function to perform mean area weighting within state to summarise PM2.5 
#' concentrations from gridded data
#' @param state df row of state features
#' @keywords internal
#' @noRd
get_pm_data <- function(state, new_dal){
  print(paste0("Calculating PM2.5 for ", state$NAME, ", ", Sys.time(), "..."))
  tic(state$NAME)
  
  # Converts shapefile to spatial object
  new_geo_sp <- as(state, "Spatial")
  
  # Crops nationwide PM2.5 raster file to within state's bounds
  dalhousie_crop <- crop(new_dal, new_geo_sp, snap = "out")
  
  # Convert spatial object with PM2.5 levels to dataframe, including spatial coordinates
  dalhousie_df <- as.data.frame(dalhousie_crop, xy = T)
  
  # Convert spatial object with PM2.5 levels to SpatialPolygonsDataFrame
  dalhousie_sp <- as(dalhousie_crop, "SpatialPolygonsDataFrame")
  
  # Convert to an sf object
  dalhousie_sf <- st_as_sf(dalhousie_sp)
  
  # Pulls geometry from Spatial object as sf
  new_geo_sf <- st_as_sf(new_geo_sp)$geometry
  
  # Combines objects together to have PM2.5 readings combined with geometries
  census_dalhs_int <- st_intersection(dalhousie_sf, new_geo_sf)
  
  # Adds column corresponding to area of each polygon
  census_dalhs_int$grid_area <- as.numeric(st_area(census_dalhs_int$geometry))
  
  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <- census_dalhs_int$grid_area/ct_area
  
  # State value is weighted average of PM2.5 levels
  this_ct_val <- sum(census_dalhs_int$Value * census_dalhs_int$weight)/sum(census_dalhs_int$weight)
  
  state$Particulate.Matter <- this_ct_val
  gc()
  toc()
  return(state)
}


#' Helper function to compute PM2.5 concentrations for every census tract for each
#' state
#'
#' @keywords internal
#' @noRd
get_national_geo <- function(st, dal){
  
  # Load shapefile for given state
  geo <- tigris::states(cb = T) %>%
    filter(STUSPS %in% st)
  
  # CRS needs to line up
  new_dal <- projectRaster(dal, crs = crs(geo))
  new_dal@data@names <- "Value"
  
  # Initialize particulate matter column
  new_geo <- geo %>%
    mutate(Particulate.Matter = NA_real_)
  
  # Convert df to a list to be able to parallelize in mclapply
  new_geo.list <-
    setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))
  
  # Perform parallel computation
  new_geo.list_PM <- mclapply(new_geo.list, get_pm_data, new_dal = new_dal)
  pm_data <- new_geo.list_PM %>% bind_rows() # Back to df
  
  # Pull desired columns
  pm_data <- as.data.frame(pm_data) %>%
    dplyr::select(GEOID, NAME, Particulate.Matter)
  
  return(pm_data)
}


## EXTERNAL

#' Pulls PM data at national level, either internally or externally
#'
#'
#' @param year
#'
#' @return dataframe with requested data
#'
#' @examples 
#' pull_national_ACAG(2016)
pull_national_ACAG <- function(year){
  
  # Pre-available years
  available_years <- c(2015, 2016, 2017, 2018)
  
  # Does not include Alaska or Hawaii!
  states <- c(setdiff(state.abb, c("AK", "HI")), "DC")
  
  file_path <- file.path("data", "output", year, "National", "Dalhousie_pm_dat_US.csv")
  
  # Returns csv corresponding to the available year as a dataframe object
  if (year %in% available_years){
    return(read.csv(file_path))
  }
  
  # Returns an object pulled from selected year of data as a dataframe
  else{
    # Pull PM2.5 data for the given year
    dalhousie <- raster(
      file.path(
        "data",
        "input",
        "acag_raw_data_files",
        paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
      ))
    dalhousie@data@names <- "Value"
    
    # Perform a pull, state by state
    Dalhousie_pm_dat_US <- get_national_geo(st = states, dal = dalhousie)
    
    return(Dalhousie_pm_dat_US)
  }
  
}
