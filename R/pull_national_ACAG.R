## INTERNAL

#' Function to perform mean area weighting within state to summarise PM2.5 
#' concentrations from gridded data
#' @param state df row of state features
#' @keywords internal
#' @noRd
get_pm_data <- function(state){
  print(paste0("Calculating PM2.5 for ", state$NAME, ", ", Sys.time(), "..."))
  tic(state$NAME)
  new_geo_sp <- as(state, "Spatial")
  dalhousie_crop <- crop(new_dalhousie, new_geo_sp, snap = "out")
  dalhousie_df <- as.data.frame(dalhousie_crop, xy = T)
  # Plot in base dalhousie
  # plot(dalhousie_crop)
  # plot(new_geo_sp, add = T)
  
  dalhousie_sp <- as(dalhousie_crop, "SpatialPolygonsDataFrame")
  dalhousie_sf <- st_as_sf(dalhousie_sp)
  new_geo_sf <- st_as_sf(new_geo_sp)$geometry
  census_dalhs_int <- st_intersection(dalhousie_sf, new_geo_sf)
  # plot(census_dalhs_int)
  
  # Area of each polygon
  census_dalhs_int$grid_area <- as.numeric(st_area(census_dalhs_int$geometry))
  
  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <- census_dalhs_int$grid_area/ct_area
  
  this_ct_val <- sum(census_dalhs_int$Value * census_dalhs_int$weight)/sum(census_dalhs_int$weight)
  
  state$Particulate.Matter <- this_ct_val
  gc()
  toc()
  return(state)
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
  
  available_years <- c(2015, 2016, 2017, 2018)
  
  file_path <- file.path("data", "output", year, "National", "Dalhousie_pm_dat_US.csv")

  if (year %in% available_years){
    return(read.csv(file_path))
  }
  
  else{
    dalhousie <- raster(
      file.path(
        "data",
        "input",
        "acag_raw_data_files",
        paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
      ))
    dalhousie@data@names <- "Value"
    
    # Does not include Alaska or Hawaii!
    states <- c(setdiff(state.abb, c("AK", "HI")), "DC")
    
    Dalhousie_pm_dat_US <- "temp_df"
    
    geo <- tigris::states(cb = T) %>%
      # mutate(INTPTLAT = as.numeric(INTPTLAT),
      #        INTPTLON = as.numeric(INTPTLON)) %>%
      filter(STUSPS %in% states)
    
    # CRS needs to line up
    new_dalhousie <- projectRaster(dalhousie, crs = crs(geo))
    new_dalhousie@data@names <- "Value"
    
    # Initialize particulate matter column
    new_geo <- geo %>%
      mutate(Particulate.Matter = NA_real_)
    
    # Convert df to a list to be able to parallelize in mclapply
    new_geo.list <-
      setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))
    
    # Perform parallel computation
    new_geo.list_PM <- mclapply(new_geo.list, get_pm_data)
    pm_data <- new_geo.list_PM %>% bind_rows() # Back to df
    
    pm_data <- as.data.frame(pm_data) %>%
      dplyr::select(GEOID, NAME, Particulate.Matter)
    
    #return(pm_data)
    return(Dalhousie_pm_dat_US)
  }
  
  #return(Dalhousie_pm_dat_US)
}


