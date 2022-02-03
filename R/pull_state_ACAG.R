## INTERNAL

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' county
#' @param census data frame - single county with geometry
#' @keywords internal
#' @noRd
get_county_pm <- function(census, new_dal){
  print(paste0("Calculating PM2.5 for ", census$NAME, ", ", Sys.time(), "..."))
  tic(census$NAME)
  
  new_geo_sp <- as(census, "Spatial")
  dalhousie_crop <- crop(new_dal, new_geo_sp, snap = "out")
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
  
  
  # Check to see that area calculations make sense
  # census_dalhs_int$x <- st_coordinates(st_centroid(census_dalhs_int$geometry))[,1]
  # census_dalhs_int$y <- st_coordinates(st_centroid(census_dalhs_int$geometry))[,2]
  # ggplot(census_dalhs_int)+
  #   geom_sf(aes(geometry = geometry, fill = Value))+
  #   geom_point(aes(x = x, y = y, size = grid_area, alpha = grid_area))
  
  
  ## TODO: use sp intersect function to get areas within tracts and weight by area
  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <- census_dalhs_int$grid_area/ct_area
  
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
  new_geo <- 
    if (st != "DC") {
      tigris::counties(st) %>%
        mutate(INTPTLAT = as.numeric(INTPTLAT),
               INTPTLON = as.numeric(INTPTLON))
    } else {
      tigris::counties(st, cb = T)
    }
  
  #RETURN TO THIS; IT'S BECAUSE OF THE WAY THINGS ARE PULLED
  new_dal <- projectRaster(dal, crs = crs(new_geo))
  
  new_dal@data@names <- "Value"
  
  new_geo.list <-
    setNames(split(new_geo, seq(nrow(new_geo))), rownames(new_geo))
  
  #new_geo.list_PM <- mclapply(new_geo.list, get_county_pm, mc.cores = detectCores())
  new_geo.list_PM <- mclapply(new_geo.list, get_county_pm, new_dal = new_dal, mc.cores = 1)
  pm_data <- do.call("rbind", new_geo.list_PM)
  
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
  available_years <- c(2015, 2016, 2017, 2018)
  states <- c(setdiff(state.abb, c("AK", "HI")), "DC")
  
  if (length(state) == 0){
    state = states
  }
  
  state_df = as.data.frame(state)
  
  ACAG_pm_dat_State <- data.frame(state = character(),
                                 GEOID = numeric(),
                                 NAME = character(),
                                 Particulate.Matter = numeric())
  
  if (year %in% available_years){
    if (all(state %in% states)){
      
      dflist <- lapply(state, function(x){
        tempdf <- read.csv(file.path("data", "output", year, "State", paste0("Dalhousie_pm_dat_", x, ".csv"))) 
        tempdf <- tempdf %>%
          add_column(state = rep(x, nrow(tempdf)), .before = "GEOID")
        return(tempdf)
      })
      
      ACAG_pm_dat_State <- bind_rows(dflist)
      
      return(ACAG_pm_dat_State)
    }
    else{
      ###THROW EXCEPTION
      stop("Improper input, unrecognized state")
    }
  }
  
  else{
    if (all(state %in% states)){
      dalhousie <- raster(
        file.path(
          "data",
          "input",
          "acag_raw_data_files",
          paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
        ))
      dalhousie@data@names <- "Value"
      
      #NEED TO ADJUST SO IT DOES SAME STATES IN SAME GO
      dflist <- lapply(state, get_state_geo, city_df, dalhousie)
      ACAG_pm_dat_State <- bind_rows(dflist)
      return(ACAG_pm_dat_State)
    }
    else{
      ###THROW EXCEPTION
      stop("Improper input, unrecognized state")
    }
  }
}
