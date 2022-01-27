## INTERNAL

# function to get the census data shape based on a given level
# NOTE: level gives you the information 1 level down (for example: level = us 
# gives all of the states, state gives all the counties, etc.)
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


# function to get the county for a given city and state
# NOTE: need to use numbers
#' @keywords internal
#' @noRd
get_city_county <- function(city,st){
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


#' Function to save final processed data as CSV and Rdata files to output
#' directory
#' @param df data frame of processed data
#' @param county_name vector of county names (as strings) in state
#' @param state string - state abbreviation
#' @keywords internal
#' @noRd
save_data <- function(df, state){
  for (name_index in 1:length(df)){
    this_city <- names(df)[name_index]
    pm_data <- df[[this_city]]
    save(pm_data, file = file.path(save_dir, 
                                   "Dalhousie_Particulate_Matter",
                                   year,
                                   "City",
                                   paste0("Dalhousie_pm_dat_", this_city, "_", state, ".RData")))
    
    pm_data <- as.data.frame(pm_data) %>%
      dplyr::select(GEOID, NAME, Particulate.Matter)
    write.csv(pm_data, file.path(save_dir, 
                                 "Dalhousie_Particulate_Matter",
                                 year,
                                 "City",
                                 paste0("Dalhousie_pm_dat_", this_city, "_", state, ".csv")),
              row.names = F)
  }
  
}


#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' census tract
#' @param census data frame - single census tract with geometry
#' @keywords internal
#' @noRd
get_census_pm <- function(census, new_dal) {
  dalhousie_crop <- crop(new_dal, census, snap = "out")
  dalhousie_df <- as.data.frame(dalhousie_crop, xy = T)
  # Replace tracts that are all NA with 0.0
  if (all(is.na(dalhousie_df$Value))){
    next
    # dalhousie_crop@data@values <- 0.0
  }
  # Plot in base dalhousie
  # plot(dalhousie_crop)
  # plot(census, add = T)
  
  dalhousie_sp <- as(dalhousie_crop, "SpatialPolygonsDataFrame")
  dalhousie_sf <- st_as_sf(dalhousie_sp)
  new_geo_sf <- st_as_sf(census)$geometry
  census_dalhs_int <- st_intersection(dalhousie_sf, new_geo_sf)
  # plot(census_dalhs_int)
  
  # Area of each polygon
  census_dalhs_int$grid_area <- as.numeric(st_area(census_dalhs_int$geometry))
  
  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <- census_dalhs_int$grid_area/ct_area
  this_ct_val <- sum(census_dalhs_int$Value * census_dalhs_int$weight)/sum(census_dalhs_int$weight)
  
  census@data$Particulate.Matter <- this_ct_val
  
  gc()
  
  return(census)
}


#' Helper function to grab and transform all census tracts within a given city
#' and initialize particulate matter column to be passed to get_census_pm
#' @param cty_row data frame - single city with geometry
#' @param st string - state that city resides in
#' @keywords internal
#' @noRd
get_census_geo <- function(this_city, st, city_df) {
  # Need the city boundaries
  new_geo <- get_census_shape("city", st = st, city = this_city)
  
  # Make sure that CRS projections match
  new_dal <- #if (this_city == city_df$city[1]){
    projectRaster(dalhousie, crs = crs(new_geo))
  #}else { NULL }
  
  # Make sure name of data didn't change
  new_dalhousie@data@names <- "Value"
  
  #return(city_shp)
  return(list("new_geo" = new_geo, "new_dal" = new_dal))
}


#' Helper function to compute PM2.5 concentrations for every census tract for each
#' city in this state
#' @param st string - state
#' @keywords internal
#' @noRd
#get_city_geo <- function(st) {
get_city_geo <- function(city_name, city_df, dal) {
  
  st <- city_df %>%
    filter(city_state == city_name) %>%
    pull(state)
  
  these_cities <- city_df %>%
    filter(city_state == city_name) %>%
    pull(city)
  
  print("Fetching census shape data for counties...")
  new_vars <- lapply(these_cities, function(this_city){
    get_census_geo(this_city, st = st, city_df = city_df, dal = dal)
  })
  
  new_dal <- new_vars[[1]]$new_dal
  
  new_geo <- lapply(new_vars, function(x){
    return(x$new_geo)
  })
  
  # Assign rownames
  new_geo.list <- lapply(new_geo, function(df){
    setNames(split(df, seq(nrow(df))), df$GEOID)
  })
  
  names(new_geo.list) = these_cities
  
  # Compute mean area concentration for CTs
  print("Computing mean area weighted PM2.5...")
  final_geo <- lapply(new_geo.list, function(df){
    #mclapply(df, get_census_pm, mc.cores = detectCores())
    mclapply(df, get_census_pm, new_dal = new_dal, mc.cores = 1)
  })
  
  # Combine into single df
  final_geo.sf <- lapply(final_geo, function(df){
    lapply(df, st_as_sf)
  })
  final_geo.df <- lapply(final_geo.sf, bind_rows)
  
  this_city <- names(final_geo.df)[1]
  pm_data <- final_geo.df[[this_city]]
  pm_data <- as.data.frame(pm_data) %>%
    dplyr::select(GEOID, NAME, Particulate.Matter) %>%
    add_column(city_state = rep(city_name, nrow(pm_data)), .before = "GEOID")
  
  return(pm_data)

  # # Save
  # paste0(paste0("Saving ", st, "..."))
  # save_data(df = final_geo.df, state = st)
}

## EXTERNAL

#'Pulls PM data at city level, either internally or externally.
#'If city_state list is empty, returns condensed city list.
#'
#'
#'@param year
#'@param city_state, list containing desired cities in format "city_st"
#'
#'@return dataframe containing PM2.5 for tracts in each city, or PM2.5 tracts for all cities ((NOT AVAILABLE))
pull_city_ACAG <- function(year, city_state = c()){
  
  available_years <- c(2015, 2016, 2017, 2018)
  cities <- read.csv(file.path("data", "input", "all_cities.csv")) %>%
    mutate(city_state = paste0(city, "_", state))
  # us_cities <- read.csv(file.path("data", "input", "US_cities.csv"))
  # specific_us_cities <- read.csv(file.path("data", "input", "Specific_US_cities.csv"))
  
  if (length(city_state) == 0){
    city_state = cities$city_state
  }
  
  state <- sapply(city_state, function(x){
    unlist(strsplit(x, split = "_"))[2]
  })
  
  city <- sapply(city_state, function(x){
    unlist(strsplit(x, split = "_"))[1]
  })
  
  city_df <- cbind.data.frame(city, state, city_state)
  
  ACAG_pm_dat_City <- data.frame(city_state = character(),
                                 GEOID = numeric(),
                                 NAME = character(),
                                 Particulate.Matter = numeric())
  
  if (year %in% available_years){
    if (all(city_state %in% cities$city_state)){
      
      dflist <- lapply(city_state, function(x){
        tempdf <- read.csv(file.path("data", "output", year, "City", paste0("Dalhousie_pm_dat_", x, ".csv"))) 
        tempdf <- tempdf %>%
          add_column(city_state = rep(x, nrow(tempdf)), .before = "GEOID")
        return(tempdf)
      })
      
      ACAG_pm_dat_City <- bind_rows(dflist)
      
      return(ACAG_pm_dat_City)
    }
    else{
      ###THROW EXCEPTION
      stop("Improper input, unrecognized city")
    }
  }
  
  else{
    if (all(city_state %in% cities$city_state)){
      dalhousie <- raster(
        file.path(
          "data",
          "input",
          "acag_raw_data_files",
          paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
        ))
      dalhousie@data@names <- "Value"
      
      #NEED TO ADJUST SO IT DOES SAME STATES IN SAME GO
      dflist <- lapply(city_state, get_city_geo, city_df, dalhousie)
      #dflist <- lapply(unique(cities$state), get_city_geo)
      ACAG_pm_dat_City <- bind_rows(dflist)
      return(ACAG_pm_dat_City)
    }
    else{
      ###THROW EXCEPTION
      stop("Improper input, unrecognized city")
    }
  }
  
  #return(Dalhousie_pm_dat_US)
}