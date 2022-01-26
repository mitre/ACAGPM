## INTERNAL

#' Function to save final processed data as CSV and Rdata files to output
#' directory
#' @param df data frame of processed data
#' @param county_name vector of county names (as strings) in state
#' @param state string - state abbreviation
save_data <- function(df, state){
  final_df.list <- c()
  
  for (name_index in 1:length(df)){
    county_name <- names(df)[name_index]
    pm_data <- df[[county_name]]
    # save(pm_data,
    #      file = file.path(
    #        save_dir,
    #        "Dalhousie_Particulate_Matter",
    #        year,
    #        "County",
    #        paste0("Dalhousie_pm_dat_", county_name, "_", state, ".RData")
    #      ))
    
    pm_data <- as.data.frame(pm_data)
      
    pm_data <-
      if (state != "DC") {
        pm_data %>%
          dplyr::select(GEOID, NAMELSAD, Particulate.Matter)
      } else {
        pm_data %>%
          dplyr::select(GEOID, NAME, Particulate.Matter)
      }
    
    pm_data <- pm_data %>%
      add_column(county_state = rep(paste0(county_name, "_", state), nrow(pm_data)), .before = "GEOID")
    
    final_df.list <- append(final_df.list, pm_data)
    
    # write.csv(
    #   pm_data,
    #   file.path(
    #     save_dir,
    #     "Dalhousie_Particulate_Matter",
    #     year,
    #     "County",
    #     paste0("Dalhousie_pm_dat_", county_name, "_", state, ".csv")
    #   ),
    #   row.names = F
    # )
  }
  
  final_df <- bind_rows(final_df.list)
  
  return(final_df)
}

#' Helper function to compute mean area weighted PM2.5 concentration for a given
#' census tract
#' @param census data frame - single census tract with geometry
#' @return data frame with mean weight PM2.5 estimates propagated for that CT
get_census_pm <- function(census, new_dal) {
  
  # Crop dalhousie data to match census tract boundary
  new_geo_sp <- as(census, "Spatial")
  dalhousie_crop <-
    crop(new_dal, new_geo_sp, snap = "out")
  dalhousie_df <- data.frame(rasterToPoints(dalhousie_crop))
  # Plot in base dalhousie
  # plot(dalhousie_crop)
  # plot(new_geo_sp, add = T)
  
  dalhousie_sp <-
    as(dalhousie_crop, "SpatialPolygonsDataFrame")
  dalhousie_sf <- st_as_sf(dalhousie_sp)
  new_geo_sf <- st_as_sf(new_geo_sp)$geometry
  census_dalhs_int <-
    st_intersection(dalhousie_sf, new_geo_sf)
  # plot(census_dalhs_int)
  
  # Area of each polygon
  census_dalhs_int$grid_area <-
    as.numeric(st_area(census_dalhs_int$geometry))
  
  
  # Make weight = area/total census tract area -- i.e. percent coverage
  ct_area <- as.numeric(st_area(new_geo_sf))
  census_dalhs_int$weight <-
    census_dalhs_int$grid_area / ct_area
  this_ct_val <-
    sum(census_dalhs_int$Value * census_dalhs_int$weight) / sum(census_dalhs_int$weight)
  
  census$Particulate.Matter <- this_ct_val
  
  gc()
  
  return(census)
}

#' Helper function to grab and transform all census tracts within a given county
#' and initialize particulate matter column to be passed to get_census_pm
#' @param cty_row data frame - single county with geometry
#' @param st string - state that county resides in
#' @return data frame of tract level data for county and state with empty PM column
get_census_geo <- function(cty_row, st, dal) {
  cty <- cty_row$COUNTYFP
  
  # Get specific county shape info
  county_geo <- tigris::tracts(state = st, county = cty) %>%
    mutate(INTPTLAT = as.numeric(INTPTLAT),
           INTPTLON = as.numeric(INTPTLON))
  
  # Make sure that CRS projections match
  new_dal <-
    #if (st == states[1] & cty == county_geo$COUNTYFP[1]) {
      projectRaster(dal, crs = crs(county_geo))
    #} else {
    #  new_dalhousie
    #}
  
  # Make sure name of data didn't change
  new_dal@data@names <- "Value"
  
  # Initialize particulate matter column
  new_geo <- county_geo %>%
    mutate(Particulate.Matter = NA_real_) %>%
    filter(AWATER / as.numeric(st_area(geometry)) < .9) # Remove tracts that are over 90
  
  
  return(list("new_geo" = new_geo, "new_dal" = new_dal))
}


#' Helper function to compute PM2.5 concentrations for every census tract for each
#' county in this state
#' @param st string - state
#' @param cty_df dataframe
get_county_geo <- function(st, cty_df, dal) {
  # Get Counties in state, filter to cty_df only in state
  cty_df.st <- cty_df %>%
    filter(state == st)
  
  print(paste0("Fetching county data for ", st, "..."))
  geo <-
    if (st != "DC") {
      tigris::counties(st) %>%
        mutate(INTPTLAT = as.numeric(INTPTLAT),
               INTPTLON = as.numeric(INTPTLON))
    } else {
      tigris::counties(st, cb = T)
    }
  
  # Convert to list to be able to use with mclapply
  name_col <- if (st != "DC") {
    "NAMELSAD"
  } else {
    "NAME"
  }
  cty_row.list <-
    setNames(split(geo, seq(nrow(geo))), geo[[name_col]])
  cty_row.list <- 
    cty_row.list[unlist(lapply(names(cty_row.list), function(x){
      x %in% cty_df.st$county}))]
  
  # Get census shape info
  print("Fetching census shape data for counties...")
  new_vars <- lapply(cty_row.list, get_census_geo, st = st, dal = dal)
  
  new_dal <- new_vars[[1]]$new_dal
  
  new_geo <- lapply(new_vars, function(x){
    return(x$new_geo)
  })
  
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
  #save_data(df = final_geo.df, state = st)
  #return(final_geo.df)
  return(save_data(df = final_geo.df, state = st))
}

## EXTERNAL

#' Pulls PM data at county level, either internally or externally.
#' If county field is empty, returns PM data for all counties.
#'
#' @param year
#' @param county_state
#'
#' @return dataframe, for PM2.5 of counties in each state
pull_county_ACAG <- function(year, county_state = c()){
  available_years <- c(2015, 2016, 2017, 2018)
  # cities <- read.csv(file.path("data", "input", "all_cities.csv")) %>%
  #   mutate(city_state = paste0(city, "_", state))
  us_cities <- read.csv(file.path("data", "input", "US_cities.csv")) #USE THIS LIST TO CONFIRM EXISTENCE
  # specific_us_cities <- read.csv(file.path("data", "input", "Specific_US_cities.csv"))
  states <- c(setdiff(state.abb, c("AK", "HI")), "DC")
  every_county <- read.csv(file.path("data", "input", "all_counties.csv")) %>%
    mutate(county_state = paste0(County, "_", State))
  
  state <- sapply(county_state, function(x){
    unlist(strsplit(x, split = "_"))[2]
  })
  
  county <- sapply(county_state, function(x){
    unlist(strsplit(x, split = "_"))[1]
  })
  
  county_df <- cbind.data.frame(county_state, county, state)
  
  ACAG_pm_dat_County <- data.frame(county_state = character(),
                                  GEOID = numeric(),
                                  NAME = character(),
                                  Particulate.Matter = numeric())
  
  if (year %in% available_years){
    #if (all(state %in% states)){ #check to see if there is a list of counties
    if (all(county_state %in% every_county$county_state)){
      dflist <- lapply(county_state, function(x){
        tempdf <- read.csv(file.path("data", "output", year, "County", paste0("Dalhousie_pm_dat_", x, ".csv"))) 
        tempdf <- tempdf %>%
          add_column(county_state = rep(x, nrow(tempdf)), .before = "GEOID")
        return(tempdf)
      })
      
      ACAG_pm_dat_County <- bind_rows(dflist)
      
      return(ACAG_pm_dat_County)
    }
    else if (length(county_state) == 0){
      #pull condensed
    }
    else{
      ###THROW EXCEPTION
      stop("Improper input, unrecognized county")
    }
  }
  
  else{
    if (all(county_state %in% every_county$county_state)){
      dalhousie <- raster(
        file.path(
          "data",
          "input",
          "acag_raw_data_files",
          paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
        ))
      dalhousie@data@names <- "Value"
      
      #NEED TO ADJUST SO IT DOES SAME STATES IN SAME GO
      #dflist <- mclapply(county_state, get_county_geo)
      dflist <- lapply(unique(state), get_county_geo, county_df, dalhousie)
      ACAG_pm_dat_County <- bind_rows(dflist)
      return(ACAG_pm_dat_County)
    }
    else if (length(county_state) == 0){
      #pull condensed
    }
    else{
      ###THROW EXCEPTION
      stop("Improper input, unrecognized county")
    }
  }
}
