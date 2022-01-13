#All final functions

setwd("C:/Users/ALIBERATORE/Desktop/Health Equity MIP/Gitlab Repo/ACAGPM")




#'Pulls PM data at state level, either internally or externally
#'
#'
#'@param year
#'@param state
#'@param condensed
#'
#'@return dataframe

pull_state_ACAG <- function(year, state, condensed = FALSE){
  if (condensed){
    
    
  }
  else{
    available_years <- c(2015, 2016, 2017, 2018)
    
    dir.create(file.path("data", year, "State"))
    
    file_path = file.path()
    
    if (year %in% available_years){
      return(read.csv(file.path("data", year, "State", paste0(paste0("Dalhousie_pm_dat_", state), ".csv"))))
    }
    
    else{
      Dalhousie_pm_dat_US <- "temp_df"
      return(Dalhousie_pm_dat_US)
    }
  }
}


#'Pulls PM data at county level, either internally or externally
#'
#'
#'@param year
#'@param state
#'@param county
#'@param condensed
#'
#'@return dataframe

pull_county_ACAG <- function(year, state, county, condensed = FALSE){
  
  if (year %in% available_years){
    return(read.csv(file.path("data", year, "State", paste0(paste0("Dalhousie_pm_dat_", state), ".csv"))))
  }
  
  else{
    Dalhousie_pm_dat_US <- "temp_df"
    return(Dalhousie_pm_dat_US)
  }
}


#'Pulls PM data at census level, either internally or externally
#'
#'
#'@param year
#'
#'@return dataframe

pull_census_ACAG <- function(year){
  
  if (year %in% available_years){
    return(read.csv(file.path("data", year, "State", paste0(paste0("Dalhousie_pm_dat_", state), ".csv"))))
  }
  
  else{
    Dalhousie_pm_dat_US <- "temp_df"
    return(Dalhousie_pm_dat_US)
  }
}