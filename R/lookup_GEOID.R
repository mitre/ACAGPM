
# states <- c(setdiff(state.abb, c("AK", "HI")), "DC")
#
# state_lookup <- tigris::states() %>%
#   dplyr::filter(STUSPS %in% states) %>%
#   sf::st_drop_geometry() %>%
#   dplyr::select(c(STATEFP, GEOID, STUSPS, NAME))
#
# county_lookup <- lapply(states, function(st){
#   temp_table <- tigris::counties(state = st) %>%
#     sf::st_drop_geometry() %>%
#     dplyr::select(c(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD))
# })
#
# county_lookup_temp <- county_lookup %>%
#   dplyr::bind_rows() %>%
#   dplyr::left_join(state_lookup %>% select(c(GEOID, STATEFP, STUSPS, NAME)), by = "STATEFP", suffix = c(".COUNTY", ".STATE"))
#
# tract_lookup <- lapply(states, function(st){
#   temp_table <- tigris::tracts(state = st) %>%
#     sf::st_drop_geometry() %>%
#     dplyr::select(c(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD))
# })
#
# tract_lookup_temp <- tract_lookup %>%
#   dplyr::bind_rows() %>%
#   dplyr::left_join(state_lookup %>% select(c(GEOID, STATEFP, STUSPS, NAME)), by = "STATEFP", suffix = c(".TRACT", ".STATE")) %>%
#   dplyr::left_join(county_lookup %>% select(c(GEOID, STATEFP, COUNTYFP, NAME, NAMELSAD)), by = c("STATEFP", "COUNTYFP"), suffix = c(".TRACT", ".COUNTY"))



## EXTERNAL

#' Look up GEOID of geographies
#'
#' Provides the user with the ability to look up GEOIDs for input geography.
#'
#' @param state, character string of selected state
#' @param county, character string of selected county
#' @param tract, character string of selected tract
#'
#' @return Character string representing GEOID corresponding to user input.
#'
#' @examples
#' state_GEOID <- lookup_GEOID(state = "AL")
#' county_GEOID <- lookup_GEOID(county = "Abbeville County, SC")
#' tract_GEOID <- lookup_GEOID(tract = "Census Tract 9502, Abbeville County, SC")
#' multiple_GEOID <- lapply(c("AL", "AZ", "AR"), function(st){
#'                       lookup_GEOID(state = st)})
#' @export
lookup_GEOID <- function(state = NULL, county = NULL, tract = NULL) {
  if (is.character(state) && length(state) == 1){
    state_lookup <- read.csv(system.file(file.path("data", "input", "state_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8")

    state_split <- unlist(strsplit(state, split = "\\s*,\\s*"))
    if (length(state_split) != 1){
      stop("Invalid input, string has improper length.")
    }

    geoid <- state_lookup %>%
      dplyr::filter(STUSPS == state | NAME.STATE == state) %>%
      dplyr::pull(GEOID.STATE)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(as.double(geoid))

  } else if (is.character(county) && length(county) == 1){
    county_lookup <- read.csv(system.file(file.path("data", "input", "county_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8")

    county_split <- unlist(strsplit(county, split = "\\s*,\\s*"))
    if (length(county_split) != 2){
      stop("Invalid input, string has improper length.")
    }

    co <- county_split[1]
    st <- county_split[2]

    geoid <- county_lookup %>%
      dplyr::filter((STUSPS == st & NAME.COUNTY == co) |
               (STUSPS == st & NAMELSAD.COUNTY == co) |
               (NAME.STATE == st & NAME.COUNTY == co) |
               (NAME.STATE == st & NAMELSAD.COUNTY == co)) %>%
      dplyr::pull(GEOID.COUNTY)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(as.double(geoid))

  } else if (is.character(tract) && length(tract) == 1){
    tract_lookup <- read.csv(system.file(file.path("data", "input", "tract_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8")

    tract_split <- unlist(strsplit(tract, split = "\\s*,\\s*"))
    if (length(tract_split) != 3){
      stop("Invalid input, string has improper length.")
    }

    tr <- tract_split[1]
    co <- tract_split[2]
    st <- tract_split[3]

    geoid <- tract_lookup %>%
      dplyr::filter((STUSPS == st & NAME.COUNTY == co & NAME.TRACT == tr) |
               (STUSPS == st & NAME.COUNTY == co & NAMELSAD.TRACT == tr) |
               (STUSPS == st & NAMELSAD.COUNTY == co & NAME.TRACT == tr) |
               (STUSPS == st & NAMELSAD.COUNTY == co & NAMELSAD.TRACT == tr) |
               (NAME.STATE == st & NAME.COUNTY == co & NAME.TRACT == tr) |
               (NAME.STATE == st & NAME.COUNTY == co & NAMELSAD.TRACT == tr) |
               (NAME.STATE == st & NAMELSAD.COUNTY == co & NAME.TRACT == tr) |
               (NAME.STATE == st & NAMELSAD.COUNTY == co & NAMELSAD.TRACT == tr)) %>%
      dplyr::pull(GEOID.TRACT)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(as.double(geoid))

  } else {
    stop("No input entered.")
  }
}
