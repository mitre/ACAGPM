## EXTERNAL

#' Look up GEOID of geographies
#'
#' Provides the user with the ability to look up GEOIDs for input geography.
#'
#' @param state, character string of selected state. Full name or abbreviation.
#' @param county, character string of selected county. Name with or without parish/county/borough classification.
#' @param tract, character string of selected tract. Full name or just tract code.
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
    state_lookup <- read.csv(system.file(file.path("data", "input", "state_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8", colClasses = c("STATEFP" = "character", "GEOID" = "character"))

    state_split <- unlist(strsplit(state, split = "\\s*,\\s*"))
    if (length(state_split) != 1){
      stop("Invalid input, string has improper length.")
    }

    geoid <- state_lookup %>%
      dplyr::filter(STUSPS == state | NAME == state) %>%
      dplyr::pull(GEOID)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(geoid)

  } else if (is.character(county) && length(county) == 1){
    county_lookup <- read.csv(system.file(file.path("data", "input", "county_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8", colClasses = c("STATEFP" = "character", "COUNTYFP" = "character", "GEOID.COUNTY" = "character", "GEOID.STATE" = "character"))

    county_split <- unlist(strsplit(county, split = "\\s*,\\s*"))
    if (length(county_split) != 2){
      stop("Invalid input, string has improper length.")
    }

    co <- county_split[1]
    st <- county_split[2]

    geoid <- county_lookup %>%
      dplyr::filter((STUSPS == st & NAME.COUNTY == co) |
               (STUSPS == st & NAMELSAD == co) |
               (NAME.STATE == st & NAME.COUNTY == co) |
               (NAME.STATE == st & NAMELSAD == co)) %>%
      dplyr::pull(GEOID.COUNTY)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(geoid)

  } else if (is.character(tract) && length(tract) == 1){
    tract_lookup <- read.csv(system.file(file.path("data", "input", "tract_lookup.csv"), package = "ACAGPM"), encoding = "UTF-8", colClasses = c("STATEFP" = "character", "COUNTYFP" = "character", "GEOID.TRACT" = "character", "NAME.TRACT" = "character", "GEOID.STATE" = "character", "GEOID.COUNTY" = "character"))

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

    return(geoid)

  } else {
    stop("No input entered.")
  }
}
