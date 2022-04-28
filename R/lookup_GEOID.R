## EXTERNAL

#' Look up GEOID of geographies
#'
#' Provides the user with the ability to look up GEOIDs for input geography.
#'
#' @param state, character string of selected state. Full name or abbreviation. In the format "state".
#' @param county, character string of selected county. Name with or without parish/county/borough classification. In the format "county, state".
#' @param tract, character string of selected tract. Full name or just tract code. In the format "tract, county, state".
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
    state_lookup <- NULL
    load(system.file(file.path("extdata", "input", "state_lookup.RData"), package = "ACAGPM"))

    state_split <- unlist(strsplit(state, split = "\\s*,\\s*"))
    if (length(state_split) != 1){
      stop("Invalid input, string has improper length.")
    }

    geoid <- state_lookup %>%
      dplyr::filter(.data$STUSPS == state | .data$NAME == state) %>%
      dplyr::pull(.data$GEOID)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(geoid)

  } else if (is.character(county) && length(county) == 1){
    county_lookup <- NULL
    load(system.file(file.path("extdata", "input", "county_lookup.RData"), package = "ACAGPM"))

    county_split <- unlist(strsplit(county, split = "\\s*,\\s*"))
    if (length(county_split) != 2){
      stop("Invalid input, string has improper length.")
    }

    co <- county_split[1]
    st <- county_split[2]

    geoid <- county_lookup %>%
      dplyr::filter((.data$STUSPS == st & .data$NAME.COUNTY == co) |
               (.data$STUSPS == st & .data$NAMELSAD == co) |
               (.data$NAME.STATE == st & .data$NAME.COUNTY == co) |
               (.data$NAME.STATE == st & .data$NAMELSAD == co)) %>%
      dplyr::pull(.data$GEOID.COUNTY)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(geoid)

  } else if (is.character(tract) && length(tract) == 1){
    tract_lookup <- NULL
    load(system.file(file.path("extdata", "input", "tract_lookup.RData"), package = "ACAGPM"))

    tract_split <- unlist(strsplit(tract, split = "\\s*,\\s*"))
    if (length(tract_split) != 3){
      stop("Invalid input, string has improper length.")
    }

    tr <- tract_split[1]
    co <- tract_split[2]
    st <- tract_split[3]

    geoid <- tract_lookup %>%
      dplyr::filter((.data$STUSPS == st & .data$NAME.COUNTY == co & .data$NAME.TRACT == tr) |
               (.data$STUSPS == st & .data$NAME.COUNTY == co & .data$NAMELSAD.TRACT == tr) |
               (.data$STUSPS == st & .data$NAMELSAD.COUNTY == co & .data$NAME.TRACT == tr) |
               (.data$STUSPS == st & .data$NAMELSAD.COUNTY == co & .data$NAMELSAD.TRACT == tr) |
               (.data$NAME.STATE == st & .data$NAME.COUNTY == co & .data$NAME.TRACT == tr) |
               (.data$NAME.STATE == st & .data$NAME.COUNTY == co & .data$NAMELSAD.TRACT == tr) |
               (.data$NAME.STATE == st & .data$NAMELSAD.COUNTY == co & .data$NAME.TRACT == tr) |
               (.data$NAME.STATE == st & .data$NAMELSAD.COUNTY == co & .data$NAMELSAD.TRACT == tr)) %>%
      dplyr::pull(.data$GEOID.TRACT)

    if (length(geoid) == 0){
      stop("Invalid input, corresponding GEOID not found.")
    }

    return(geoid)

  } else {
    stop("No input entered.")
  }
}
