
### GEOID Lookup

test_that("input errors for lookup", {
  expect_error(lookup_GEOID(), "No input entered.")
  expect_error(lookup_GEOID(state = ""), "Invalid input, string has improper length.")
  expect_error(lookup_GEOID(state = "x, y"), "Invalid input, string has improper length.")
  expect_error(lookup_GEOID(county = "x"), "Invalid input, string has improper length.")
  expect_error(lookup_GEOID(county = "x, y, z"), "Invalid input, string has improper length.")
  expect_error(lookup_GEOID(tract = "x, y"), "Invalid input, string has improper length.")
  expect_error(lookup_GEOID(tract = "x, y, z, a"), "Invalid input, string has improper length.")
})

test_that("nonexistent geographies", {
  expect_error(lookup_GEOID(state = "Fake"), "Invalid input, corresponding GEOID not found.")
  expect_error(lookup_GEOID(county = "Fake, Fake"), "Invalid input, corresponding GEOID not found.")
  expect_error(lookup_GEOID(tract = "Fake, Fake, Fake"), "Invalid input, corresponding GEOID not found.")
})

test_that("lookup works as intended", {
  expect_identical(lookup_GEOID(state = "AL"), "01")
  expect_identical(lookup_GEOID(county = "Abbeville County, SC"), "45001")
  expect_identical(lookup_GEOID(tract = "Census Tract 9502, Abbeville County, SC"), "45001950200")
})


### State

test_that("input errors for state", {
  expect_error(pull_state_ACAG(pull_type = "Fake", year = 2016, level = "National"), "Improper input, pull_type must be Internal or External")
  expect_error(pull_state_ACAG(pull_type = "Internal", year = 2016, level = "Fake"), "Improper input, unrecognized level")
  expect_error(pull_state_ACAG(pull_type = "Internal", year = 2099, level = "National"), "Improper input, invalid year")
  expect_error(pull_state_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c("Fake")), "Improper input, unrecognized state")
  expect_error(pull_state_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c(100)), "Improper input, must be character")
  expect_error(pull_state_ACAG(pull_type = "External", level = "National", acag = NULL), "Improper input, acag must be RasterLayer object")
  expect_error(pull_state_ACAG(pull_type = "External", level = "Fake", acag = raster::raster(tigris::states())), "Improper input, unrecognized level")
  expect_error(pull_state_ACAG(pull_type = "External", level = "State", state = c("Fake"), acag = raster::raster(tigris::states())), "Improper input, unrecognized state")
  expect_error(pull_state_ACAG(pull_type = "External", level = "State", state = c(100), acag = raster::raster(tigris::states())), "Improper input, must be character")
})

test_that("2015 state pull works as intended", {
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2015, level = "National")), c(49, 03))
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2015, level = "State", state = c("01", "05", "04"))), c(3, 3))
})

test_that("2016 state pull works as intended", {
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2016, level = "National")), c(49, 03))
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c("01", "05", "04"))), c(3, 3))
})

test_that("2017 state pull works as intended", {
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2017, level = "National")), c(49, 03))
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2017, level = "State", state = c("01", "05", "04"))), c(3, 3))
})

test_that("2018 state pull works as intended", {
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2018, level = "National")), c(49, 03))
  expect_equal(dim(pull_state_ACAG(pull_type = "Internal", year = 2018, level = "State", state = c("01", "05", "04"))), c(3, 3))
})


### County

test_that("input errors for county", {
  expect_error(pull_county_ACAG(pull_type  = "Fake", year = 2016, level = "National"), "Improper input, pull_type must be Internal or External")
  expect_error(pull_county_ACAG(pull_type  = "Internal", year = 2016, level = "Fake"), "Improper input, unrecognized level")
  expect_error(pull_county_ACAG(pull_type  = "Internal", year = 2099, level = "National"), "Improper input, invalid year")
  expect_error(pull_county_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c("Fake")), "Improper input, unrecognized state")
  expect_error(pull_county_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c(100)), "Improper input, must be character")
  expect_error(pull_county_ACAG(pull_type = "Internal", year = 2016, level = "County", county_state = c("Fake")), "Improper input, unrecognized county")
  expect_error(pull_county_ACAG(pull_type = "Internal", year = 2016, level = "County", county_state = c(100)), "Improper input, must be character")
  expect_error(pull_county_ACAG(pull_type = "External", level = "National", acag = NULL), "Improper input, acag must be RasterLayer object")
})

test_that("2015 county pull works as intended", {
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2015, level = "National")), c(3108, 5))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2015, level = "State", state = c("01", "05", "04"))), c(157, 4))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2015, level = "County", county_state = c("45001", "22001", "51001"))), c(3, 4))
})

test_that("2016 county pull works as intended", {
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2016, level = "National")), c(3108, 4))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c("01", "05", "04"))), c(157, 4))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2016, level = "County", county_state = c("45001", "22001", "51001"))), c(3, 4))
})

test_that("2017 county pull works as intended", {
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2017, level = "National")), c(3108, 4))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2017, level = "State", state = c("01", "05", "04"))), c(157, 4))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2017, level = "County", county_state = c("45001", "22001", "51001"))), c(3, 4))
})

test_that("2018 county pull works as intended", {
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2018, level = "National")), c(3108, 5))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2018, level = "State", state = c("01", "05", "04"))), c(157, 4))
  expect_equal(dim(pull_county_ACAG(pull_type = "Internal", year = 2018, level = "County", county_state = c("45001", "22001", "51001"))), c(3, 4))
})


### Tract

test_that("input errors for tract", {
  expect_error(pull_tract_ACAG(pull_type  = "Fake", year = 2016, level = "National"), "Improper input, pull_type must be Internal or External")
  expect_error(pull_tract_ACAG(pull_type  = "Internal", year = 2016, level = "Fake"), "Improper input, unrecognized level")
  expect_error(pull_tract_ACAG(pull_type  = "Internal", year = 2099, level = "National"), "Improper input, invalid year")
  expect_error(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c("Fake")), "Improper input, unrecognized state")
  expect_error(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c(100)), "Improper input, must be character")
  expect_error(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "County", county_state = c("Fake")), "Improper input, unrecognized county")
  expect_error(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "County", county_state = c(100)), "Improper input, must be character")
  expect_error(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "Tract", tract_county_state = c("Fake")), "Improper input, unrecognized tract")
  expect_error(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "Tract", tract_county_state = c(100)), "Improper input, must be character")
  expect_error(pull_tract_ACAG(pull_type = "External", level = "National", acag = NULL), "Improper input, acag must be RasterLayer object")
})

test_that("2015 tract pull works as intended", {
  # expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2015, level = "National")), c(72538, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2015, level = "National")), c(72223, 5))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2015, level = "State", state = c("01", "05", "04"))), c(3391, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2015, level = "County", county_state = c("45001", "22001", "51001"))), c(28, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2015, level = "Tract", tract_county_state = c("45001950200", "45001950300", "45001950600"))), c(3, 4))
})

test_that("2016 tract pull works as intended", {
  # expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "National")), c(72538, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "National")), c(72223, 5))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "State", state = c("01", "05", "04"))), c(3391, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "County", county_state = c("45001", "22001", "51001"))), c(28, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2016, level = "Tract", tract_county_state = c("45001950200", "45001950300", "45001950600"))), c(3, 4))
})

test_that("2017 tract pull works as intended", {
  # expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2017, level = "National")), c(72538, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2017, level = "National")), c(72223, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2017, level = "State", state = c("01", "05", "04"))), c(3391, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2017, level = "County", county_state = c("45001", "22001", "51001"))), c(28, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2017, level = "Tract", tract_county_state = c("45001950200", "45001950300", "45001950600"))), c(3, 4))
})

test_that("2018 tract pull works as intended", {
  # expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2018, level = "National")), c(72538, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2018, level = "National")), c(72223, 5))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2018, level = "State", state = c("01", "05", "04"))), c(3391, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2018, level = "County", county_state = c("45001", "22001", "51001"))), c(28, 4))
  expect_equal(dim(pull_tract_ACAG(pull_type = "Internal", year = 2018, level = "Tract", tract_county_state = c("45001950200", "45001950300", "45001950600"))), c(3, 4))
})
