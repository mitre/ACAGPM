# ACAGPM

Particulate Matter (PM2.5) is a term referring to microscopic particles, not greater than 2.5 micro-meters in diameter, suspended in air. These particles have a variety of adverse health effects on humans, given their ability to quickly enter the bloodstream via inhalation.

Research initiatives, especially those relating to climate and human health, can greatly benefit from access to PM2.5 data; this package aims to provide users with easy access to that data.

### Installing and Running

Installing entails the following process:

1. Download this repository to a directory of your choosing.

2. Set working directory to the directory containing this repository.

3. Install dependencies.

#### General users

4. Use the function **devtools::build()**. This will build the package in a structure which R can read as it does other packages.

5. Take note of the file path which was returned from **devtools::build()**. Set a variable **file_path** to that file path. Use the following command: **install.packages(file_path, repos = NULL, type = "source")**

6. Use the command **library(ACAGPM)**. The package will now be accessible for use.

#### Developers

4. Use the function **devtools::load_all()**. This will load the functions for use as if the package were built and loaded.

### Usage and structure

The package has 2 basic functionalities, and an additional functionality, gone into detail in their own sections. Further detail on each function and their uses is provided in the vignette.

Particulate Matter values for a geography are found using area-weighted mean calculations. Some things to note:

- PM concentrations are the PM values found in a raster file grid block.

- Weight is the area of a grid block contained in the geography, divided by the total area of the geography.

- Exact value is found by summing the PM concentrations multiplied by the corresponding weights, then dividing that sum by the sum of all weights.

Output of the basic functionalities are formed around their potential for use with census data via the GEOID field. Users will be able to take other data available through packages such as *tidycensus* and join with this data, creating practical analyses with ease.

#### Basic: Internal Data Pulls

Data can be pulled from within the package with the functions **pull_state_ACAG**, **pull_county_ACAG**, and **pull_tract_ACAG**. Years 2015-2018 are currently available for internal pulls. Each function has a similar structure:

- Process user input and make sure it is acceptable using the lookup tables from the */ACAGPM/inst/extdata/input* subdirectory.

- Iterate through input, pulling files from the */ACAGPM/inst/extdata/output* subdirectory and combining into a dataframe.

- Return output to the user containing NAME, GEOID, and Particulate.Matter fields for geographies.

#### Basic: External Data Pulls

Data for years 2000-2014 can be pulled externally with the functions **pull_state_ACAG**, **pull_county_ACAG**, and **pull_tract_ACAG**. Additional steps must be taken before using the functions.

1. Retrieve Raster files

    - Go to the [Surface PM2.5 Data Folder](https://wustl.app.box.com/v/ACAG-V4NA03-PM25/folder/136303334735) owned by Washington University in St. Louis' Atmospheric Composition and Analysis Group. Files will be in the format *V4NA03_PM25_NA_**YEAR**01_**YEAR**12-RH35-NoNegs.asc.zip* where **YEAR** is the desired year of data. Save the file to a directory of your choosing. Then, use your favorite file compression tool to unzip the file. There will be 2 files: *V4NA03_PM25_NA_**YEAR**01_**YEAR**12-RH35-NoNegs.asc* and *V4NA03_PM25_NA_**YEAR**01_**YEAR**12-RH35-NoNegs.prj*. The *.asc* file will be of use to us in this case.

2. Load Raster Object in R

    - Exectute the following code in R. This will load the raster file into R as a RasterLauer object. Make sure that file_dir is the directory which contains the PM2.5 file, and year matches the **YEAE** value in the filename.

```
library(raster)

file_dir <- "C:/Users/userName/Desktop/ACAGPM"
year <- 2000

acag <- raster::raster(
  file.path(
    file_dir, 
    paste0("V4NA03_PM25_NA_", year, "01_", year, "12-RH35-NoNegs.asc")
  ))

acag@data@names <- "Value"
```

With those 2 steps complete, a function of your choosing may be called. Each function has a similar structure:

- Process user input and make sure it is acceptable using the lookup tables from the */ACAGPM/inst/extdata/input* subdirectory.

- Pass states containing selected input to an internal function which does most of the heavy lifting.

- Pull geography shapefiles from tigris. Turn into lists for use with lapply and parallelized equivalent, llply.

- Filter geographies to those selected in user input.

- Set crs of raster object to match that of the geographies.

- Create a cluster for parallel computation.

- Perform parallel computation of area-weighted mean particulate matter values for each geography after projecting raster file's values onto shapfile.

- Combine list into dataframe and pull GEOID, NAME, and Particulate.Matter fields.

- Return output to the user.

#### Additional: lookup_GEOID

This function allows the user to look up the GEOID of a state, county, or tract.

- Uses dataframes saved as .RDS files in the */ACAGPM/inst/extdata/input* subdirectory.

- Relatively straightforward in build. Input is split, used as a filter for the dataframe, output is pulled, and returned to the user.

### Contact

For more information about the Asthma Equity Explorer Dashboard, please contact:

Hannah De los Santos, PhD, Co-Principal Investigator at [hdelossantos\@mitre.org](hdelossantos@mitre.org)

Cassandra Okechukwu, PhD, Co-Principal Investigator at [cokechukwu\@mitre.org](cokechukwu@mitre.org)
