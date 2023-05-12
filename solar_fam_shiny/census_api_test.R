
library(tidyverse)
library(censusapi)
library(jsonlite)
library(tidycensus)


api_key <- "ebab921b3002df9b71881ad6c426f34281ce0e11"

# Set API key
Sys.setenv(CENSUS_KEY="ebab921b3002df9b71881ad6c426f34281ce0e11")

# Define variables
variables <- c(
  tot_pop = "B01003"),
  median_home_USD = "B25077_001")
 # unemployment_rate = "DP03_0009E"


# Get ACS data for metro areas
acs_data <- get_acs(
  geography = "B01003",
  variables = variables,
  year = 2021
)

# Rename columns
colnames(acs_data) <- c("metro_area", "median_income", "population", "median_home_value")


# Rename columns

# Print the ACS data
print(acs_data)


############################

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="ebab921b3002df9b71881ad6c426f34281ce0e11")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

census_api_key <- "ebab921b3002df9b71881ad6c426f34281ce0e11"
hud_api_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjRmNGRhYmNkMDIwMWEwZjk4MGU2Y2JiZjM5NTQwMGI3ODZhODE0NDRmNGJkNGJkNGJhODZiNDlhMTNlY2E4OWE2M2I0ZDZmMjI1MTE2ZmY5In0.eyJhdWQiOiI2IiwianRpIjoiNGY0ZGFiY2QwMjAxYTBmOTgwZTZjYmJmMzk1NDAwYjc4NmE4MTQ0NGY0YmQ0YmQ0YmE4NmI0OWExM2VjYTg5YTYzYjRkNmYyMjUxMTZmZjkiLCJpYXQiOjE2Nzk1NDgxMTQsIm5iZiI6MTY3OTU0ODExNCwiZXhwIjoxOTk1MTY3MzE0LCJzdWIiOiI0ODUyMyIsInNjb3BlcyI6W119.e1YULHkALAJwQ2NoGdOi2_LvVinMRO-wO8C3xRetKi6GSU9MwMEIfo14xCLOjf6LHwz4ydEg-Xn0vKauh8RbAQ"

host <- "https://api.census.gov/data/2021"


job_end <- "acs/acs1?get=NAME,B01001_001&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:*&key="

version_url <- file.path(host,job_end)
api_version <- jsonlite::fromJSON(version_url) # retrieved cbsa numbers, msa names, and some value??


vt <- get_acs(geography = "cbsa", 
              variables = c(medincome = "B19013_001", 
                            tot_pop = "B01003_001", 
                            median_home_USD = "B25077_001"), 
              year = 2021)

# monthly variables

# from https://api.census.gov/data/2021/acs/acs1/profile/variables.html
unemployment_rate <- "DP03_0009E"
med_incom_and_benefits <- "DP03_0062E"
per_hous_occ <- "DP04_0005PE"


acs_variables <- listCensusMetadata(
  name = "acs/acs1",
  vintage = 2021,
  type = "groups") # include_values =TRUE
 msa_geo <- 310 # geoLevelDisplay from type = "geographies"


demo_data <- acs_variables %>% 
  filter(name == "B19013E_001E") # median household income
  filter(name == "DP03_0062E")
  
  

