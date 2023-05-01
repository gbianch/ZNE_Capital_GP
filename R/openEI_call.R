# @param zipcode 


## function to select the most recent utiilty rate information for a given area code
get_zip_utility <- function(zipcode){
  # query for residential rates that are approved by experts and sorted by most recent effective date
  openEI_start_url <- "https://api.openei.org/utility_rates?version=latest&format=json&sector=Residential&detail=full&approved=true&sort_select=effectiveDate&direction=desc&address="
  openEI_api_end <- "&api_key=rU6MEqhK9OJ7obAnmcbzYhpFOenfBM5Q8GF0yJFA"
  
  # put url together
  openEI_url <- paste0(openEI_start_url, zipcode, openEI_api_end)
  # get the results for the query
  response <- GET(openEI_url)
  # extract content and parse as JSON
  content <- content(response, "text")
  json <- fromJSON(content)
  
  # error handling
  if(length(json$items) == 0) {
    message(paste("No utility data available for zipcode", zipcode))
    return(NA)
  }
  
  utility_df <- data.frame(
    # repeat zipcode in each row for each utility observation
    "zipcode" = rep(zipcode, each=nrow(json$items$label)),
    "start_date" = json$items$startdate,
    "label" = json$items$label,
    "utility" = json$items$utility,
    "rate_name" = json$items$name) 
  
  # Find index of row with most recent start date value
  max_index <- which.max(utility_df$start_date) 
  
  # Extract row with greatest seconds value
  max_row <- utility_df[max_index, ]
  return(max_row)
}
