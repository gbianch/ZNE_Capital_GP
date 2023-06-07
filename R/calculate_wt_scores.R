#' Compute Weighted Metrics or Criteria Score
#'
#' THis function normalizes values using the min and max of a given vector 
#' @param   data df of columns to be weighted
#' @param   weights vector of weights, should sum to one, to weight each column. Number of weights should match number of columns
#'
#' @return  Normalized column

calculate_wt_scores <- function(data, weights){
  
    # multiply each criteria by respective weight
    mutate(wt_real_estate = real_estate_score * weights[1],
           wt_landlord_policy = landlord_score * weights[2], 
           wt_solar_irr = financial_score * weights[3],
           wt_co2 = co2_score * weights[4], 
           wt_health_impacts = health_impact_score * weights[5],
           wt_electricity = electricity_score * weights[6],
           wt_climate_risk = climate_risk_score * weights[7]) %>% 
    # select weighted columns and rename them
    select(!c(ends_with("_score"))) %>%
    # create column for investment score
    mutate(investment_score = 0) %>%
    # for loop to sum weighted columns for each row
    mutate(investment_score = apply(.[, 4:10], 1, sum)) %>% 
    rename("Real Estate"="wt_real_estate",
           "Landlord Policy"="wt_landlord_policy",
           "Electricity Generation"="wt_electricity", 
           "Avoided CO2 Emissions"="wt_co2",
           "Climate Risk Avoided"="wt_climate_risk",
           "Avoided Health Costs"="wt_health_impacts", 
           "Solar Financials"="wt_solar_irr") %>%  
    unite("msa_state", city_msa:state, sep = ", ")
}
