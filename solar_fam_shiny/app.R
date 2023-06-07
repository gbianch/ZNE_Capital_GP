library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib) # shiny theme
library(tidycensus)
library(googlesheets4)
library(sf) # map states and US
library(readxl)
library(here)
library(tmap)
library(tigris)
library(tmaptools)

cbsa_geom <- core_based_statistical_areas(cb = TRUE) %>% 
  mutate(cbsa = factor(CBSAFP))


states_geom <- tidycensus::state_laea    #read in the dataset


# state and U.S. map coordinates
states <- map_data("state") %>% 
  st_as_sf(coords = c("long", "lat"))

usa<- map_data("usa") %>% 
  st_as_sf(coords = c("long", "lat"))

real_estate_metrics <- read_csv(here("data/intermediate/n_real_estate_metrics.csv")) %>% 
  select(cbsa, anchor_city, msa, starts_with("n_")) %>% 
  mutate(cbsa = factor(cbsa)) %>% 
  rename("n_employ_change" = n_avg_emp_change)

criteria_unweighted <- read_csv(here("data/intermediate/unwt_criteria_scores.csv")) %>% 
  mutate(cbsa = factor(cbsa),
         real_estate_score = 0) 


ui <- fluidPage(
  # Application title
  titlePanel("title"),
  navbarPage("solar fam",
             theme = bs_theme(bootswatch = "cerulean", primary = "#8aab57"),
             ### tab 1 - background
             tabPanel("Landing Page", 
                      mainPanel(
                        h5("Abstract"),
                        p("As the renewable energy transition accelerates, housing, due to its high energy demand, can play a critical role in the clean energy shift. Specifically, multifamily housing provides a unique opportunity for solar photovoltaic (PV) system adoption, given the existing competing interests between landlords and tenants which has historically slowed this transition. Landlords are less incentivized to install solar due to upfront costs, but ultimately tenants receive the benefits from solar installations in the form of reduced electricity bills. To address this transition gap, this project identified and ranked Metropolitan Statistical Areas (MSAs) in the United States for ZNE Capital (the client) to acquire multifamily housing to install solar PV systems. Working with the client, the group identified seven criteria to determine favorable markets for rooftop solar PV on multifamily housing: landlord policy favorability, real estate market potential, CO2 abatement potential, electricity generation potential, solar installation internal rate of return, climate risk avoidance, and health costs associated with primary air pollutants. A total investment favorability score is calculated based on criteria importance assigned by the user.  Investment favorability scores were investigated for different preferences to demonstrate the robustness and generalizability of the framework. The data analysis and criteria calculations were conducted using RStudio, ultimately to provide reproducible code to be used for future projects. The results are presented in a ranked list and GIS map of MSAs based on the overall favorability score. In addition, GIS maps of each criterion are included with the relative scores of each MSA to shed light on geographic trends. Future studies can utilize the reproducible code to inform decisions on where to invest in solar PV on multifamily housing anywhere in the United States by changing weights within the model depending on preferences."),
                        
                        h5("Importance & Objectives"),
                        p("The main objective of this project was to develop a model that includes seven criteria to identify U.S. MSAs ideal for installing rooftop solar PV systems on multifamily housing complexes. This model was designed to be flexible enough to capture the perspectives and desires of the various stakeholders for such investments, including real estate companies interested in solar PV, solar installation nonprofits, and state governments, which can express their priorities through weights assigned for each of the seven criteria. Each criterion consists of indicator metrics calculated from data. Each criterion is multiplied by its respective user-assigned weight to determine the investment favorability score. Once created, the model was employed to identify MSAs in the United States where the acquisition of multifamily housing complexes and installation of rooftop solar PV was most favorable for the client, Zero Net Energy (ZNE) Capital. In addition to the client scenario, an example equity-centered scenario was calculated using this model by adjusting the seven criteria weights, giving higher weighting to health impacts and CO2 emissions avoided. These weights can be adjusted based on the priorities of the model user."),
                        h5("Model Flow Chart"),  ### Add model image here
                        p("Visualization of methodology to calculate investment favorability score for each metropolitan area. The investment favorability score of rooftop solar on apartment buildings was calculated as a weighted additive total of the seven criteria scores."),
                        div(img(src = "model_no_weights.jpg",  width = "1000", height = "500"), style="text-align: center;"),
                        p("Weights for each criterion can be adjusted in the model based on user priorities. The client’s weights reflect relative importance in ZNE Capital’s decision making for their business model. An example equity-centric scenario was included to represent non-profit or government stakeholders that maximize positive social and environmental impact by prioritizing CO2 abatement potential and health impacts."),
                      ) ### end mainPanel
             ), # ## END of tab 1
             ### tab2
             tabPanel("Use the Model",
                      tabsetPanel(
                        tabPanel("Step 1: Demographic Preferences",
                                 sidebarLayout(
                                   sidebarPanel(
      
                                      # inputs for real estate metrics
                                      numericInput(inputId = "PopChgWtInput", label = "Population Change",
                                                   value = 0,width = "175px", min = 0.0,max = 1.0,step = 0.05), # end RE weight input
                                      
                                      numericInput(inputId = "EmployWtInput", label = "Employment Change",
                                                   value = 0.0, width = "175px", min = 0.0,max = 1.0,step = 0.05),
                                      
                                      numericInput(inputId = "RentChgWtInput", label = "Rent Change",
                                                   value = 0.10,width = "175px", min = 0.0, max = 1.0, step = 0.05),
                                      
                                      numericInput(inputId = "OccWtInput", label = "Occupancy Rate",
                                                   value = 0.15,width = "175px", min = 0.0, max = 1.0,step = 0.05),# end Co2 weight input
                                      
                                      numericInput(inputId = "RentIncomeWtInput", label = "Rent to Income",
                                                   value = 0.10,width = "175px", min = 0.0, max = 1.0, step = 0.05), # end climate risk weight input
                                      
                                      numericInput(inputId = "IncomeHomeWtInput", label = "Income to Home Price",value = 0.10,
                                                   width = "175px",min = 0.0,max = 1.0, step = 0.05), # end health impact weight input
                                
                                      actionButton(inputId = "EnterREWeights", label = "Calculate")), # end sidebar panel
                                 
                                         
                                   mainPanel(dataTableOutput(outputId = "RE_wt_table"))
                                 )), # end step 1 tab panel
                        tabPanel("Step 2: Landlord Inputs",
                                 mainPanel(plotOutput(outputId = "RE_map"))), # end step 2 tab panel
                        tabPanel("Step 3. Criteria Weights", 
                                 br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     ### inputs for criteria weights
                                     numericInput(inputId = "REWtInput", label = "Real Estate",
                                                  value = 0.18,width = "175px", min = 0.0,max = 1.0,step = 0.05), # end RE weight input
              
                                     numericInput(inputId = "LandlordWtInput", label = "Landlord Policy",
                                                       value = 0.07, width = "175px", min = 0.0,max = 1.0,step = 0.05),
                                     
                                     numericInput(inputId = "ElectrictyWtInput", label = "Electricity Generation",
                                                          value = 0.07,width = "175px", min = 0.0, max = 1.0, step = 0.05),
                                     
                                     numericInput(inputId = "CO2WtInput", label = "CO2 Emissions Abated",
                                                  value = 0.07,width = "175px", min = 0.0, max = 1.0,step = 0.05),# end Co2 weight input
                                     
                                     numericInput(inputId = "ClimateRiskWtInput", label = "Climate Risk Avoided",
                                                  value = 0.50, width = "175px", min = 0.0, max = 1.0, step = 0.05), # end climate risk weight input
                                     numericInput(inputId = "HealthWtInput", label = "Reduced Health Impacts",value = 0,
                                                             width = "175px",min = 0.0,max = 1.0, step = 0.05), # end health impact weight input
                                     numericInput(inputId = "SolarIrrWtInput", label = "Solar Financials",
                                                          value = 0.11, width = "175px", min = 0.0, max = 1.0, step = 0.05), # end irr input
                                     actionButton(inputId = "EnterWeights", label = "Calculate")), # end sidebar panel
                                   
                                   mainPanel( 
                                     p("information about each criteria"),
                                     p("Real Estate:"),
                                     p("Landlord"),
                                     p("Electricity Generation:"),
                                     p("Solar Financials:"),
                                     verbatimTextOutput(outputId = "wt_sum"),
                                     h5("Ranking the top metropolitan areas based on solar PV investment favorability"),
                                     plotOutput(outputId = "wt_criteria_chart"))
                                   ) # end sidebar layout
                                 ), # end tab panel
                        tabPanel("Export Results", 
                                 mainPanel(dataTableOutput(outputId = "wt_table_test")))
                        
                             
                      ) # end tabset panel
             ), # end tab panel
             
             tabPanel("Project Findings",
                      mainPanel(tmapOutput("RE_tmap"))), # end tab panel
             tabPanel("Data Exploration", icon = icon("info-circle"),
                      tabsetPanel(
                        tabPanel("Demographics",
                                 h5("Population Growth"),
                                 p("The metrics included in this criteria were population growth, employment growth, average annual occupancy, annual rent change, the ratios of median annual rent to median income, and median income to median home price. The data for each metric was gathered and normalized individually before being weighted and used to calculate the Real Estate score (see section 2.7, Data Normalization and Weights)."),
                                  p("Population growth in a MSA is a commonly used real estate metric to gauge the demand for housing. The National Association of Realtors (NAR) identifies population growth as a key factor in determining a region's real estate market potential and a strong indicator of future demand for housing (Tracey, 2022). The U.S. Census Bureau provides accurate demographic data at the MSA level, making it a reliable source for population estimates. To quantify the change in MSA populations, the group utilized the annual population estimates provided by the U.S. Census Bureau between April 1, 2020, and July 1, 2021. Additionally, the group excluded MSAs with a population growth rate below 0.5% to streamline the analysis. Population change was calculated using the following equation: Population change =2021 population estimate- 2020 population estimate2020 population estimate100"),

                                 
                             
                                 h5("Rent Change"),
                                 p("Median rent rates for fiscal year 2021 and 2022 were gathered for each MSA from the U.S. Department of Housing and Urban Development (HUD) and represent rates for one bedroom apartments (Office of Policy Development and Research, 2021). HUD is a government agency that collects and maintains vast amounts of data related to housing, community development, and urban affairs, which is rigorously reviewed, analyzed, and made available to the public. The agency is widely regarded as a reliable source for its’ standardized methodologies for data collection."),
                                 p("It's worth noting that ZNE Capital offers apartments of various sizes, ranging from studios (~400 ft2) to three-bedroom units(~1,150 ft2), with the most common unit size being a 2 bedroom 1 bath apartment (~900 ft2). Although the collected rental rates don't accurately represent the majority of ZNE Capital's units, assuming a one-bedroom apartment rental rate allows for a valid comparison between MSAs, as apartments across MSAs are uniform in size."),
                                
                                h5("Employment Rates"),
                                 p("This metric was included to ensure potential areas had prosperous economies as employment growth improves individuals ability to pay rent. Monthly unemployment rates were collected from the civil labor force data provided by the Bureau of Labor Statistics (BLS), an independent federal statistical agency that collects and analyzes data on labor market activity, including employment and unemployment rates. The BLS uses a sample survey methodology to collect data from households and businesses across the country, ensuring that its data is representative of the entire population. In the survey methodology, the civilian labor force includes those “who worked during reference week as paid employees, worked in their own business or profession, worked on their own farm, or worked 15 hours or more as unpaid workers on a family farm or in a family business; or those who did not work during the reference week but had jobs or businesses from which they were temporarily absent due to illness, bad weather, industrial dispute, vacation, or other personal reasons. This data excludes individuals working around the house or unpaid volunteer work for religious, charitable, and similar organizations; also excluded are all institutionalized people and people on active duty in the United States Armed Forces” (U.S. Bureau of Labor Statistics, 2022). Monthly employment rates were calculated by dividing the employment count by the civilian labor force, and then the monthly rates were averaged to annual employment rates. The change in employment growth was calculated using the following equation: change in employment rate=2021 average employment rate- 2020 average employment rate2020 average employment rate100 "),
                                
                                h5("Median Rent to Median Income"),
                                p("The U.S. Census Bureau provided data on the household median income for 2021 at the MSA level, using a standardized distribution in increments of $2,500 (U.S. Census Bureau, 2020, 89). To avoid a positive skew caused by high-income outliers, the median income was chosen over the mean income as a population indicator."),
                                p("To assess apartment affordability, the 2021 annual median rent was divided by the median income, resulting in a rent-to-income ratio. Lower values indicate better affordability, which is favorable for ZNE Capital, as they seek tenants who are not rent-burdened. The rent-to-income ratio was then subtracted from 1 to generate an additive real estate score, with higher values indicating greater affordability and a better ability for renters to pay. This indicator directly captures renters' ability to pay, making it a useful tool for investment decision-making. Equations for calculations are seen below:"),
                                 
                                
                                h5("Median Income to Median House Price"),
                                p("Median house prices were sourced from the National Association of REALTORS®, a prominent real estate platform for realtors. To determine the level of housing affordability, the 2021 median income was divided by the 2021 median house price, producing a metric that reflects the affordability of houses given income. Lower values indicate less affordability, which may lead individuals to rent. Therefore, the ratio was subtracted from 1 with higher values representing less affordable houses, meaning there individuals are more likely to rent than buy a house. Equations are below:"),
                                 
                                
                                h5("Apartment Occupancy Rates"),
                                 p("Occupancy rate is the ratio of rented units to the total available units for rent in an MSA. Yardi Matrix is a robust research platform that gives users access to property-level information, including occupancy rates, for multifamily properties in the United States (Yardi Systems, n.d.). Students used Yardi Matrix to collect average occupancy rates at the city level, identifying an anchor city based on the largest population size for MSAs comprising more than one city.  For this metric, lower occupancy rates in an MSA signify lower demand for rental units."),
                                 ),# end demographics tab panel
                        tabPanel("Climate Risk"),
                        tabPanel("reOpt Model")
                      )),

  ) ### end nav bar
  
) ### end fluidPage


# Define server logic 
server <- function(input, output) {
  
  ### real estate metric weight inputs
  REweight_inputs <- reactive({
    c(input$PopChgWtInput, 
      input$EmployWtInput,
      input$OccWtInput,
      input$RentChgWtInput,
      input$IncomeHomeWtInput,
      input$RentIncomeWtInput)  
  })
  
  
  # real estate weighted table
  output$RE_wt_table <- renderDataTable({
    req(input$EnterREWeights)
  wt_real_estate_metrics <- real_estate_metrics %>% 
    mutate(wt_pop_change = n_pop_growth* input$PopChgWtInput,
           wt_employ_change = n_employ_change* input$EmployWtInput,
           wt_occ = n_occ_rate2021* input$OccWtInput,
           wt_rent_change = n_rent_change* input$RentChgWtInput,
           wt_rent_income = n_rent_to_income* input$RentIncomeWtInput,
           wt_income_homeprice = n_income_to_homeprice* input$IncomeHomeWtInput) %>% 
    select(!starts_with("n_")) %>% 
    mutate(real_estate_score = apply(.[,4:9], 1, sum)) %>% 
    mutate_if(is.numeric, round, digits = 4) %>% 
    select(!c(msa, anchor_city)) %>% 
    rename("Population Change"="wt_pop_change",
           "Employment Change"="wt_employ_change",
           "Occupancy Rate"="wt_occ", 
           "Rent Change"="wt_rent_change",
           "Rent to Income"="wt_rent_income",
           "Income to Homeprice"="wt_income_homeprice") #%>% 
   # pivot_longer(cols = 2:7,names_to = "metric",values_to = "score")

  })
    
  

  
  
#### store wt_real estate score to calculate investment score
  real_estate_score <- reactive({
    req(input$EnterREWeights)
    wt_real_estate_metrics <- real_estate_metrics %>% 
      mutate(wt_pop_change = n_pop_growth * input$PopChgWtInput,
             wt_employ_change = n_employ_change * input$EmployWtInput,
             wt_occ = n_occ_rate2021 * input$OccWtInput,
             wt_rent_change = n_rent_change * input$RentChgWtInput,
             wt_rent_income = n_rent_to_income * input$RentIncomeWtInput,
             wt_income_homeprice = n_income_to_homeprice * input$IncomeHomeWtInput) %>% 
      select(-starts_with("n_")) %>% 
      mutate(real_estate_score = rowSums(.[4:9])) %>% 
      mutate_if(is.numeric, round, digits = 4)
    
    return(wt_real_estate_metrics$real_estate_score)
  })
  
  output$RE_score_test <- renderPrint({
  real_estate_score()
  })
  
  ## criteria weight inputs
  weight_inputs <- reactive({
    c(input$REWtInput, input$LandlordWtInput,
                             input$ElectrictyWtInput,
                             input$CO2WtInput,
                             input$ClimateRiskWtInput,
                             input$HealthWtInput,
                             input$SolarIrrWtInput)  
  })

  # text to show weight sum
  output$wt_sum <- renderPrint({
    sum(weight_inputs())
  })

  
  # function to calculate weighted criteria
  calculate_wt_criteria <- function(data, weights = weight_inputs()){
    wt_criteria <- data %>% 
      mutate(wt_real_estate = real_estate_score * input$REWtInput,
                    wt_landlord_policy = landlord_score * input$LandlordWtInput,
                    wt_electricity = electricity_score * input$ElectrictyWtInput,
                    wt_co2 = co2_e_score * input$CO2WtInput,
                    wt_climate_risk = climate_risk_score * input$ClimateRiskWtInput,
                    wt_health_impacts = health_impact_score * input$HealthWtInput,
                    wt_solar_irr = financial_score * input$SolarIrrWtInput) %>% 
      select(!c(ends_with("_score"))) %>% 
      rename("Real Estate"="wt_real_estate",
             "Landlord Policy"="wt_landlord_policy",
             "Electricity Generation"="wt_electricity", 
             "CO2 Emissions Abated"="wt_co2",
             "Climate Risk Avoided"="wt_climate_risk",
             "Reduced Health Impacts"="wt_health_impacts", 
             "Solar Financials"="wt_solar_irr") 

    return(wt_criteria)
  }
  
  
  output$wt_table_test <- renderDataTable({
    req(input$EnterWeights)
    wt_real_estate_score <- real_estate_score()
    criteria_unweighted$real_estate_score <- real_estate_score()
    wt_data <- calculate_wt_criteria(criteria_unweighted) %>% 
      mutate("Investment Favorability Score" = apply(.[,4:10], 1, sum)) %>%
    select(!cbsa) %>% 
      unite("City, ST", city_msa:state, sep = ", ")
    return(wt_data)
  })
  

  
  ## output weighted criteria into bar chart
  output$wt_criteria_chart <- renderPlot({
    req(input$EnterWeights)
    wt_real_estate_score <- real_estate_score()
    criteria_unweighted$real_estate_score <- wt_real_estate_score
    
    wt_data <- calculate_wt_criteria(criteria_unweighted) %>% 
      pivot_longer(cols = 4:10, # selecting criteria cols
                   names_to = "criteria",
                   values_to = "wt_criteria_score")  %>% 
   unite("msa_state", city_msa:state, sep = ", ")
      
      ggplot(data = wt_data, aes(y = reorder(msa_state, wt_criteria_score, FUN=sum), x = wt_criteria_score)) + 
        geom_col(aes(fill = criteria)) +
        labs(x = "Total Score", y = " ", title = " ") +
        theme_bw() +
        guides(fill=guide_legend(title="Criteria (weighted)")) +
       scale_fill_manual(values = c("palevioletred1","orange",  "gold", "#44AA99", "dodgerblue4", "#BC80BD", "#6495ED"),
      breaks = c("Climate Risk Avoided", "CO2 Emissions Abated", "Electricity Generation", "Solar Financials", "Real Estate", "Landlord Policy", "Reduced Health Impacts")) +
        scale_x_continuous(limits = c(0, 1.0), expand = c(0,0), breaks=seq(0, 1.0, 0.1))
  })
  
output$RE_map <- renderPlot({
  wt_real_estate_score <- real_estate_score()
  cbsa_shp <- cbsa %>% 
    mutate(CBSAFP = as.factor(CBSAFP)) %>% 
    inner_join(wt_real_estate_score, by = c("CBSAFP" = "cbsa")) # merge msa data with shape files
  # map of U.S. with MSAs highlighted
  ggplot() +
    geom_polygon(data = states,
                 aes(x = long, y = lat,
                     group = group)) + 
    geom_sf (data = cbsa, fill = "lightgreen") +
    labs(title = "Metro Areas included in this analysis", x = "", y = "") +
    theme_void()
})

output$RE_tmap <- renderTmap({

  wt_real_estate_score <- real_estate_score()
  criteria_unweighted$real_estate_score <- wt_real_estate_score
  
  wt_data <- calculate_wt_criteria(criteria_unweighted) 

  cbsa_RE <- cbsa_geom %>% inner_join(wt_data)
  
  tm_shape(states_geom) +
    tm_fill("grey") +
    
    tm_shape(cbsa_RE) +
      tm_fill("Real Estate", palette = "BuGn") 
      
})

  
}

# Run the application 
shinyApp(ui = ui, server = server)




