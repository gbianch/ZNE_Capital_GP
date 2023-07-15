
library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib) # shiny theme
library(sf) # map states and US
library(here)
library(tmap)
library(tigris)
options(tigris_use_cache= TRUE)
library(tmaptools)
source(here("R/normalize.R"))
library(rsconnect) # to publish 
library(shinydashboard)
library(shinyWidgets)
library(tidycensus)
library(shinyBS)
library(shiny.router)
library(DT)
library(shinytest)


  
#### READ IN SHP DATA ----------------------------------------------
cbsa_geom <- core_based_statistical_areas(class="sf") %>% 
  mutate(cbsa = as.character(CBSAFP),
         GEOID = as.numeric(GEOID)) %>% 
  select(!c("CSAFP", "LSAD", "NAMELSAD", "MTFCC", "GEOID", "CBSAFP"))

states_geom <- tidycensus::state_laea   #read in the dataset
states_geom <- st_transform(states_geom, st_crs(cbsa_geom)) 
states_sf <- states()
states_sf <- states_sf %>% 
  filter(!STUSPS %in% c("PR", "VI", "MP", "GU", "AS")) %>% shift_geometry()  

#### READ IN Metric and criteria DATA ----------------------------------------------
real_estate_metrics <- read_csv(here("data/intermediate/n_real_estate_metrics.csv")) %>% 
  select(cbsa, anchor_city, msa, starts_with("n_")) %>% 
  mutate(cbsa = as.character(cbsa)) %>% 
  rename("n_employ_change" = n_avg_emp_change)

landlord_data <- read_csv(here("data/intermediate/landlord_data.csv")) %>% 
  mutate(
    security_deposit_limit_num = case_when(
      security_deposit_limit == "N" ~ 1.0, 
      security_deposit_limit == "1_month_rent" ~ 0.0,
      security_deposit_limit == "2_months_rent" ~ 0.5)) %>% 
  mutate(n_eviction_notice = 1- normalize(eviction_notice_days),
         n_security_deposit = security_deposit_limit_num,
         cbsa = as.character(cbsa)) %>% 
  select(cbsa, rent_control, n_eviction_notice, n_security_deposit) 

criteria_unweighted <- read_csv(here("data/intermediate/unwt_criteria_scores.csv")) %>% 
  mutate(cbsa = as.character(cbsa)) 

### create clean table for criteria map
unwt_criteria_clean <- criteria_unweighted %>%
  rename("Real Estate"="real_estate_score",
         "Landlord Policy"="landlord_score",
         "Electricity Generation"="electricity_score", 
         "CO2 Emissions Abated"="co2_e_score",
         "Climate Risk Avoided"="climate_risk_score",
         "Reduced Health Impacts"="health_impact_score", 
         "Solar Financials"="financial_score") %>% 
  select(1:10) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  unite("City, ST", city_msa:state, sep = ", ") 

# Define the criteria names and colors
criteria <- c("Climate Risk Avoided", "CO2 Emissions Abated", "Electricity Generation",
              "Solar Financials", "Real Estate", "Landlord Policy", "Reduced Health Impacts")

colors <- c("palevioletred1", "orange", "gold", "#44AA99", "dodgerblue4", "#BC80BD", "#6495ED")

# Create a data frame with the criteria and colors
criteria_colors <- data.frame(criteria = criteria, color = colors)


####### Informatin

abstract <- "As the renewable energy transition accelerates, housing, due to its high energy demand, 
can play a critical role in the clean energy shift. Specifically, multifamily housing provides a unique 
opportunity for solar photovoltaic (PV) system adoption, given the existing competing interests between 
landlords and tenants which has historically slowed this transition. Landlords are less incentivized to
install solar due to upfront costs, but ultimately tenants receive the benefits from solar installations 
in the form of reduced electricity bills. To address this transition gap, this project identified and ranked
Metropolitan Statistical Areas (MSAs) in the United States for ZNE Capital (the client) to acquire multifamily 
housing to install solar PV systems. Working with the client, the group identified seven criteria to determine 
favorable markets for rooftop solar PV on multifamily housing: landlord policy favorability, real estate market 
potential, CO2 abatement potential, electricity generation potential, solar installation internal rate of return, 
climate risk avoidance, and health costs associated with primary air pollutants. A total investment favorability score 
is calculated based on criteria importance assigned by the user.  Investment favorability scores were investigated for different 
preferences to demonstrate the robustness and generalizability of the framework. The data analysis and criteria calculations were 
conducted using RStudio, ultimately to provide reproducible code to be used for future projects. The results are presented in a ranked list and GIS map of MSAs based on the overall favorability score.
In addition, GIS maps of each criterion are included with the relative scores of each MSA to shed light on geographic trends. 
Future studies can utilize the reproducible code to inform decisions on where to invest in solar PV on multifamily housing anywhere in the United States by changing weights within the model depending on preferences."


#### start of shiny app----------------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel(div(h4("Identifying areas to invest in Rooftop Solar on Multifamily Housing", 
                 tags$img(src = "bren-leaf-logo.jpg", align="right", height="30px", width="30px"))),
             windowTitle="solar fam"),
  navbarPage("solar family",
             theme = bs_theme(bootswatch = "cerulean", primary = "#8aab57"),
             ### tab 1 - background
             tabPanel("Landing Page", icon=icon("home", lib="glyphicon"), id="home", 
                      mainPanel(
                        h5("How to navigate the shiny app"),
                        p("This web app was created to show how several different criteria can impact the
                          investment feasability of rooftop solar on multifamily housing across the country.
                          The model includes 7 criteria that factor into the investment favorability score, in which
                          stakeholders can adjust the weight of each based on the relative importance the criteria given their priorities when
                          investing in multifamily housing. The default weights are the weights used in the client scenario."),
                        br(),
                        p("To start, click on the", strong("Use the Model"), "tab, and it will prompt you through each step. After each step,
                          be sure to click the enter button to initiate analysis:"),
                        tags$ul(
                          tags$li(strong("Step 1 "), "allows the user to input weights for the 
                          metrics in the real estate criteria. User should enter higher weights for metrics they deem more signifcant
                          for the real estate criteria."),
                          tags$li(strong("Step 2."), "allows the user to input landlord policy preferences. The user can select to include
                          or remove areas in states that allow rent control. In addition, the user can input weights for the two metrics within
                          the landlord policy criteria."),
                          tags$li(strong("Step 3"), "allows the user to input weights for the 7 criteria that make up the investment
                          favorability score")),
                 
                        p(strong("Note:"), "the weights for each step are set at the default values used in the client scenario. Findings for
                          the equity-centric scenario can be found in the", strong("Project"), "tab"),
                        
                        
                        p(HTML("This analysis and interactive web-app is limited to 29 metropolitan statisticals areas, but the
                          methodology can be replicated for to analyze additional metro areas in future studies. For more information regarding the project 
                          background and metholodology, please refer to the <a href='https://bren.ucsb.edu/projects/constructing-model-identify-markets-rooftop-solar-multifamily-housing'> Bren Project Directory</a> 
                          for the technical report and the <a href='https://github.com/gbianch/ZNE_Capital_GP'> GitHub repository. </a>")),
                        
                        
                        p("Weights for each criterion can be adjusted in the model based on user priorities. The client’s weights reflect relative importance in ZNE Capital’s decision making for their business model. An example equity-centric scenario was included to represent non-profit or government stakeholders that maximize positive social and environmental impact by prioritizing CO2 abatement potential and health impacts."),
                        h5("Model Flow Chart"),  ### Add model image here
                        div(class = "center-image", img(src = "model_no_weights.jpg", width = "1000px", height = "500px")),
                        p(strong("Figure 1."), "Visualization of methodology to calculate investment favorability score for each metropolitan area. The investment favorability score of rooftop solar on apartment buildings was calculated as a weighted additive total of the seven criteria scores.")
                        
                        
                      ) ### end mainPanel
             ), # ## END of tab 1
             #### beginning of tab 2 ----------------------------------------------
             tabPanel(title="Use the Model",  icon=icon("solar-panel"),
                      tabsetPanel(
                        tabPanel("Step 1: Real Estate Metrics",
                                 sidebarLayout(
                                   sidebarPanel(h5("Input Metric Weights"),
                                                helpText("Default values are consistent with weights used in the project."),
                                     # inputs for real estate metrics
                                     numericInput(inputId = "PopChgWtInput", label = "Population Change",
                                                  value = 0.35,width = "175px", min = 0.0,max = 1.0,step = 0.05), # end RE weight input
                                     
                                     numericInput(inputId = "EmployWtInput", label = "Employment Change",
                                                  value = 0.20, width = "175px", min = 0.0,max = 1.0,step = 0.05),
                                     
                                     numericInput(inputId = "RentChgWtInput", label = "Rent Change",
                                                  value = 0.10,width = "175px", min = 0.0, max = 1.0, step = 0.05),
                                     
                                     numericInput(inputId = "OccWtInput", label = "Occupancy Rate",
                                                  value = 0.15,width = "175px", min = 0.0, max = 1.0,step = 0.05),# end Co2 weight input
                                     
                                     numericInput(inputId = "RentIncomeWtInput", label = "Rent to Income",
                                                  value = 0.10,width = "175px", min = 0.0, max = 1.0, step = 0.05), # end climate risk weight input
                                     
                                     numericInput(inputId = "IncomeHomeWtInput", label = "Income to Home Price",value = 0.10,
                                                  width = "175px",min = 0.0,max = 1.0, step = 0.05), # end health impact weight input
                                     
                                     actionButton(inputId = "EnterREWeights", label = "Enter Weights"),
                                     helpText("Click button to generate table of the weighted real estate metrics. Weights must sum to 1.")), # end sidebar panel
                                   
                                   mainPanel(br(),
                                             p("The real estate metrics were included to indicate economic growth and housing demand in 
                                               each city. Higher weights represent greater importance for the metric when understanding real estate trends. Metrics have been normalized and/or
                                               inverted so each metric adds to the investment favorability score.
                                               The rent to income metric represents housing affordability"),
                                             tmapOutput("real_estate_map"),
                                             br(),
                                             br(),
                                             p("Below is a table of the weighted real estate metrics. Values of '1' indicate the city with the highest (most favorable)
                                               conditions for a given metric based on the 29 cities included in the analysis. Values of '0' mean that city scored the lowest 
                                               within a given metric."),
                                             br(),
                                     dataTableOutput("RE_wt_table")
                                     )
                                 )), # end step 1 tab panel
                        tabPanel("Step 2: Landlord Inputs",
                                 sidebarLayout(
                                   sidebarPanel(
                                     switchInput(inputId = "RentControl",
                                                 label= "Include areas with Rent Control",
                                                 value = FALSE,
                                                 handleWidth = "150px",
                                                 labelWidth = "300px",
                                                 onLabel = "YES",
                                                 offLabel= "NO"),
                                     numericInput(inputId = "EvNoWtInput", label = "Eviction Timeliness",
                                                  value = 0.80,width = "175px", min = 0.0,max = 1.0,step = 0.05), # end RE weight input
                                     
                                     numericInput(inputId = "SeDepWtInput", label = "Security Deposit Limit",
                                                  value = 0.20, width = "175px", min = 0.0,max = 1.0,step = 0.05)),
                                   
                                   mainPanel(h5("Criteria Significance"), 
                                             p("The landlord criteria can be important for staleholders working with private investors, as it ensures profit from the real estate properties. For this reason, states with rent control may not
                                               attract real estate investors. However, there is the option to include cities with rent control. In this 
                                               analysis, the only city within a rent-controlled state was Portland, Maine."),
                                             br(),
                                             h5("Weights"),
                                             p("The rent control switch allows the user to include or remove cities in rent controlled states.
                                               When the switch reads 'YES', it means the analysis includes areas with rent control. The current weights were the 
                                               default weights used in the project.The security deposit metric represents the maximum allowable deposit for a state. States
                                               with no limit are more favorable, followed by two months rent, and one month of rent. The eviction
                                               notice metric represents the number of days allowed to providee an eviction to for after failing to pay rent."),
                                            p("Tenant protection laws vary by state. For the client scenario, states with landlord-friendly policies received higher 
                                              landlord criterion scores when calculating each MSA's investment scores."))
                                             
                                             
                                 )), # end step 2 tab panel
                        ###### start of criteria weightst tab -----------------------------------------
                        tabPanel("Step 3. Criteria Weights", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     h5("Weight Significance"),
                                     p("The weights entered for each criteria represent the importance of that criteria to invest in rooftop solar on multifamily housing. 
                                       Higher weight values indicate greater importance of the criteria to stakeholder when investing in multifamily housing and adding rooftop solar. A value of zero
                                       means the criteria is not included in the investment favorability score."),
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
                                    # actionButton(inputId = "EnterWeights", "Run Analysis", style="color: #fff; background-color: #8aab57; border-color: #2e6da4", onclick = "document.getElementById('EnterWeights').style.color = 'orange';"),
                                     actionButton(inputId = "EnterWeights", label = "Enter Weights")), # end sidebar panel
                                   
                                   mainPanel( 
                                     h5("Explore Criteria Spatially"),
                                     verbatimTextOutput(outputId = "wt_sum_test"),
                                     radioButtons(inputId = "radio_criteria", label = h5("Select criteria"),
                                                  choices = list("Climate Risk Avoided", "CO2 Emissions Abated", "Electricity Generation", "Solar Financials", "Real Estate", "Landlord Policy", "Reduced Health Impacts")),
                                  
                                     plotOutput(outputId = "criteria_layers"))) # end sidebar layout
                        ), # end tab panel
                        tabPanel("Results", icon=icon("globe"),
                                 sidebarLayout(
                                   sidebarPanel(h5("Interpreting Results"),
                                                p("The bar graph below shows the ranked areas from highest to lowest investment favorability score. The various colors
                                       in the graph represents different criteria to visualize how much each  contributes to the total investment favorability score."),
                                       dataTableOutput("ranked_table")
                                       ),
                                 
                                 mainPanel(
                                       h5("Ranked metropolitan areas based investment favorability for rooftop solar on multifamily housing"),
                                       plotOutput(outputId = "wt_criteria_chart"),
                                       tmapOutput("ranked_map")
                                       ))),
                        
                        tabPanel("Download Results", icon=icon("circle-arrow-down"),
                                   mainPanel(br(),
                                     dataTableOutput(outputId = "wt_table"))
                                   )
                        
                        
                      ) # end tabset panel
             ), # end tab panel
#### aAbout the project tab -------------------------------------------------------------
             tabPanel("The Project", icon = icon("info-circle"),
                      tabsetPanel(
                        tabPanel("Abstract",
                                 p(abstract)), # end abstract tab
                        tabPanel("Importance & Objectives",
                                 br(),
                                 p("The main objective of this project was to develop a model that includes seven criteria to identify U.S. MSAs ideal for installing rooftop solar PV systems on multifamily 
                                  housing complexes. This model was designed to be flexible enough to capture the perspectives and desires of the various stakeholders for such investments, including real 
                                  estate companies interested in solar PV, solar installation nonprofits, and state governments, which can express their priorities through weights assigned for each of the 
                                  seven criteria. Each criterion consists of indicator metrics calculated from data. Each criterion is multiplied by its respective user-assigned weight to determine the investment 
                                  favorability score. Once created, the model was employed to identify MSAs in the United States where the acquisition of multifamily housing complexes and installation of rooftop solar
                                  PV was most favorable for the client, Zero Net Energy (ZNE) Capital. In addition to the client scenario, an example equity-centered scenario was calculated using this model by adjusting
                                  the seven criteria weights, giving higher weighting to health impacts and CO2 emissions avoided. These weights can be adjusted based on the priorities of the model user.")),
                        tabPanel("Findings",
                                 sidebarLayout(
                                   sidebarPanel(), 
                                   mainPanel("Two Scenarios",
                                     p("This project assessed two stakeholder scenarios, client and equity centered. The differences between the scenarios are observed in the criteria weights. The client scenario
                                   prioritized climate risk and real estate criterion using higher weight values. Climate risk was given the greatest weight to ensure long-term investments were not in areas with high potential of extreme climate events 
                                   such as hurricanes or flooding. The climate risk weight contributed the most to investment favorability in the client scenario, therefore the most favorable areas have the lowest climate risk.
                                   The real estate criteria was priortized in the client scenario to ensure property investments are in areas of economic growth. The equity-centered scenario excluded the landlord criteria and included the
                                   a health impacts criteria. The more even distribution of weights represents stakeholders looking to provide affordable housing in areas that have the greatest potential to reduce air pollutants.
                                   "),
                                   plotOutput(outputId = "equity_map")))
                                
                                 
                        ), # end about the projct tab) 
            tabPanel("The Team", icon = icon("users", lib="font-awesome")))),

                      
                      
                               
                                
#### aAbout the project tab -------------------------------------------------------------            
              tabPanel("The Data", icon=icon("circle-info", lib="font-awesome"),
                      tabsetPanel(
                        tabPanel("Real Estate",
                                 br(),
                                 p("The metrics included in this criteria were population growth, employment growth, average annual occupancy, annual rent change, the ratios of median annual rent to median income, and median income to median home price. The data for each metric was gathered and normalized individually before being weighted and used to calculate the Real Estate score."),
                                 h5("Population Growth"),
                                 p("Population growth in a MSA is a commonly used real estate metric to gauge the demand for housing. The National Association of Realtors (NAR) identifies population growth as a key factor in determining a region's real estate market potential and a strong indicator of future demand for housing (Tracey, 2022). The U.S. Census Bureau provides accurate demographic data at the MSA level, making it a reliable source for population estimates. To quantify the change in MSA populations, the group utilized the annual population estimates provided by the U.S. Census Bureau between April 1, 2020, and July 1, 2021. Additionally, the group excluded MSAs with a population growth rate below 0.5% to streamline the analysis."),
                                 
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
                                 p("Occupancy rate is the ratio of rented units to the total available units for rent in an MSA. Yardi Matrix is a robust research platform that gives users access to property-level information, including occupancy rates, for multifamily properties in the United States (Yardi Systems, n.d.). Students used Yardi Matrix to collect average occupancy rates at the city level, identifying an anchor city based on the largest population size for MSAs comprising more than one city.  For this metric, lower occupancy rates in an MSA signify lower demand for rental units.")
                        ),# end demographics tab panel
                        ##### start climate risk tab-----------------------------------------
                        tabPanel("Climate Risk",
                                 h5("Climate Risk Avoidance Overview"),
                                 HTML("Climate risk avoidance was incorporated into the model using the overall risk score from the <a href='https://hazards.fema.gov/nri/map'>National Risk Index</a> (NRI) created by the Federal Emergency Management Agency (FEMA). The overall risk score is a representation of risk relative to the expected annual loss, social vulnerability, and community resilience of all other counties (see section 5.2). Areas with a high climate risk score according to the NRI were ranked lower in the investment favorability score."),
                                 h5("National Risk Index Terms and Definitions"),
                                 p(strong("Community resilience:"),
                                   "FEMA draws this from the National Institute of Standards and Technology which defines community resilience as “the ability of a community to prepare for anticipated natural hazards, adapt to changing conditions, and withstand and recover rapidly from disruptions."),
                                 p(strong("Expected annual loss:"),
                                   "This is the average economic loss from natural hazards per year. Expected annual loss is computed for the different hazard types since some hazards impact buildings, some impact agriculture, etc. The expected annual loss for drought exclusively quantifies harm to agriculture."),
                                 p(strong("Social vulnerability:"),"This metric is calculated using 29 different socioeconomic variables, including median gross rent for renter-occupied housing units, per capita income, percentage of population over 25 with <12 years of education, percentage of population speaking English as second language (with limited English proficiency), and data on racial demographics."),
                                 h5("Risk Index Calculation"),
                                 p("FEMA’s overall risk score was calculated using the “Generalized National Risk Index Risk Equation” (FEMA, 2021, pg. 35), which multiplies the expected annual loss by social vulnerability (a risk-compounding factor) and inverse community resilience (a risk-reducing factor). These calculations were done on both county and census tract levels; county-level data was used in this study because it was closer in scale to the MSAs used for other model criteria. Each component of the risk score was calculated relative to other counties (Federal Emergency Management Agency, 2021). For detailed information on FEMA’s source data, see Appendix G. A low overall risk score is more favorable since it indicates a low threat of damage from natural disasters and a high ability of a community to recover if a natural disaster occurs. The overall risk score was subtracted from 1 to compute the climate risk avoidance score. Higher climate risk avoidance scores represent lower climate risk and are preferred to add to the investment favorability score.")
                        ), # end climate risk tab 
                        
                        tabPanel("REopt Tool",
                                 h5("REopt Overview"),
                                 HTML("The <a href='https://reopt.nrel.gov/tool'>REopt Tool</a>  is a publicly available, open-source web tool created by the National Renewable Energy Laboratory (NREL) that was used in this project to analyze the solar electricity generation, internal rate of return (IRR), CO2 abatement potential, and human health impacts of installing rooftop solar on a standard apartment building (4 floors, 33,740 square feet). REopt was chosen for this analysis because by modeling rooftop solar energy generation and demand for an apartment building, it was able to model cohesive metrics across economic, environmental, and social criteria. Furthermore, REopt drew from other reliable national databases, including PVWatts,  EPA AVERT, and OpenEI. All input values except location, electricity rate, and net metering system capacity were held constant in order to compare MSAs accurately. For MSAs that contained multiple cities, an “anchor city” was selected based on population size; a list of anchor cities can be found in the appendix. The REopt model calculated all metrics using both the optimal recommended solar installation and a business-as-usual (no solar) scenario to provide a baseline for comparison.")
                        ),
                        tabPanel("References"))
             ) # end tab panel,
             
  ) ### end nav bar
  
) ### end fluidPage

# SERVER##########################################################################
# Define server logic 
server <- function(input, output) {

  
  ###### real estate metric weight inputs and outputs ---------------------------
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
     validate(need(try(sum(REweight_inputs()) == 1),
                   sprintf("Weights must sum to 1. Current sum is %.2f", sum(REweight_inputs()))))
              
     wt_real_estate_metrics <- real_estate_metrics %>% 
       mutate(wt_pop_change = n_pop_growth* input$PopChgWtInput,
              wt_employ_change = n_employ_change* input$EmployWtInput,
              wt_occ = n_occ_rate2021* input$OccWtInput,
              wt_rent_change = n_rent_change* input$RentChgWtInput,
              wt_rent_income = n_rent_to_income* input$RentIncomeWtInput,
              wt_income_homeprice = n_income_to_homeprice* input$IncomeHomeWtInput) %>% 
       select(!starts_with("n_")) %>% 
        mutate(real_estate_score = apply(.[,4:9], 1, sum)) %>% 
        mutate_if(is.numeric, round, digits = 2) %>% 
        select(!c(msa, anchor_city)) %>% 
        rename("Population Change"="wt_pop_change",
               "Employment Change"="wt_employ_change",
               "Occupancy Rate"="wt_occ", 
               "Rent Change"="wt_rent_change",
               "Rent to Income"="wt_rent_income",
               "Income to Homeprice"="wt_income_homeprice",
               "Real Estate Score") %>% 
       select(!cbsa) %>% 
       DT::datatable(.,
                     options = list(
                       buttons = c('pdf', 'excel', 'csv'),
                       dom = 'Bt',
                       scrollX= FALSE,
                       searching=FALSE,
                       paging = FALSE),
                     rownames = FALSE,
                     filter = "none",
                     extensions = "Buttons")
       
   })
       
  
  
  # store wt_real estate score to calculate investment score
  real_estate_score <- reactive({
    wt_real_estate_metrics <- real_estate_metrics %>% 
      mutate(wt_pop_change = n_pop_growth * input$PopChgWtInput,
             wt_employ_change = n_employ_change * input$EmployWtInput,
             wt_occ = n_occ_rate2021 * input$OccWtInput,
             wt_rent_change = n_rent_change * input$RentChgWtInput,
             wt_rent_income = n_rent_to_income * input$RentIncomeWtInput,
             wt_income_homeprice = n_income_to_homeprice * input$IncomeHomeWtInput) %>% 
      select(-starts_with("n_")) %>% 
      mutate(real_estate_score = rowSums(.[4:9])) %>% 
      mutate_if(is.numeric, round, digits = 2)
    
    return(wt_real_estate_metrics$real_estate_score)
  })
  
  output$real_estate_map <- renderTmap({
    validate(need(try(sum(REweight_inputs()) == 1),
                  sprintf("Weights must sum to 1. Current sum is %.2f", sum(REweight_inputs()))))
    
    wt_REmetrics <- real_estate_metrics %>% 
      mutate(wt_pop_change = n_pop_growth* input$PopChgWtInput,
             wt_employ_change = n_employ_change* input$EmployWtInput,
             wt_occ = n_occ_rate2021* input$OccWtInput,
             wt_rent_change = n_rent_change* input$RentChgWtInput,
             wt_rent_income = n_rent_to_income* input$RentIncomeWtInput,
             wt_income_homeprice = n_income_to_homeprice* input$IncomeHomeWtInput) %>% 
      select(!starts_with("n_")) %>% 
      mutate(real_estate_score = apply(.[,4:9], 1, sum)) %>% 
      mutate_if(is.numeric, round, digits = 2) %>% 
      select(!c(msa, anchor_city)) %>% 
      rename("Population Change"="wt_pop_change",
             "Employment Change"="wt_employ_change",
             "Occupancy Rate"="wt_occ", 
             "Rent Change"="wt_rent_change",
             "Rent to Income"="wt_rent_income",
             "Income to Homeprice"="wt_income_homeprice")
      
    REmetric_sf <- cbsa_geom %>% inner_join(wt_REmetrics)
    
    tm_basemap(NULL) +
      tm_shape(states_sf) +
      tm_polygons("grey") +
      tm_shape(REmetric_sf, name="Rent Change") +
      tm_polygons("Rent Change", palette="Purples") +
      tmap_mode("view")
    
  })
  
 
  
  ###### landlord metric weight inputs and outputs ---------------------------
  testswitch <- reactive({ input$RentControl})
  
  rent_control_cbsas <- reactive({
    if (testswitch() == "TRUE"){
      rent_control_cbsa <- landlord_data 
    }   
    else{rent_control_cbsa <- landlord_data %>% 
      filter(rent_control == "N")
    }
    return(rent_control_cbsa$cbsa)
  })
#-----------------------------

  LLweight_inputs <- reactive({
    c(input$SeDepWtInput, input$EvNoWtInput)

  })
  
  landlord_score <- reactive({
    wt_landlord_metrics <- landlord_data %>% 
      mutate(wt_security_deposit= n_security_deposit * input$SeDepWtInput,
             wt_eviction_notice = n_eviction_notice * input$EvNoWtInput) %>% 
      select(-starts_with("n_")) %>% 
      mutate(landlord_score = apply(.[,3:4], 1, sum)) %>% 
      mutate_if(is.numeric, round, digits = 2) %>% 
      arrange(cbsa)
    
    return(wt_landlord_metrics$landlord_score)
  })
  
  

  ####### criteria weight inputs ------------------------------------------
  weight_inputs <- reactive({
    c(input$REWtInput, 
      input$LandlordWtInput,
      input$ElectrictyWtInput,
      input$CO2WtInput,
      input$ClimateRiskWtInput,
      input$HealthWtInput,
      input$SolarIrrWtInput)  
  })
  


  
  output$wt_sum_test <- renderText({
    validate(need(sum(weight_inputs()) == 1,"Weights must sum to 1."),
             need(sum(REweight_inputs()) == 1, "Real Estate criteria weights must sum to 1."),
             need(sum(LLweight_inputs()) == 1,"Landlord criteria weights must sum to 1.")
    )
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
             "Solar Financials"="wt_solar_irr") %>% 
      mutate_if(is.numeric, round, digits = 2)

    return(wt_criteria)
  }
  
  ### output for weighted criteria and total scores -------------------------
  filtered_criteria_scores <- reactive({
    # return error if weights don't sum to 1
    validate(need(sum(weight_inputs()) == 1,"Weights must sum to 1."),
             need(sum(REweight_inputs()) == 1, "Real Estate criteria weights must sum to 1."),
             need(sum(LLweight_inputs()) == 1,"Landlord criteria weights must sum to 1."))
    
    # calculate weights scores if sum of weights is 1
    wt_real_estate_score <- real_estate_score() 
    wt_landlord_score <- landlord_score()
    rc_cbsas <- rent_control_cbsas() 
    
    criteria_unweighted$real_estate_score <- wt_real_estate_score
    criteria_unweighted$landlord_score <- wt_landlord_score
    
    wt_data <- calculate_wt_criteria(criteria_unweighted) %>% 
      filter(cbsa %in% rc_cbsas) %>% 
      mutate("Investment Favorability Score" = apply(.[,4:10], 1, sum),
             cbsa= as.character(cbsa)) %>% 
      mutate_if(is.numeric, round, digits = 2)

    return(wt_data)
  })
  
  output$ranked_table <- renderDataTable({
    filtered_criteria_scores() %>% 
      unite("City, ST", city_msa:state, sep = ", ") %>% 
      select("City, ST", "Investment Favorability Score") %>% 
      DT::datatable(.,
                  options = list(
                    scrollX= FALSE,
                    searching=FALSE,
                    paging = FALSE),
                  rownames = FALSE,
                  filter = "none",
                  width=500)
    })
  
    
  output$wt_table <- renderDataTable({
    weights <- c("WEIGHT INPUTS", weight_inputs(), NA)
    filtered_criteria_scores() %>% 
      select(!cbsa) %>% 
      unite("City, ST", city_msa:state, sep = ", ") %>% 
      arrange("Investment Favorability Score") %>% 
      rbind(., weights) %>% 
      DT::datatable(.,
                  options = list(
                    buttons = c('pdf', 'excel', 'csv'),
                    dom = 'Bt',
                    scrollX= FALSE,
                    searching=FALSE,
                    paging = FALSE),
                  rownames = FALSE,
                  filter = "none",
                  extensions = "Buttons")
                  
    })
  
  
  ## output weighted criteria into bar chart
  output$wt_criteria_chart <- renderPlot({
    
     wt_data <- filtered_criteria_scores() %>% 
       pivot_longer(cols = 4:10, # selecting criteria cols
                    names_to = "criteria",
                    values_to = "wt_criteria_score") %>% 
       unite("msa_state", city_msa:state, sep = ", ") %>% 
       select(!cbsa) 
    
    ggplot(data = wt_data, aes(y = reorder(msa_state, wt_criteria_score, FUN=sum), x = wt_criteria_score)) + 
      geom_col(aes(fill = criteria)) +
      labs(x = "Investment Favorability Score", y = " ", title = " ") +
      theme_bw() +
      guides(fill=guide_legend(title="Criteria (weighted)")) +
      scale_fill_manual(values = c("palevioletred1","orange",  "gold", "#44AA99", "dodgerblue4", "#BC80BD", "#6495ED"),
                        breaks = c("Climate Risk Avoided", "CO2 Emissions Abated", "Electricity Generation", "Solar Financials", "Real Estate", "Landlord Policy", "Reduced Health Impacts")) +
      scale_x_continuous(limits = c(0, 1.0), expand = c(0,0), breaks=seq(0, 1.0, 0.1))
  })
  
  map_criteriaInput <- reactive({
    unwt_criteria <- unwt_criteria_clean %>% 
      pivot_longer(cols = 3:9, # selecting criteria cols
                   names_to = "criteria",
                   values_to = "unwt_criteria_score") 
    
    unwt_data_shp <- cbsa_geom %>% inner_join(unwt_criteria) %>% 
      filter(criteria %in% input$radio_criteria)
    
    return(unwt_data_shp)
        
    })
    
  output$ranked_map <- renderTmap({
    wt_scores <- filtered_criteria_scores() %>% 
      pivot_longer(cols = 4:10, # selecting criteria cols
                   names_to = "criteria",
                   values_to = "wt_criteria_score")
    
    wt_data_shp <- cbsa_geom %>% inner_join(wt_scores)
    
    tm_basemap(NULL) +
    tm_shape(states_sf) +
      tm_polygons("grey") +
      tm_shape(wt_data_shp) +
      tm_polygons("Investment Favorability Score", palette="Purples") +
      tmap_mode("view")
    
  })
  
  output$criteria_layers <- renderPlot({
    
    unwt_data <- map_criteriaInput()
    
   c_color <- criteria_colors$color[criteria_colors$criteria == input$radio_criteria]
   
    ggplot() +
      geom_sf(data = states_sf, fill = "grey") +
      geom_sf(data = unwt_data, aes(fill=unwt_criteria_score)) +
      theme_void() +
      scale_fill_gradient(low = "white", high = c_color, aes(fill= input$radio_criteria)) 
    
  })
  
  output$equity_map <- renderPlot({
    equity_score <- criteria_unweighted %>% select(cbsa, city_msa, state, equity_scenario_score)
    data_shp <- cbsa_geom %>% inner_join(equity_score) 
    ggplot() +
      geom_sf(data = states_sf, fill = "grey") +
      geom_sf(data = data_shp, aes(fill=equity_scenario_score), color = "white") +
      theme_void() +
      scale_fill_gradient(low = "white",high = "green3") +
      labs(fill = "Equity Scenario Score")
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




