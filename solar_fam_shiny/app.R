
library(shiny)
library(ggplot2)
library(tidyverse)
#library(bslib)
library(tidycensus)
library(googlesheets4)
library(tigris) # for shape files
options(tigris_use_cache = TRUE)
library(tmap) # interactive map
tmap_options(check.and.fix = TRUE)
library(sf) # map states and US


real_estate_metrics2021 <- read_sheet("https://docs.google.com/spreadsheets/d/1lmTpSDwVANxdAg5sW87Q8l_gtig7M0EjPPJ6J5_7dwg/edit#gid=519968233")
criteria_unweighted <- read_sheet("https://docs.google.com/spreadsheets/d/1yqjhJvXUcEiC3qiYWkKNlF6NNpYXWg15zZHwmwpS5Q0/edit#gid=1307699202", sheet = "FINAL_total_score") %>% 
  mutate(cbsa = as.character(cbsa))

#census_api_key("ebab921b3002df9b71881ad6c426f34281ce0e11", overwrite = "TRUE")

# tidycensus package metro area shape files
msa_shp <- core_based_statistical_areas() %>% 
  janitor::clean_names() %>% 
  select(!c(csafp, lsad, cbsafp))

states_4269 <- st_transform(state_laea, st_crs(msa_shp))

# state shape files from sf packages
# state_shp <- states(class = "sf", progress_bar = TRUE, shift_geo = TRUE) %>% 
#   st_transform(st_crs(., msa_shp))

# merge criteria table with shp data
unwt_criteria_shp <- criteria_unweighted %>% 
  merge(msa_shp, by.x = "cbsa", by.y = "geoid") %>% 
  st_sf()


ui <- fluidPage(
  # Application title43
  titlePanel("Identifying the best areas for rooftop solar on multifamily housing"),
  navbarPage("Solar Family", #navbarPage allows us to create our tabs
             
             ### tab 1 - background
             tabPanel("Landing Page", 
                      #sidebarLayout(
                      #sidebarPanel(" ",
                      mainPanel(
                        h2("Background Info"),
                        h5("Sixth level title"),
                        plotOutput("map"),
                        h4("model flow chart"), 
                        ### Add model image here
                        div(img(src = "model_no_weights.jpg", 
                                width = "800",
                                height = "400"), 
                            style="text-align: center;")
                      ) ### end mainPanel
                      # ) #end sidebar panel
                      # ) #end sidebar layour
             ), # ## END of tab 1
             
             
             ### tab2
             tabPanel("Coefficient Customization",
                      sidebarLayout(
                        sidebarPanel("Model Inputs",
                                     # input for population size
                                     textInput(inputId = "PopulationSizeMin",
                                               label = "Minimum Population Size",
                                               value = "300000",
                                               width = "80px"), # end population input
                                     
                                     # help text under input box
                                     helpText("Format example: 100000"),
                                     actionButton(inputId = "EnterPopulation", label = "Enter Population Size"),
                                     
                                     ### inputs for weights
                                     numericInput(inputId = "REWtInput",
                                               label = "Real Estate Weight",
                                               value = "0.22",
                                               width = "80px"), # end RE weight input
                                     
                                     numericInput(inputId = "LandlordWtInput",
                                                label = "Landlord Policy Weight",
                                                value = "0.00",
                                                width = "80px"), # end landlord weight input
                                     
                                     numericInput(inputId = "ElectrictyWtInput",
                                                label = "Electricity Weight",
                                                value = "0.07",
                                                width = "80px"), # end electricity weight input
                                     
                                     numericInput(inputId = "CO2WtInput",
                                                 label = "CO2 Emissions Weight",
                                                 value = "0.07",
                                                 width = "80px"), # end electricity weight input
                                     
                                     numericInput(inputId = "ClimateRiskWtInput",
                                                  label = "Climate Risk Weight",
                                                  value = "0.07",
                                                  width = "80px"), # end electricity weight input
                                     
                                      
                                     actionButton(inputId = "EnterWeights", label = "Enter Weights")
                                     
                        ), # end sidebar panel
                        mainPanel(
                          h4("RE weight input test"),
                          #dataTableOutput(outputId = "pop_test_table"),
                          textOutput(outputId = "wt_test"),
                          #tmapOutput(outputId = "unwt_criteria_tmap", width = "100%"),
                        
                          
                        ) # end main panel
                      ) # end sidebar panel
                      
             ), # end tabpanel
             tabPanel("Score Visualization"),
             tabPanel("Raw Data")
             
  ) ### end nav bar
) ### end fluidPage





# Define server logic 
server <- function(input, output) {
  
  # change min population input to numeric class
  pop_size_input <- reactive({ as.numeric(input$PopulationSizeMin)})
  
  # output data table based on minimum population size input
  output$pop_test_table <- renderDataTable({
    req(input$EnterPopulation) # require action button
    pop_size_list <- real_estate_metrics2021 %>% 
      filter(popestimate2021 > pop_size_input())
    
  })
  
  ## Weight inputs
  RE_wt_input <- reactive({(input$REWtInput)})
  landlord_wt_input <- reactive({(input$LandlordWtInput)})
  electricity_wt_input <- reactive({(input$ElectrictyWtInput)})
  co2_wt_input <- reactive({(input$CO2WtInput)})
  climate_risk_wt_input <- reactive({(input$ClimateRiskWtInput)})
  

  
## testing weight inputs
  output$wt_test<- renderPrint({
    weight_sum <- sum(RE_wt_input(), landlord_wt_input(), electricity_wt_input(), co2_wt_input(),
                      climate_risk_wt_input())
    req(input$EnterWeights)
    paste0(weight_sum)
    
    
  })

  
  output$unwt_criteria_tmap <- renderTmap({
    tmap_mode("view")
   # tm_shape(states(class = "sf")) + # distorted state shapes, don't use
    tm_shape(states_4269) +
      tm_fill(fill = "grey20") +
      tm_polygons("unwt_criteria_shp") +
      tm_fill(col = "real_estate_score", palette = "Blues") +
      tmap_options(check.and.fix = TRUE)
    # tm_polygons() +
    # tm_symbols(shape = tmapIcon, size = 0.15, border.lwd = NA) 
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



