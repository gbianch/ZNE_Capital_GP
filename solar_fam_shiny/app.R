
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
                                                 width = "80px"), # end Co2 weight input
                                     
                                     numericInput(inputId = "ClimateRiskWtInput",
                                                  label = "Climate Risk Weight",
                                                  value = "0.07",
                                                  width = "80px"), # end climate risk weight input
                                     
                                     numericInput(inputId = "HealthWtInput",
                                                  label = "Health Impacts Weight",
                                                  value = "0.07",
                                                  width = "80px"), # end electricity weight input
                                     
                                     numericInput(inputId = "solarIrrWtInput",
                                                  label = "Solar IRR Weight",
                                                  value = "0.07",
                                                  width = "80px"), # end electricity weight input
                                     
                                      
                                     actionButton(inputId = "EnterWeights", label = "Calculate")
                                     
                        ), # end sidebar panel
                        mainPanel(
                          h4("RE weight input test"),
                          #dataTableOutput(outputId = "pop_test_table"),
                          textOutput(outputId = "wt_test"),
                         plotOutput(outputId = "wt_criteria_chart"),
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
  ClimateRisk_wt_input <- reactive({(input$ClimateRiskWtInput)})
  health_wt_input <- reactive({(input$HealthWtInput)})
  solarIRR_wt_input <- reactive({(input$solarIrrWtInput)})
  

  
## testing weight inputs
  output$wt_test<- renderPrint({
    req(input$EnterWeights)
    weight_sum <- sum(RE_wt_input(), landlord_wt_input(), electricity_wt_input(), co2_wt_input(),
                      ClimateRisk_wt_input(), health_wt_input(), solarIRR_wt_input())
    # require calculate button to be pressed before returning anything
    req(input$EnterWeights)
    # if weights don't equal 1 return error
    if (weight_sum != 1){
      paste0("Weights must sum to 1.")
    }
    # if weights equal 1, multiply them by corresponding scores
    else {
      weighted_criteria <- unwt_criteria_shp %>% 
        mutate(wt_RE = RE_wt_input()*real_estate_score,
               wt_landlord = landlord_wt_input()*landlord_score,
               wt_electricity = electricity_wt_input()*electricity_score,
               wt_co2 = co2_wt_input()*co2_score,
               wt_climate_risk = climate_risk_wt_input()*climate_risk_score,
               wt_health = health_wt_input()*health_impact_score,
               wt_solarIRR = solarIRR_wt_input()*financial_score,
               total = sum(cols(starts_with("wt_")))) %>% 
        # removed unweighted columns for barchart
        select(!c(ends_with("_score"))) %>% 
        pivot_longer(cols = starts_with("wt"), # selecting criteria cols
                     names_to = "criteria",
                     values_to = "wt_criteria_score") %>% 
        # filter(msa_state != "Portland, ME") %>%  # removing portland (negative number)
        group_by(cbsa)
    } # end else
    
  })

  ## output weighted criteria into bar chart
  output$wt_criteria_chart <- renderPlot({
    ggplot(data = weighted_criteria, aes(y = reorder(msa_state, total), x = criteria_score)) + 
      geom_col(aes(fill = criteria)) +
      labs(x = "Total Score",
           y = " ",
           title = " ") +
      theme_bw() +
      guides(fill=guide_legend(title="Criteria (weighted)")) +
      scale_fill_manual(values = c("palevioletred1", "orange",  "gold", "#44AA99", "dodgerblue4", "#6495ED", "#BC80BD"),
                        labels=c('Climate Risk', 'CO2 Emissions', 'Electricity', "Solar IRR", "Real Estate", "Health Impacts", "Landlord Policy")) +
      scale_x_continuous(limits = c(0, 1.0), expand = c(0,0), breaks=seq(0, 01.0, 0.1))
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



