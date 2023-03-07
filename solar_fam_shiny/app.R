
library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib) # shiny theme
library(tidycensus)
library(googlesheets4)
library(tigris) # for shape files
options(tigris_use_cache = TRUE)
library(tmap) # interactive map
tmap_options(check.and.fix = TRUE)
library(sf) # map states and US
library(shinydashboard)



real_estate_metrics2021 <- read_sheet("https://docs.google.com/spreadsheets/d/1lmTpSDwVANxdAg5sW87Q8l_gtig7M0EjPPJ6J5_7dwg/edit#gid=519968233")
criteria_unweighted <- read_sheet("https://docs.google.com/spreadsheets/d/1yqjhJvXUcEiC3qiYWkKNlF6NNpYXWg15zZHwmwpS5Q0/edit#gid=1307699202", sheet = "FINAL_total_score") %>% 
  mutate(cbsa = as.character(cbsa))

#census_api_key("ebab921b3002df9b71881ad6c426f34281ce0e11", overwrite = "TRUE")

# tidycensus package metro area shape files
msa_shp <- core_based_statistical_areas() %>% 
  janitor::clean_names() %>% 
  select(!c(csafp, lsad, cbsafp)) #4269

states_4269 <- st_transform(state_laea, st_crs(msa_shp))

# state shape files from sf packages
# state_shp <- states(class = "sf", progress_bar = TRUE, shift_geo = TRUE) %>% 
#   st_transform(st_crs(., msa_shp))

# merge criteria table with shp data
unwt_criteria_shp <- criteria_unweighted %>% 
  merge(msa_shp, by.x = "cbsa", by.y = "geoid")

ui <- fluidPage(
  # Application title
  titlePanel("title"),
  navbarPage("solar fam",
             theme = bs_theme(bootswatch = "cerulean", primary = "#8aab57"),
             ### tab 1 - background
             tabPanel("Landing Page", 
                      mainPanel("Background Info",
                        h5("Sixth level title"),
                        #plotOutput("map"),
                        h4("model flow chart"), 
                        ### Add model image here
                        div(img(src = "model_no_weights.jpg",  width = "800", height = "400"), style="text-align: center;")
                      ) ### end mainPanel
             ), # ## END of tab 1
            
             ### tab2
             tabPanel("Coefficient Customization",
                    box(title = "Criteria Weights", width = 6, status = "primary",
                          fluidRow(column(2,
                                          ### inputs for weights
                                          numericInput(inputId = "REWtInput", label = "Real Estate",
                                                       value = 0,width = "100px", min = 0.0,max = 1.0,step = 0.05) # end RE weight input
                                          ), # end RE fcolumn
                                   column(2, numericInput(inputId = "LandlordWtInput", label = "Landlord Policy",
                                                       value = 0,
                                                       width = "100px",
                                                       min = 0.0,max = 1.0,step = 0.05) # end landlord weight input
                                          ), # end landlord fluid row
                                   column(2, numericInput(inputId = "ElectrictyWtInput",
                                                          label = "Electricity",
                                                          value = 0,width = "100px",
                                                          min = 0.0, max = 1.0, step = 0.05) # end electricity weight input
                                          )), #  fluid row 1
                          fluidRow(column(2, numericInput(inputId = "CO2WtInput",
                                                          label = "CO2 Emissions",value = 0,width = "100px",
                                                          min = 0.0, max = 1.0,step = 0.05) # end Co2 weight input
                                          ), # end Co2 fluid row
                                   column(2, numericInput(inputId = "ClimateRiskWtInput",
                                                          label = "Climate Risk",value = 0,width = "100px",
                                                          min = 0.0, max = 1.0, step = 0.05) # end climate risk weight input
                                          ), # end climate risk input
                                   column(2, numericInput(inputId = "HealthWtInput",
                                                             label = "Health Impacts",value = 0,
                                                             width = "100px",min = 0.0,max = 1.0, step = 0.05) # end health impact weight input
                                          )), # end health impacts input
                          fluidRow(column(4, numericInput(inputId = "SolarIrrWtInput",
                                                          label = "Solar IRR",
                                                          value = 0,
                                                          width = "100px",
                                                          min = 0.0, max = 1.0, step = 0.05) # end irr input
                                          )), # end IRR fluid row
                        fluidRow(column(2, actionButton(inputId = "EnterWeights", label = "Calculate")))
                          ), # end box
                  
                        mainPanel(
                          verbatimTextOutput(outputId = "wt_sum"),
                          dataTableOutput(outputId = "wt_table")
                          # tabsetPanel(
                          #   id = "tabset",
                          #   tabPanel("Barchart",)),
                          #   tabPanel("Map", "two"),
                          #   tabPanel("Table", "table"))
                          # ) #end tabset panel 
                        ) # tab 1end main panel
                      
             ), # end 2 tabpanel
             tabPanel("Map Visualization"
                      ), # end map viz tab Panel 
             
             tabPanel("Demographic Data",
                      textInput(inputId = "PopulationSizeMin",
                                label = "Minimum Population Size",
                                value = "300000",
                                width = "100px"), # end population input)
                      # help text under input box
                      helpText("Format example: 100000"),
                      actionButton(inputId = "EnterPopulation", label = "Enter Population Size")
             ) # end tab panel
  
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


  ########################################################################

### method 2 with observe and creating table   
  # Update reactive values when weights are changed
  
  # Render the weighted criteria table
  output$wt_table <- renderDataTable({
    req(input$EnterWeights)
    unwt_criteria_shp

  })
  ####################################################################

  ## output weighted criteria into bar chart
  output$wt_criteria_chart <- renderPlot({
    wt_data <- wt_criteria()
    req(input$EnterWeights)
        ggplot(data = wt_criteria(), aes(y = reorder(msa, total), x = wt_criteria_score)) + 
          geom_col(aes(fill = criteria)) +
          labs(x = "Total Score",y = " ", title = " ") +
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




