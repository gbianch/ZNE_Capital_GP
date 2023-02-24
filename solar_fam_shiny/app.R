

library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib)
library(tidycensus)
library(RJSONIO)
library(googlesheets4)
library(tigris) # for shape files
options(tigris_use_cache = TRUE)

census_api_key("ebab921b3002df9b71881ad6c426f34281ce0e11", overwrite = "TRUE")

# census metro area shape files
cbsa_shp <- core_based_statistical_areas() 

real_estate_metrics2021 <- read_sheet("https://docs.google.com/spreadsheets/d/1lmTpSDwVANxdAg5sW87Q8l_gtig7M0EjPPJ6J5_7dwg/edit#gid=519968233")


ui <- fluidPage(
  # Application title
  titlePanel("Identifying the best areas for rooftop solar on multifamily housing"),
  navbarPage("Solar Family", #navbarPage allows us to create our tabs
             
             ### tab 1 - background
             tabPanel("Landing Page", 
                      #sidebarLayout(
                        #sidebarPanel(" ",
                          mainPanel(
                            h2("Background"),
                            h5("Sixth level title"),
                            plotOutput("map"),
                            h4("model flow chart") ### Add model image here
                            ) ### end mainPanel
                       # ) #end sidebar panel
                      # ) #end sidebar layour
             ), # ## END of tab 1
             
             
             ### tab2
             tabPanel("Coefficient Customization",
                      sidebarLayout(
                        sidebarPanel(" ",
                          # input for population size
                          textInput(inputId = "PopulationSizeMin",
                                    label = "Minimum Population Size",
                                    value = "300,000",
                                    width = "100px"), # end population input
                         
                           # help text under input box
                          helpText("Format example: 100,000"),
                          actionButton(inputId = "EnterPopulation", label = "Enter Population Size"),
                        ), # end sidebar panel
                        mainPanel(
                          h4("population min check"),
                          tableOutput(outputId = "pop_test_table")
                          
                          ) # end main panel
                      ) # end sidebar panel

             ), # end tabpanel
             tabPanel("Score Visualization"),
             tabPanel("Raw Data")

    ) ### end nav bar
) ### end fluidPage





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # min population input 
  population_size <- reactive({
    req(input$PopulationSizeMin)
    pop_data_min <- real_estate_metrics2021 %>% 
      filter(popestimate2021 >= input$PopulationSizeMin)
    
  })
  
  output$pop_test_table <- renderTable({
    input$population_size

  })
  
  weight_input <- reactive({ ### whatever is in reactive function()
    penguins %>%
      filter(criteria == input$criteria) # penguin species comes from inputId in radio buttons
  })
  
  census_table <- reactive({
    census_data %>%
      filter(species == input$penguin_species) %>%
      group_by(sex) %>%
      summarize(mean_flip = mean(flipper_length_mm),
                mean_mass = mean(body_mass_g))
  }) ### end penguin table
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)



