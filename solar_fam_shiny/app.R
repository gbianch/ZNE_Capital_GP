

library(shiny)
library(ggplot2)
library(tidyverse)
#library(bslib)
library(tidycensus)
library(googlesheets4)
library(tigris) # for shape files
options(tigris_use_cache = TRUE)

#census_api_key("ebab921b3002df9b71881ad6c426f34281ce0e11", overwrite = "TRUE")

# census metro area shape files
cbsa_shp <- core_based_statistical_areas() 

real_estate_metrics2021 <- read_sheet("https://docs.google.com/spreadsheets/d/1lmTpSDwVANxdAg5sW87Q8l_gtig7M0EjPPJ6J5_7dwg/edit#gid=519968233")
criteria_unweighted <- read_sheet("https://docs.google.com/spreadsheets/d/1yqjhJvXUcEiC3qiYWkKNlF6NNpYXWg15zZHwmwpS5Q0/edit#gid=1307699202", sheet = "FINAL_total_score")

model_img <- img(src = "model_no_weights.jpg",
                 alt = "Model Visualization",
                 width="500%")
  
ui <- fluidPage(
  # Application title43
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
                        sidebarPanel(" ",
                          # input for population size
                          textInput(inputId = "PopulationSizeMin",
                                    label = "Minimum Population Size",
                                    value = "300000",
                                    width = "100px"), # end population input
                         
                           # help text under input box
                          helpText("Format example: 100000"),
                          actionButton(inputId = "EnterPopulation", label = "Enter Population Size"),
                        ), # end sidebar panel
                        mainPanel(
                          h4("population min check"),
                          dataTableOutput(outputId = "pop_test_table")
                          
                          ) # end main panel
                      ) # end sidebar panel

             ), # end tabpanel
             tabPanel("Score Visualization"),
             tabPanel("Raw Data")

    ) ### end nav bar
) ### end fluidPage





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # change min population input to numeric class
  pop_size_input <- reactive({ as.numeric(input$PopulationSizeMin)})
  
# output data table based on minimum population size input
  output$pop_test_table <- renderDataTable({
    req(input$EnterPopulation) # require action button
    pop_size_list <- real_estate_metrics2021 %>% 
      filter(popestimate2021 > pop_size_input())

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



