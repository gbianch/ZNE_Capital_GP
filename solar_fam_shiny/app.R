

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
popURL <-"http://api.census.gov/data/2021/acs/acs1?key=ebab921b3002df9b71881ad6c426f34281ce0e11"
#"https://api.census.gov/data/2021/acs/acs1?get=NAME,B01001_001E&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:35620&key=ebab921b3002df9b71881ad6c426f34281ce0e11" 
ljson <- fromJSON(popURL)

pop_est <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area",
                         product = "housing")

msa_flows <- get_flows(geography = "metropolitan statistical area/micropolitan statistical area",
                       year = 2021)

# reads in metro areas
census_data <- get_acs(geography = "cbsa",  year = 2021, 
                       variables = c(median_income = "B19013_001E", # median household income
                                     total_labor_force = "B23025_003E", # total civilian labor force
                                     employed_labor_force = "B23025_004E"),   # employed labor force
                       output = "wide",
                       keep_geo_vars = TRUE) 

# census metro area shape files
cbsa_shp <- core_based_statistical_areas() 

real_estate_inputs <- read_sheet("https://docs.google.com/spreadsheets/d/1lmTpSDwVANxdAg5sW87Q8l_gtig7M0EjPPJ6J5_7dwg/edit#gid=519968233")


ui <- fluidPage(
  # Application title
  titlePanel("Identifying the best areas for rooftop solar on multifamily housing"),
  navbarPage("Solar Family", #navbarPage allows us to create our tabs
             tabPanel("Landing Page", #this is how we add tabs.
                      sidebarLayout(
                        sidebarPanel("WIDGETS",
                                     helpText("Create demographic maps with information from the 2010 US Census."),
                                     selectInput("var", 
                                                 label = "Choose a variable to display",
                                                 choices = c("Population Growth", "Employment Growth", "Rent Change"),
                                                 selected = "Population Growth"),
               
                        ), #End sidebarPanel widgets
               
                        mainPanel(h2("Background"),
                                  h5("Sixth level title"),
                                  "put my graph here!",
                                  plotOutput("map"),
                                  tableOutput(outputId = "census_table"),
                                  
                        ), ### end mainPanel
                        
                       ) #end sidebar (tab1) layout
             ), # end tabpanel thing 1
             tabPanel("Metric Data Exploration",
             sidebarPanel(" ",
                          radioButtons(inputId = "criteria_select",
                                       label = "Select a criteria to map",
                                       choices = c("Real Estate", "Landlord Policy", "Solar IRR", "Climate Risk", "Health Impacts", "Electricity", "CO2 Emissions")), # left value is displayed
                          
             ), # end side bar panel
             ), # end tabpanel
             tabPanel("Calculating Scores"),
             tabPanel("Mapping Results"),

    ) ### end nav bar
) ### end fluidPage



# Define server logic required to draw a histogram
server <- function(input, output) {
  criteria_select <- reactive({ ### whatever is in reactive function()
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
  
  
  output$census_table <- renderTable({
    census_table(input = "map")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



