

library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("TITLE HEre "),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel("Put my widgets here",
                 radioButtons(inputId = "penguin_species",
                              label = "Select a criteria to map",
                              choices = c("Real Estate", "Landlord Policy", "Solar IRR", "Climate Risk")), # left value is displayed
                 ### select input box
                 "Choice a color",
                 selectInput(inputId = 'pt_color',
                             label = "choose your favorite color!",
                             choices = c("Awesome red" = "red",
                                         "Pretty purple" = "purple",
                                         "cornflowerblue",
                                         "orange"))
    ), ### end sidebarPanel
    
    mainPanel("put my graph here!",
              plotOutput(outputId = "penguin_plot"),
              tableOutput(outputId = "penguin_table")
    ) ### end mainPanel
    
  ) ### end sidebarLayout
  
) ### end fluidPage



# Define server logic required to draw a histogram
server <- function(input, output) {
  penguin_select <- reactive({ ### whatever is in reactive function()
    penguins %>%
      filter(species == input$penguin_species) # penguin species comes from inputId in radio buttons
  })
  
  penguin_table <- reactive({
    penguins %>%
      filter(species == input$penguin_species) %>%
      group_by(sex) %>%
      summarize(mean_flip = mean(flipper_length_mm),
                mean_mass = mean(body_mass_g))
  }) ### end penguin table
  
  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select(),
           aes(x = flipper_length_mm, y = body_mass_g )) +
      geom_point(color = input$pt_color) + ### change color based on selection
      theme_minimal()
  })
  
  output$penguin_table <- renderTable({
    penguin_table()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



