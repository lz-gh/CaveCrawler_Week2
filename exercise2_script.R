library(shiny)
library(WVPlots)
library(shinyWidgets)

ui = fluidPage(
  
  h1("CaveCrawler Exercise 2"),
  br(),
  br(),
  br(),
  chooseSliderSkin(skin = "Nice", color = "maroon"),
  theme = "exercise_2.css",
  tabsetPanel(
    tabPanel(
      title = "Select by Population",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
         id = "sidebar1",
         checkboxGroupInput(
         inputId = "population_checkbox",
         label = "Select Population in database",
         choices = c("Arroyo","Arroyo Sarco","Caballo Moro","Chamal","Chica","Japonis",
                     "Nacimiento del Rio Santa Clara","Pachon","Rascon","Rio Choy",
                     "Rio Florido","Rio Frio","Rio Meco","Rio Subterraneo Valley",
                     "Rio Tampaon","Rio Tantaon","San Rafael Los Castros","Subterraneo",
                     "Tinaja","Toro","Yerbaniz"),
         tableOutput("table_out")
         ),
         uiOutput("minmax_updater"),
        ),
        mainPanel(id = "main",
                  "Plots and data from table to be shown here",
        )
      )
    ),
    tabPanel(
      title = "Select by Morph",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          id = "sidebar1",
          checkboxGroupInput(
            inputId = "morph_checkbox",
            label = "Select morph in database",
            choices = c("Cave", "Surface"),
            tableOutput("table_out")
          ),
          # uiOutput("minmax_updater"),
        ),
        mainPanel(id = "main",
                  "Plots and data from table to be shown here",
                  br(),
                  actionButton(
                    inputId = "action_button_1",
                    label = "Action Button!"),
                  conditionalPanel(
                    condition = "input.action_button_1 == true",
                      "Haven't decided on its function, but here's the skeleton"
                  )
        )
    )
   )
  )
)

server = function (input, output) {
  read.csv <- "PopulationLocations.csv"
  output$minmax_updater <- renderUI({
    lats <- table_population$Latitude
    
    min.lat <- round(min(lats), 2)
    max.lat <- round(max(lats), 2)
    
    sliderInput("min_latitude", "reactive threshold:", 
                min = min.lat, 
                max = max.lat, value = 22.5, step = 0.05)
  })
  
  table_population <- read.csv("PopulationLocations.csv")
  
  # Output only the entries of table_name whose latitude is ABOVE the value
  # inputted into the slider
  output$table_out <- renderTable({
    table_population[table_population$Latitude > input$min_latitude, ]
  })
}

shinyApp (ui = ui, server = server)