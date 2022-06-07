library(shiny)
library(WVPlots)
library(shinyWidgets)
# Annabel Edit: added this so I can output an empty plot - see code for reason
library(ggplot2)

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
         uiOutput("minmax_updater")
        ),
        mainPanel(id = "main",
                  br(),
                  conditionalPanel(condition = "input.population_checkbox.length < 2",
                                   "ERROR: Must input at least two populations"
                  ),
                  plotOutput('wvplot_1'),
                  tableOutput('react_tableout1'),
                  downloadButton(
                    outputId = "download_this1",
                    label = "Click to Download",
                    class = "download"
                    )
                  )
        )
    ),
    tabPanel(
      title = "Select by Morph",
      fluid = TRUE,
      fluidRow(
         column(
           width = 2,
           checkboxGroupInput(
           inputId = "morph_checkbox",
           label = "Select morph in database",
           choices = c("Cave", "Surface"),
           tableOutput("table_out")
          ),
          # uiOutput("minmax_updater"),
        ),
        column(
          width = 6,
          "IMPORTANT: Make a selection to display plot.",
          br(),
          plotOutput('wvplot_2'),
          # action button to display image
          actionButton(
            inputId = "action_button_1",
            label = "Action Button!"),
          conditionalPanel(
            condition = "input.action_button_1 == true",
            img(src = 'smashing_pumpkins.jpg')
          )
        ),
        column(
          width = 4,
          tableOutput('react_tableout2'),
          downloadButton(
            outputId = "download_this2",
            label = "Click to Download",
            class = "download"
            )
        )
      )
    )
  )
)

server = function (input, output) {
  
  table_population <- read.csv("PopulationLocations.csv")
  
  output$minmax_updater <- renderUI({
    lats <- table_population$Latitude
    
    min.lat <- round(min(lats), 2)
    max.lat <- round(max(lats), 2)
    
    sliderInput("min_latitude", "Latitude Threshold", 
                min = min.lat, 
                max = max.lat, value = 22.5, step = 0.05)
  })
  

  # Output only the entries of table_name whose latitude is ABOVE the value
  # inputted into the slider
  output$table_out <- renderTable({
    table_population[table_population$Latitude > input$min_latitude, ]
  })
  
  
  # Annabel Edit: To improve user friendliness AND to enhance computational
  # efficiency, I made it so the plot and table are only outputted if at least
  # 2 populations are selected. If less than two populations are selected, we 
  # must output an empty plot and table so the UI side is not drawing upon 
  # non-existent objects
  
    #a reactive data frame based on population checkboxes
    pop_data1 <- reactive({
      if(length(input$population_checkbox) >= 2){
        new_data <- table_population[table_population$Population %in% input$population_checkbox,]
        return(new_data)
      }else{
        data.frame()
      }
    })
    #outputs table based off of new_data
    output$react_tableout1 <- renderTable(req(pop_data1()))
    
    output$wvplot_1 <- renderPlot({
      if(length(input$population_checkbox) >= 2){
        ShadedDensity(
          frame = pop_data1(), #uses new reactive data
          xvar = 'Latitude',  #uses string vector of latitudes.
          tail = 'right',
          threshold = input$min_latitude,
          title = 'Density of Populations at Latitudes',
          shading = 'maroon'
        )
      }else{
        ggplot()
      }
    })
  
  #reactive data for tab 2
  pop_data2 <- reactive({
    new_data <- table_population[table_population$Morph %in% input$morph_checkbox,]
    return(new_data)
  })
  #table for tab 2
  output$react_tableout2 <- renderTable(req(pop_data2()))
  # wvplot for tab 2
  output$wvplot_2 <- renderPlot( 
    ShadedDensity(
      frame = pop_data2(), #uses new reactive data
      xvar = 'Latitude',  #uses string vector of latitudes.
      threshold = input$min_latitude,
      tail = 'right',
      title = 'Density of Morphs at Latitudes',
      shading = 'maroon'
    )
  )
  
  #download button for tab 1
  output$download_this1 <- downloadHandler(
    filename = function(){
      paste("Tab1_download_",Sys.Date(),".csv", sep = '')
    },
    content = function(file){
      write.csv(pop_data1(), file, row.names = FALSE)
    }
  )
  
  #download button for tab 2
  output$download_this2 <- downloadHandler(
    filename = function(){
      paste("Tab2_download_",Sys.Date(),".csv", sep = '')
    },
    content = function(file){
      write.csv(pop_data2(), file, row.names = FALSE)
    }
  )
}

shinyApp (ui = ui, server = server)