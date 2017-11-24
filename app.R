library(shiny)
library(xlsx)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("Visualize", id = "tabs",
    tabPanel("Dataset", 
             icon = icon("table"), 
             div(style = "margin-top: 30px;",
               fluidRow(
                 column(4, fileInput('file1', 'Select your file')),
                 column(4, fileInput('filePlusMin', 'Select +/- file')),
                 div(style = "margin-top: 25px;",
                  column(4, actionButton("upload", "Upload"))
                 )
               ),
               
               fluidRow(
                 column(4, pickerInput(inputId = "molecule", label = "Select molecule", choices = c(), multiple = FALSE)),
                 div(style = "margin-top: 25px;",
                  column(4, actionButton("plot", "Plot"))
                 )
               )
             )
    ),
    
    tabPanel("Visualisation", icon = icon("bar-chart-o"),
             div(style = "margin-top: 40px; margin-left: 20%;",
              plotlyOutput(outputId = "Graphic", width = "800px", height = "600px"))
             )
  )
)


server <- function(input, output, session) {

  observeEvent(input$upload, {
    if(!is.null(input$file1) && !is.null(input$filePlusMin)){
      data <- read.xlsx(input$file1$datapath, sheetIndex = 1, stringsAsFactors=FALSE)
      dataplusmin <<- read.xlsx(input$filePlusMin$datapath, sheetIndex =1, stringsAsFactors=FALSE)
      source("normalize.R")
      TotalData <- getTotals(data, dataplusmin)
      norm_data <<- Normalization(TotalData)
      
      molChoose <- c()
      
      for(name in 1:length(colnames(data))){
        if(!colnames(data)[name] == "Sample"){
          if(name%%2 == 0){
            molfirst <- gsub("[.]", " ", colnames(data)[name])
            mol <- gsub("Results", "", molfirst)
            molChoose <- c(molChoose, mol)
          }else{
            invisible("geen keuze")
          }
        }else{
          invisible("geen keuze")
        }
      }
      
      updatePickerInput(session, "molecule",
                        label = "Select molecule",
                        choices = c(molChoose))
    }
    
  })
  
  observeEvent(input$plot, {
    source("visualize.R")
    values_good <- getSelectedMol(input$molecule, norm_data)
    alignDataFrame <- getPlotData(values_good, dataplusmin)
    p <- setPlot(alignDataFrame)
    updateTabsetPanel(session, "tabs", selected = "Visualisation")
    
    output$Graphic <- renderPlotly({
      p
    })
  })
  
  
  }

shinyApp(ui = ui, server = server)

