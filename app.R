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
               
               div(style = "margin-top: 40px;",
                 fluidRow(
                   column(4, pickerInput(inputId = "molecule1", label = "Select molecule", choices = c(), multiple = FALSE))
                 ),
                 fluidRow(
                   column(4, pickerInput(inputId = "molecule2", label = "Select molecule", choices = c(), multiple = FALSE))
                 ),
                 fluidRow(
                   column(4, pickerInput(inputId = "molecule3", label = "Select molecule", choices = c(), multiple = FALSE)),
                   div(style = "margin-top: 25px;",
                       column(4, actionButton("plot", "Plot"))
                   )
                 )
               )
             )
    ),
    
    tabPanel("Visualisation", icon = icon("bar-chart-o"),
             div(style = "margin-top: 11%; ",
                 fluidRow(
                   column(4, plotlyOutput(outputId = "Graphic1", width = "450px", height = "350px")),
                   column(4, plotlyOutput(outputId = "Graphic2", width = "450px", height = "350px")),
                   column(4, plotlyOutput(outputId = "Graphic3", width = "450px", height = "350px"))
                 )
              )
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
      
      updatePickerInput(session, "molecule1",
                        label = "Select molecule",
                        choices = c(molChoose))
      
      updatePickerInput(session, "molecule2",
                        label = "Select molecule",
                        choices = c(molChoose))
      
      updatePickerInput(session, "molecule3",
                        label = "Select molecule",
                        choices = c(molChoose))
    }
    
  })
  
  observeEvent(input$plot, {
    source("visualize.R")
    values_good1 <- getSelectedMol(input$molecule1, norm_data)
    values_good2 <- getSelectedMol(input$molecule2, norm_data)
    values_good3 <- getSelectedMol(input$molecule3, norm_data)
    
    alignDataFrame1 <- getPlotData(values_good1, dataplusmin)
    alignDataFrame2 <- getPlotData(values_good2, dataplusmin)
    alignDataFrame3 <- getPlotData(values_good3, dataplusmin)
    
    p1 <- setPlot(alignDataFrame1)
    p2 <- setPlot(alignDataFrame2)
    p3 <- setPlot(alignDataFrame3)
    
    updateTabsetPanel(session, "tabs", selected = "Visualisation")
    
    output$Graphic1 <- renderPlotly({
      p1
    })
    
    output$Graphic2 <- renderPlotly({
      p2
    })
    
    output$Graphic3 <- renderPlotly({
      p3
    })
  })
  
  
  }

shinyApp(ui = ui, server = server)