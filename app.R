library(shiny)
library(xlsx)
library(shinythemes)
library(shinyWidgets)
library(plotly)

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
                   column(4, uiOutput(outputId = "Cmolecule1")),
                   column(2, uiOutput(outputId = "Cadd1"))
                 ),
                 fluidRow(
                   column(4, uiOutput(outputId = "Cmolecule2")),
                   column(2, uiOutput(outputId = "Cadd2"))
                 ),
                 fluidRow(
                   column(4, uiOutput(outputId = "Cmolecule3")),
                   column(2, uiOutput(outputId = "Cadd3"))
                 ),
                 
                 fluidRow(
                   column(4, uiOutput(outputId = "plotB"))
                 )
               )
             )
    ),
    
    tabPanel("Visualisation", icon = icon("bar-chart-o"),
             
             tabsetPanel(id = "visPanel",type = "tabs",
                         
               tabPanel("Graphic",
                        div(style = "margin-top: 11%; ",
                            fluidRow(
                              column(4, plotlyOutput(outputId = "Graphic1", width = "450px", height = "350px")),
                              column(4, plotlyOutput(outputId = "Graphic2", width = "450px", height = "350px")),
                              column(4, plotlyOutput(outputId = "Graphic3", width = "450px", height = "350px"))
                            )
                          )
                        ),
               
               tabPanel("Heatmap",
                        plotlyOutput(outputId = "heatmap")
                        
                        
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
      ch <<- molChoose
      
      output$Cmolecule1 <- renderUI({
        selectInput(inputId = "molecule1", label = "Select molecule", choices = c(ch))
      })
      
      output$Cadd1 <- renderUI({
        actionButton(inputId = "Add1", label = NULL, icon = icon("plus"))
      })
      
      output$plotB <- renderUI({
        actionButton(inputId = "plot", label = "Plot")
      })
    }
    
  })
  observeEvent(input$Add1, {
    output$Cmolecule2 <- renderUI({
      selectInput(inputId = "molecule2", label = "Select molecule", choices = c(ch))
    })
    
    output$Cadd2 <- renderUI({
      actionButton(inputId = "Add2", label = NULL, icon = icon("plus"))
    })
    
    observeEvent(input$Add2, {
      output$Cmolecule3 <- renderUI({
        selectInput(inputId = "molecule3", label = "Select molecule", choices = c(ch))
      })
    })
  })
  

  observeEvent(input$plot, {
      if(!is.null(input$molecule1)){
        source("visualize.R")
        values_good1 <- getSelectedMol(input$molecule1, norm_data)
        alignDataFrame1 <- getPlotData(values_good1, dataplusmin)
        p1 <- setPlot(alignDataFrame1, input$molecule1)

        output$Graphic1 <- renderPlotly({
          p1
        })
      }

      if(!is.null(input$molecule2)){
        source("visualize.R")
        values_good2 <- getSelectedMol(input$molecule2, norm_data)
        alignDataFrame2 <- getPlotData(values_good2, dataplusmin)
        p2 <- setPlot(alignDataFrame2, input$molecule2)

        output$Graphic2 <- renderPlotly({
          p2
        })
      }

      if(!is.null(input$molecule3)){
        source("visualize.R")
        values_good3 <- getSelectedMol(input$molecule3, norm_data)
        alignDataFrame3 <- getPlotData(values_good3, dataplusmin)
        p3 <- setPlot(alignDataFrame3, input$molecule3)

        output$Graphic3 <- renderPlotly({
          p3
        })
      }
    
    output$heatmapButton <- renderUI({
      actionButton(inputId = "actHeatmapB",
                   label = "Show heatmap")
    })
    
    updateTabsetPanel(session, "tabs", selected = "Visualisation")
  })
  

  source("heatmap.R")
  hm <- setHeatmap(norm_data)
    
  output$heatmap <- renderPlotly({
    hm
  })
  
  }

shinyApp(ui = ui, server = server)