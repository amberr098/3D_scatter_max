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
                   div(style = "margin-top: 20px;",
                    column(4, uiOutput(outputId = "Cadd1"))
                   )
                  ),
                 fluidRow(
                   column(4, uiOutput(outputId = "Cmolecule2")),
                   div(style = "margin-top: 20px;", 
                    column(2, uiOutput(outputId = "Cadd2"))
                   )
                 ),
                 fluidRow(
                   column(4, uiOutput(outputId = "Cmolecule3")),
                   div(style = "margin-top: 20px;",
                    column(2, uiOutput(outputId = "Cadd3"))
                   )
                 ),
                 fluidRow(
                   column(4, uiOutput(outputId = "otherGraphs"))
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
                        div(style = "margin-top: 5%; ",
                            fluidRow(
                              column(4, plotlyOutput(outputId = "Graphic1", width = "450px", height = "350px")),
                              column(4, plotlyOutput(outputId = "Graphic2", width = "450px", height = "350px")),
                              column(4, plotlyOutput(outputId = "Graphic3", width = "450px", height = "350px"))
                            ),
                            div(style = "margin-top: 2%; ", 
                              fluidRow(
                                column(4, plotlyOutput(outputId = "Graphic4", width = "450px", height = "350px")),
                                column(4, plotlyOutput(outputId = "Graphic5", width = "450px", height = "350px")),
                                column(4, plotlyOutput(outputId = "Graphic6", width = "450px", height = "350px"))
                              )
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
      TotalData <- getTotals(data)
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
      output$otherGraphs <- renderUI({
        checkboxGroupInput(inputId = "specGraphs",label = "Other graphs", choices = c("UDP-glc / UDP-glcA", "UDP-glc / UDP-xylose","UDP-glcA / UDP-xylose"))
        # selectInput(inputId = "specGraphs", label = "Other graphs", choices = c("UDP-glc / UDP-glcA", "UDP-glc / UDP-xylose","UDP-glcA / UDP-xylose"), multiple = TRUE)
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
        average_dataframe1 <- getAverageDuplicates(alignDataFrame1)
        p1 <- setPlot(average_dataframe1, input$molecule1)

        output$Graphic1 <- renderPlotly({
          p1
        })
      }

      if(!is.null(input$molecule2)){
        source("visualize.R")
        values_good2 <- getSelectedMol(input$molecule2, norm_data)
        alignDataFrame2 <- getPlotData(values_good2, dataplusmin)
        average_dataframe2 <- getAverageDuplicates(alignDataFrame2)
        p2 <- setPlot(average_dataframe2, input$molecule2)

        output$Graphic2 <- renderPlotly({
          p2
        })
      }

      if(!is.null(input$molecule3)){
        source("visualize.R")
        values_good3 <- getSelectedMol(input$molecule3, norm_data)
        alignDataFrame3 <- getPlotData(values_good3, dataplusmin)
        average_dataframe3 <- getAverageDuplicates(alignDataFrame3)
        p3 <- setPlot(average_dataframe3, input$molecule3)

        output$Graphic3 <- renderPlotly({
          p3
        })
      }
    
    output$heatmapButton <- renderUI({
      actionButton(inputId = "actHeatmapB",
                   label = "Show heatmap")
    })
    
    updateTabsetPanel(session, "tabs", selected = "Visualisation")
    
    source("heatmap.R")
    hm <- setHeatmap(norm_data)
    
    output$heatmap <- renderPlotly({
      hm
    })
  })
  
  observeEvent(input$specGraphs, {
    choices <- input$specGraphs
    for(item in 1:length(choices)){
      inp <- choices[item]
      if(isTRUE(inp == "UDP-glc / UDP-glcA")){
        source("OtherGraphs.R")
        coordinates_duplicated <- getCoordinates(norm_data, "UDP.Glc.Results","UDP.GlcA.Results", dataplusmin)
        noDupl <- getAveragesDuplicates(coordinates_duplicated)
        coordinates_df <- getAllDuplicates(noDupl)
        p4 <- setPlot(coordinates_df, "UDP-glc / UDP-glcA")
        
        output$Graphic4 <- renderPlotly({
          p4
        })
      } 
      
      if(isTRUE(inp ==  "UDP-glc / UDP-xylose")){
        source("OtherGraphs.R")
        coordinates_duplicated <- getCoordinates(norm_data, "UDP.Glc.Results","UDP.xylose.Results", dataplusmin)
        noDupl <- getAveragesDuplicates(coordinates_duplicated)
        coordinates_df <- getAllDuplicates(noDupl)
        p5 <- setPlot(coordinates_df, "UDP-glc / UDP-xylose")
        
        output$Graphic5 <- renderPlotly({
          p5
        })
      }
      
      if(isTRUE(inp ==  "UDP-glcA / UDP-xylose")){
        source("OtherGraphs.R")
        coordinates_duplicated <- getCoordinates(norm_data, "UDP.GlcA.Results","UDP.xylose.Results", dataplusmin)
        noDupl <- getAveragesDuplicates(coordinates_duplicated)
        coordinates_df <- getAllDuplicates(noDupl)
        p6 <- setPlot(coordinates_df, "UDP-glcA / UDP-xylose")
        
        output$Graphic6 <- renderPlotly({
          p6
        })
      }
    }
    
  })
  
  }

shinyApp(ui = ui, server = server)