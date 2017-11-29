library(shiny)
library(xlsx)
library(shinythemes)
library(shinyWidgets)
library(plotly)

source("UItabDataset.R")
source("UItabVisualisation.R")

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("Visualize", id = "tabs",
    tabDataset,
    tabVisualisation
  )
)


server <- function(input, output, session) {
  # Als de upload button is geactiveerd, wordt de data genormaliseerd en komen de opties
  # voor de gebruiker in de webapplicatie. 
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
      })
      output$plotB <- renderUI({
        actionButton(inputId = "plot", label = "Plot")
      })
    }
    
  })
  
  # Hier worden er selectinputs toegevoegd wanneer er op de + button wordt geklikt
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

  # Wanneer er op de plot button wordt geklikt, worden de coordinaten bepaald van 
  # de gekozen moleculen in de 3D plot en worden de 3D plots weergeven van de moleculen 
  # die gekozen zijn door de gebruiker.
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
  
  # Dit wordt geactiveerd wanneer er een van de "other graphs" wordt geselecteerd. De
  # coordinaten worden bepaald en mol1 wordt gedeeld door mol 2.
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