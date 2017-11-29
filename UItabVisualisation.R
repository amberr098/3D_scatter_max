tabVisualisation <-     tabPanel("Visualisation", icon = icon("bar-chart-o"),
                                 
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