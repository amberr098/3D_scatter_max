tabDataset <- tabPanel("Dataset", 
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
)