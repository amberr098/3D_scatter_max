library(plotly)

getSelectedMol <- function(mol, norm_data){
  find <- paste0(mol, "Results")
  getMol <- gsub(" ", ".", find)
  
  plot_data_col <- which(colnames(norm_data) == getMol, arr.ind = TRUE)
  column_medium <- which(norm_data == "Name", arr.ind = TRUE)
  values_col <- norm_data[,plot_data_col]
  medium_col <- norm_data[,column_medium[1,2]]
  
  numeric_mediums <- c()
  for(num in medium_col){
    if(!num == "Name"){
      numeric_mediums <- c(numeric_mediums, as.numeric(num))
    }
  }
  
  values_good <- values_col[-1]
  return(values_good)
}

getPlotData <- function(values_good, dataPlusMin){
  alignDataFrame <- data.frame(matrix(ncol=4, nrow = length(values_good)))
  
  for(value in 1:length(values_good)){
    glucose <- dataPlusMin[,1]
    galactose <- dataPlusMin[,2]
    glutamine <- dataPlusMin[,3]
    
    pmGlc <- glucose[value]
    pmGal <- galactose[value]
    pmGlu <- glutamine[value]
    val <- values_good[value]
    
    if(pmGlc == "+"){
      alignDataFrame[value,1] <- 25
      alignDataFrame[value,4] <- val
    }else if(pmGlc == "-"){
      alignDataFrame[value,1] <- 2
      alignDataFrame[value,4] <- val
    }else if(pmGlc == "0"){
      alignDataFrame[value,1] <- 5.55
      alignDataFrame[value,4] <- val
    }
    
    if(pmGal == "+"){
      alignDataFrame[value,2] <- 5
      alignDataFrame[value,4] <- val
    }else if(pmGal == "-"){
      alignDataFrame[value,2] <- 0.5
      alignDataFrame[value,4] <- val
    }else if(is.na(pmGal)){
      invisible("na")
    }
    
    if(pmGlu == "+"){
      alignDataFrame[value,3] <- 6
      alignDataFrame[value,4] <- val
    }else if(pmGlu == "-"){
      alignDataFrame[value,3] <- 2
      alignDataFrame[value,4] <- val
    }else if(is.na(pmGlu)){
      invisible("na")
    }
  }
  colnames(alignDataFrame) <- c("Glucose", "Galactose", "Glutamine", "Values")
  return(alignDataFrame)
}

setPlot <- function(alignDataFrame, mainTitle){
  p <- plot_ly(data = alignDataFrame, x=alignDataFrame$Glucose, y=alignDataFrame$Galactose, z=alignDataFrame$Glutamine,
               mode = "markers", type = 'scatter3d', marker = list(color = alignDataFrame$Values, colorscale = "Viridis", showscale = TRUE)) %>%
    layout(scene = list(xaxis = list(title = 'Glucose'),
                        yaxis = list(title = 'Galactose'),
                        zaxis = list(title = 'Glutamine')),
           title = mainTitle)
  
  return(p)
}