library(plotly)

# Uit alle genormaliseerde waarde de goede kolom pakken aan de hand van de keuze
# van de gebruiker. values_good returnt de kolom van het gekozen molecuul met de 
# genormaliseerde waardes. 
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

# Aan de hand van de plus en min data wordt er een tabel gecreeerd met de coordinaten
# van de punten met daarnaast de waarde van het punt. De tabel bestaat uit:
# Glucose, Galactose, Glutamine, Values. Hierbij zijn Glucose, Galactose en Glutamine
# de coordinaten voor het punt in de 3D plot.
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

# Van de duplicates een gemiddelde pakken van de values. 
getAverageDuplicates<-function(alignDataFrame){
  temp_matrix <- matrix(data = NA, nrow = nrow(alignDataFrame)/2, ncol = ncol(alignDataFrame))
  count <- 0 
  nextRow <- 1
  for(row in 1:nrow(alignDataFrame)){
    nextRow <- nextRow + 1
    
    if( (isTRUE(alignDataFrame[row,1] == alignDataFrame[nextRow,1])) && 
        (isTRUE(alignDataFrame[row,2] == alignDataFrame[nextRow,2])) && 
        (isTRUE(alignDataFrame[row,3] == alignDataFrame[nextRow,3])) ){
          count <- count +1 
          average <- (as.numeric(alignDataFrame[row,4]) + as.numeric(alignDataFrame[nextRow,4]))/2
          
          temp_matrix[count,1] <- alignDataFrame[row,1]
          temp_matrix[count,2] <- alignDataFrame[row,2]
          temp_matrix[count,3] <- alignDataFrame[row,3]
          temp_matrix[count,4] <- average
    }
  }
  colnames(temp_matrix) <- c("Glucose", "Galactose", "Glutamine", "Values")
  average_dataframe <- as.data.frame(temp_matrix)
  return(average_dataframe)
}

# Aan de hand van de tabel waarin Glucose, Galactose, Glutamine en values staan wordt
# er een plot gegenereerd. 
setPlot <- function(average_dataframe, mainTitle){
  p <- plot_ly(data = average_dataframe, x=average_dataframe$Glucose, y=average_dataframe$Galactose, z=average_dataframe$Glutamine,
               mode = "markers", type = 'scatter3d', marker = list(color = average_dataframe$Values, colorscale = "RdYlGn", showscale = TRUE)) %>%
    layout(scene = list(xaxis = list(title = 'Glucose'),
                        yaxis = list(title = 'Galactose'),
                        zaxis = list(title = 'Glutamine')),
           title = mainTitle)
  
  return(p)
}