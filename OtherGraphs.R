getCoordinates <- function(norm_data, mol1, mol2, dataPlusMin){
  mol1Col <- which(colnames(norm_data) == mol1, arr.ind = TRUE)
  mol2Col <- which(colnames(norm_data) == mol2, arr.ind = TRUE)
  mol1Values <- (norm_data[,mol1Col])[-1]
  mol2Values <- (norm_data[,mol2Col])[-1]
  
  coordinates_duplicated <- data.frame(matrix(ncol=5, nrow = length(mol1Values)))
  
  for(value in 1:length(mol1Values)){
    glucose <- dataPlusMin[,1]
    galactose <- dataPlusMin[,2]
    glutamine <- dataPlusMin[,3]
    
    # pm staat voor plus/min
    pmGlc <- glucose[value]
    pmGal <- galactose[value]
    pmGlu <- glutamine[value]
    val1 <- mol1Values[value]
    val2 <- mol2Values[value]
    
    if(pmGlc == "+"){
      coordinates_duplicated[value,1] <- 25
      coordinates_duplicated[value,4] <- val1
      coordinates_duplicated[value,5] <- val2
    }else if(pmGlc == "-"){
      coordinates_duplicated[value,1] <- 2
      coordinates_duplicated[value,4] <- val1
      coordinates_duplicated[value,5] <- val2
    }else if(pmGlc == "0"){
      coordinates_duplicated[value,1] <- 5.55
      coordinates_duplicated[value,4] <- val1
      coordinates_duplicated[value,5]
    }
    
    if(pmGal == "+"){
      coordinates_duplicated[value,2] <- 5
      coordinates_duplicated[value,4] <- val1
      coordinates_duplicated[value,5] <- val2
    }else if(pmGal == "-"){
      coordinates_duplicated[value,2] <- 0.5
      coordinates_duplicated[value,4] <- val1
      coordinates_duplicated[value,5] <- val2
    }else if(is.na(pmGal)){
      invisible("na")
    }
    
    if(pmGlu == "+"){
      coordinates_duplicated[value,3] <- 6
      coordinates_duplicated[value,4] <- val1
      coordinates_duplicated[value,5] <- val2
    }else if(pmGlu == "-"){
      coordinates_duplicated[value,3] <- 2
      coordinates_duplicated[value,4] <- val1
      coordinates_duplicated[value,5] <- val2
    }else if(is.na(pmGlu)){
      invisible("na")
    }
  }
  colnames(coordinates_duplicated) <- c("Glucose", "Galactose", "Glutamine", mol1, mol2)
  
  return(coordinates_duplicated)
}

getAveragesDuplicates <- function(coordinates_duplicated){
  temp_matrix <- matrix(data = NA, nrow = nrow(coordinates_duplicated)/2, ncol = ncol(coordinates_duplicated))
  count <- 0 
  nextRow <- 1
  for(row in 1:nrow(coordinates_duplicated)){
    nextRow <- nextRow + 1
    
    if( (isTRUE(coordinates_duplicated[row,1] == coordinates_duplicated[nextRow,1])) && 
        (isTRUE(coordinates_duplicated[row,2] == coordinates_duplicated[nextRow,2])) && 
        (isTRUE(coordinates_duplicated[row,3] == coordinates_duplicated[nextRow,3])) ){
      count <- count +1 
      average1 <- (as.numeric(coordinates_duplicated[row,4]) + as.numeric(coordinates_duplicated[nextRow,4]))/2
      average2 <- (as.numeric(coordinates_duplicated[row,5]) + as.numeric(coordinates_duplicated[nextRow,5]))/2

      temp_matrix[count,1] <- coordinates_duplicated[row,1]
      temp_matrix[count,2] <- coordinates_duplicated[row,2]
      temp_matrix[count,3] <- coordinates_duplicated[row,3]
      temp_matrix[count,4] <- average1
      temp_matrix[count,5] <- average2
    }
  }
  colnames(temp_matrix) <- c("Glucose", "Galactose", "Glutamine", colnames(coordinates_duplicated)[4], colnames(coordinates_duplicated)[5])
  averageDuplicates_dataframe <- as.data.frame(temp_matrix)
  return(averageDuplicates_dataframe)
}

getAllDuplicates <- function(data_noDupl){
  coordinates_m <- matrix(NA, nrow = nrow(data_noDupl), ncol = 4)
  
  for(row in 1:nrow(data_noDupl)){
    val <- data_noDupl[row,4]/data_noDupl[row,5]
    coordinates_m[row,1] <- data_noDupl[row,1]
    coordinates_m[row,2] <- data_noDupl[row,2]
    coordinates_m[row,3] <- data_noDupl[row,3]
    coordinates_m[row,4] <- val
  }
  
  colnames(coordinates_m) <- c("Glucose", "Galactose", "Glutamine", "Values")
  coordinates_df <- as.data.frame(coordinates_m)
  return(coordinates_df)
}

setPlot<- function(coordinates_df, mainTitle){
  p <- plot_ly(data = coordinates_df, x=coordinates_df$Glucose, y=coordinates_df$Galactose, z=coordinates_df$Glutamine,
               mode = "markers", type = 'scatter3d', marker = list(color = coordinates_df$Values, colorscale = "RdYlGn", showscale = TRUE)) %>%
    layout(scene = list(xaxis = list(title = 'Glucose'),
                        yaxis = list(title = 'Galactose'),
                        zaxis = list(title = 'Glutamine')),
           title = mainTitle)
  
  return(p)
}