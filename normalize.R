# Het toevoegen van twee kolommen: het totale van een sample aan responses 
# en de totale area van een sample. Hiermee kan later genormaliseerd worden
getTotals <- function(data){
  totalResponses <- c("sum resp")
  totalArea <- c("sum area")
  
  for(row in 2:nrow(data)){
    Resp_row <- c()
    Area_row <- c()
    
    for(col in 1:ncol(data)){
      # Som van alle Resp in 1 rij
      if(data[1,col] == "Resp."){
        resp <- as.numeric(as.character(data[row,col]))
        Resp_row <- c(Resp_row, resp)
      }
      
      # Som van alle Area in 1 rij 
      if(data[1,col] == "Area"){
        area <- as.numeric(as.character(data[row,col]))
        Area_row <- c(Area_row, area)
      }
    }
    
    SumResp <- sum(Resp_row)
    SumArea <- sum(Area_row)
    
    totalResponses <- c(totalResponses, SumResp)
    totalArea <- c(totalArea, SumArea)
  }
  
  data["sum Resp."] <- totalResponses
  data["sum Area"] <- totalArea
  return(data)
}

# Het normaliseren van de responses aan de hand van de totale response. 
# De response van een molecuul wordt gedeeld door de totale response van
# de sample en dat maal 100. Alleen de genormaliseerde responses worden 
# gereturnt. 
Normalization <- function(data){
  index_SumResp <- which(data == "sum resp", arr.ind = TRUE)
  column_SumResp <- index_SumResp[1,2]
  
  index_SumArea <- which(data == "sum area", arr.ind = TRUE)
  column_SumArea <- index_SumArea[1,2]
  
  norm_data <- data
  
  for(row in 2:nrow(data)){
    
    for(col in 1:ncol(data)){
      if(data[1,col] == "Resp."){
        value_resp <- as.numeric(as.character(data[row,col]))
        total_resp <- as.numeric(as.character(data[row, column_SumResp]))
        
        result_resp <- (value_resp/total_resp)*100
        norm_data[row,col] <-as.numeric(as.character(result_resp))
      }
      
      if(data[1,col] == "Area"){
        value_area <- as.numeric(as.character(data[row,col]))
        total_area <- as.numeric(as.character(data[row,column_SumArea]))
        
        result_area <- (value_area/total_area)*100
        norm_data[row,col] <- as.numeric(as.character(result_area))
      }
    }
  }
  return(norm_data)
}