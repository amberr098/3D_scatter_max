# Maakt de heatmap
setHeatmap <- function(norm_data){
  # Maken van de matrix met alleen de resp kolommen
  heatmap_matrix <- matrix(nrow = nrow(norm_data)+1)

  count <- 0 
  for(col in 1:ncol(norm_data)){
    count <- count + 1
    
    if(col%%2 == 0){
      if(!norm_data[1,col] == "sum resp"){
        coln <- colnames(norm_data)[col]
        adding_col <- c(coln, norm_data[,col])
        heatmap_matrix <- cbind(heatmap_matrix, adding_col)
      }
    }
  }
  final_hm <- getHeatmapMatrix(heatmap_matrix)  
  
  
  library(plotly)
  
  hm <- plot_ly(x= 1:ncol(final_hm),
               y= rownames(final_hm),
               z= final_hm, 
               colors = colorRamp(c("red", "green")),
               type = "heatmap")%>%
    layout(margin = list(l = 150, r = 50, b = 50, t = 50, pad = 4))

  return(hm)
}

getHeatmapMatrix <- function(heatmap_matrix){
  # Bevat als eerste en tweede rij nog kolomnamen.
  twoRowsExtra_hm <- heatmap_matrix[,-1]
  
  # De eerste rij worden de kolomnamen
  newnames <- twoRowsExtra_hm[1,]
  
  # Verwijderen van de tweede rij met de namen Resp. weghalen
  oneRowsExtra_hm <<- twoRowsExtra_hm[-2,]
  colnames(oneRowsExtra_hm) <- newnames
  
  # Verwijderen van de namen in de eerste rij met de molecuulnamen.
  noRowsExtra_hm <<- oneRowsExtra_hm[-1,]
  
  # De gehele matrix numeric maken om het te ordenen op Name/Sample nummer
  numeric_hm <<- apply(noRowsExtra_hm, 2, as.numeric)
  orderedHM <- numeric_hm[order(numeric_hm[,1]), ]
  newrownames <- orderedHM[,1]
  rownames(orderedHM) <- newrownames
  
  # Verwijderen van de eerste kolom met sample nummers
  onlyValues <<- orderedHM[,-1]
  
  # Het gemiddelde pakken van de duplicates. 
  noDuplicates <- matrix(NA, nrow = nrow(onlyValues)/2, ncol = ncol(onlyValues))
  nextRow <- 1
  count <- 0
  for(row in 1:nrow(onlyValues)){
    nextRow <- nextRow + 1
    if(isTRUE(rownames(onlyValues)[row] == rownames(onlyValues)[nextRow])){
      count <- count + 1
      for(col in 1:ncol(onlyValues)){
        av <- (as.numeric(onlyValues[row,col]) + as.numeric(onlyValues[nextRow, col]))/2
        noDuplicates[count, col] <- av
      }
    }
  }
  colnames(noDuplicates) <- colnames(onlyValues)
  totalRows <- nrow(onlyValues)/2
  rownames(noDuplicates) <- 1:totalRows
  
  # Omdraaien van de x en y as van de tabel
  final_hm <<- t(noDuplicates)
  
  return(final_hm)
}