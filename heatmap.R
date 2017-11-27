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
  
  heatmap_matrix_1 <- heatmap_matrix[,-1]
  newnames <- heatmap_matrix_1[1,]
  
  heatmap_matrix_2 <- heatmap_matrix_1[-2,]
  colnames(heatmap_matrix_2) <- newnames
  
  heatmap_matrix_3 <- heatmap_matrix_2[-1,]
  
  heatmap_matrix_4 <- apply(heatmap_matrix_3, 2, as.numeric)
  orderedHM <- heatmap_matrix_4[order(heatmap_matrix_4[,1]), ]
  newrownames <- orderedHM[,1]
  rownames(orderedHM) <- newrownames
  
  heatmap_matrix_5 <- orderedHM[,-1]
  
  heatmap_matrix_6 <- heatmap_matrix_5[!duplicated(rownames(heatmap_matrix_5)), ]
  
  heatmap_matrix_final <<- t(heatmap_matrix_6)
  
  library(plotly)
  
  hm <- plot_ly(x= 1:ncol(heatmap_matrix_final),
               y= rownames(heatmap_matrix_final),
               z= heatmap_matrix_final, 
               colors = colorRamp(c("red", "green")),
               type = "heatmap")%>%
    layout(margin = list(l = 150, r = 50, b = 50, t = 50, pad = 4))

  return(hm)
}