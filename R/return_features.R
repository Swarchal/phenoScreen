return_features <- function(pca_data, pca_vector, n = 2){
  
  # check inputs
  if (attributes(pca_data)$class != "prcomp"){
    stop("pca_data needs to be a prcomp object",
         call. = FALSE)
  }
  
  if (length(pca_vector) < 2){
    stop("pca_vector needs to contain at least 2 elements",
         call. = FALSE)
  }
  
  if (is.numeric(pca_vector) == FALSE){
    stop("pca_vector needs to be a numeric vector")
  }
  
  # get the loadings of the first two principal components
  loadings <- pca_data$rotation[,1:2]
  
  # function to convert pca's into ratio of 1
  pca_ratio <- function(x){
    total <- abs(x[1]) + abs(x[2])
    x1 <- x[1] / total
    x2 <- x[2] / total
    new_x <- c(x1, x2)
    return(new_x)
  }
  
  # convert pcs into ratios
  pca_vector_ratio <- pca_ratio(pca_vector)
  
  # weight loadings by pca_ratios
  alt_loadings <- as.data.frame(t(t(loadings) * pca_vector_ratio))
  
  # change row names into a column
  alt_loadings <- cbind(feature = rownames(alt_loadings), alt_loadings)
  rownames(alt_loadings) <- NULL
  
  # highest features for PC1
  w_PC1 <- alt_loadings[order(alt_loadings$PC1, decreasing = TRUE), ]
  PC1_f <- as.vector(w_PC1[1:n, 1])
  
  # highest features for PC2
  w_PC2 <- alt_loadings[order(alt_loadings$PC2, decreasing = TRUE), ]
  PC2_f <- as.vector(w_PC2[1:n, 1])
  
  return(c(PC1_f, PC2_f))
}