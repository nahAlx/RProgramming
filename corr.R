corr <- function(directory, threshold = 0) {
  id = 1:332
  totalCorrelations <- c()
  for (x in id) {
    file <- paste (directory,"/",sprintf("%03d",x),".csv", sep = "")
    matriz <- read.csv(file)
    completeCases <- sum(complete.cases(matriz))
    if (completeCases > threshold) {
      correlation <- cor(matriz$nitrate, matriz$sulfate, use="complete.obs")
      correlation <- round (correlation,digits = 5)
      totalCorrelations <- c(totalCorrelations,correlation)
    }
  }
  totalCorrelations <- unlist(totalCorrelations[!sapply(totalCorrelations, is.null)])
  return (totalCorrelations)
}