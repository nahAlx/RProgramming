complete <- function(directory, id = 1:332) {
  int <- 0
  retorno <- data.frame()
  for (x in id) {
    file <- paste (directory,"/",sprintf("%03d",x),".csv", sep = "")
    #print(file)
    matriz <- read.csv(file)
    newnob <- nrow(matriz[complete.cases(matriz),])
    newrow <- c(as.integer(x),newnob)
    retorno <- rbind(retorno,newrow)
  }
  colnames(retorno) <- c('id', 'nobs')
  retorno
}
