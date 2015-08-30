pollutantmean <- function(directory, pollutant, id = 1:332) {
  int <- 1
  for (x in id) {
    file <- paste (directory,"/",sprintf("%03d",x),".csv", sep = "")
    matriz <- read.csv(file)
    if (int ==1){
      aux <- matriz
    }else{
      aux <- rbind(matriz,aux)
    }
    int <- int +1
  }
  retorno <- mean(aux[,pollutant],na.rm = TRUE)
  retorno <- round(retorno,digits = 3)
  retorno
}
