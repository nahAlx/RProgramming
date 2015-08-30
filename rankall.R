rankall <- function(outcome, num = "best") {
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  icol <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else if (outcome == "pneumonia") {
    23
  } else {
    stop("invalid outcome")
  }
  
  x <- x[, c(7, 2, icol)]
  x[, 3] <- suppressWarnings(as.numeric(x[, 3]))
  x <- x[order(x[1], x[3], x[2]), ]
  x <- x[!is.na(x[3]), ]
  y <- split(x, x[1])
  
  if (num == "best") {
    z <- lapply(y, function(i) { i[1,] })
  } else if (num == "worst") {
    z <- lapply(y, function(i) { i[nrow(i),] })
  } else {
    z <- lapply(y, function(i) { i[num,] })
  }
  
  res <- as.data.frame(do.call(rbind, z))
  res[,1] <- row.names(res)
  res <- res[, 2:1]
  colnames(res) <- c("hospital", "state")
  res
}