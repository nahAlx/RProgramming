best <- function(state, outcome) {
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  all_states <- unique(x$State)
  if (!(state %in% all_states)) {
    stop("invalid state")
  }
  
  icol <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else if (outcome == "pneumonia") {
    23
  } else {
    stop("invalid outcome")
  }
  
  x <- x[x$State==state, c(2, icol)]
  x[, 2] <- suppressWarnings(as.numeric(x[, 2]))
  x <- x[!is.na(x[,2]),]
  
  # order returns vector of indexes
  x[order(x[2], x[1])[1], 1]
}
