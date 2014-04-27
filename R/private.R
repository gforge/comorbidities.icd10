apply.convert.na <- function(input.frame) {
  convert.na <- function(input.val) {
    if (is.na(input.val)) {input.val <- 0}
    output.val <- input.val
    return(output.val)
  }
  
  n.rows <- length(input.frame[,1])
  n.cols <- length(input.frame[1,])
  output.frame <- matrix(0, nrow=n.rows, ncol=n.cols)
  for (i in 1:n.rows){
    for (j in 1:n.cols) {
      output.frame[i,j] <- convert.na(input.frame[i,j])
    }
  }
  return(output.frame)
}

#The following function sums the points in the comorbidites matrix produced above
total.points <- function (input.frame) {
  n.rows <- length(input.frame[,1])
  output.vector <- matrix(0, nrow=n.rows, ncol=1)
  for (i in 1:n.rows) {
    output.vector[i] <- sum(input.frame[i,])
  }
  return(output.vector)
}
