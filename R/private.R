#' Function for converting na to 0
#' 
#' @param input.frame Input data frame
#' @return output.data.frame
apply.convert.na <- function(input.frame) {
  # TODO: the NAs should actually just be skipped for performance
  output.frame <- matrix(0, 
                         nrow=NROW(input.frame), 
                         ncol=NCOL(input.frame))
  for (i in 1:NCOL(input.frame)){
    output.frame[,i] <- ifelse(is.na(input.frame[,i]), 
                               0, input.frame[,i])
  }
  return(output.frame)
}
