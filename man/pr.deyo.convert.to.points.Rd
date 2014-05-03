\name{pr.deyo.convert.to.points}
\alias{pr.deyo.convert.to.points}
\title{Score points for Deyo's comorbidity weights}
\usage{
pr.deyo.convert.to.points(input.frame)
}
\arguments{
  \item{input.frame}{This is a data frame with 5 digit
  ICD-9-CM codes as XXX.XX This can also be provided as a
  vector or matrix.}
}
\value{
\code{matrix} Returns a matrix with the one column per
comorbidity. No comorbidity is indicated by 0 while 1
indicates existing comorbidity.
}
\description{
The following function adds the weights of the Charlson
original weight system.
}
\seealso{
\code{\link{deyo}}
}

