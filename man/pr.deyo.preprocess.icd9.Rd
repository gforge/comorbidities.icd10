\name{pr.deyo.preprocess.icd9}
\alias{pr.deyo.preprocess.icd9}
\title{Convert icd9 codes to numeric values and convert V434X to 44390}
\usage{
pr.deyo.preprocess.icd9(input.frame)
}
\arguments{
  \item{input.frame}{This is a data frame with 5 character
  ICD-9-CM codes without decimal points. This can also be
  provided as a vector or matrix.}
}
\value{
\code{matrix} Returns a matrix with the same number of
columns and rows as the \code{input.frame}
}
\description{
Convert icd9 codes to numeric values and convert V434X to
44390
}
\author{
Max
}
\seealso{
\code{\link{ahrq}}
}

