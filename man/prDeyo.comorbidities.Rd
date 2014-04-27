\name{prDeyo.comorbidities}
\alias{prDeyo.comorbidities}
\title{Score points for Deyo Charlson's index}
\usage{
prDeyo.comorbidities(input.frame)
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
The following function develops a matrix with rows devoted
to respondents and each column a comorbidity. The number in
each column is the point value of having the comorbidity.
}
\seealso{
\code{\link{deyo}}
}

