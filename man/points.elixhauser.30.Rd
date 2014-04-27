\name{points.elixhauser.30}
\alias{points.elixhauser.30}
\title{Score points for Elixhauser's comorbidity count}
\usage{
\method{points}{elixhauser.30}(input.frame)
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
to respondents and each column a comorbidity.
}
\seealso{
\code{\link{elixhauser}}
}

