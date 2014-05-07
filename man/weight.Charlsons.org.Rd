\name{weight.Charlsons.org}
\alias{weight.Charlsons.org}
\title{Score points for Charlson's original comorbidity weights}
\usage{
weight.Charlsons.org(input.frame)
}
\arguments{
  \item{input.frame}{This is a \code{data.frame} or
  \code{matrix} with the comorbidity groups as returned by
  any of the Charlson functions}
}
\value{
Weighted comorbidities
}
\description{
The following function adds the weights of the Charlson
original weighting system where comorbidities were score
between 1 and 6.
}

