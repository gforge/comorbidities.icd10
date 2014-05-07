\name{pr.regex.code.match}
\alias{pr.regex.code.match}
\title{Match codes with regular expressions}
\usage{
pr.regex.code.match(icd_codes, icd_ver, out, codefinder.regex.comorbidity,
  include_acute = rep(TRUE, length(icd_codes)), codefinder.regex.acute)
}
\arguments{
  \item{icd_codes}{The vector with all the icd_codess that
  should be tested}

  \item{icd_ver}{The ICD-version, currently either 9 or 10}

  \item{out}{The out vector, see
  \code{\link{pr.get.out.vector}}}

  \item{codefinder.regex.comorbidity}{The regular
  expression list with all the comorbidities}

  \item{include_acute}{If acute diagnosis are to be
  included, e.g. myocardial infarction.}

  \item{codefinder.regex.acute}{A list with vectors
  containing regular expressions that identify the acute
  diagnoses that should be excluded if \code{include_acute}
  is set to TRUE for the specific code. If not provided the
  code will not check for acute diagnoses.}
}
\value{
\code{vector} Returns the out with TRUE for those groups
where a match was found.
}
\description{
The function loops through all the codes and the different
regular expressions in search for a match.n
}
\seealso{
\code{\link{codefinder.regex.elixhauser_Quan2005}},
\code{\link{codefinder.regex.charlson_Quan2005}},
\code{\link{codefinder.regex.charlson_Armitage2010}},
\code{\link{codefinder.regex.charlson_Sundarajan2004}}
}

