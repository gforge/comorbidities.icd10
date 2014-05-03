\name{pr.regex.code.match}
\alias{pr.regex.code.match}
\title{Match codes with regular expressions}
\usage{
pr.regex.code.match(icdCode, icd_ver, out, comorbidity_regex,
  include_acute = rep(TRUE, length(icdCode)), acute_regex)
}
\arguments{
  \item{icdCode}{The vector with all the icdCodes that
  should be tested}

  \item{icd_ver}{The ICD-version, currently either 9 or 10}

  \item{out}{The out vector, see
  \code{\link{pr.get.out.vector}}}

  \item{comorbidity_regex}{The regular expression list with
  all the comorbidities}

  \item{include_acute}{If acute diagnosis are to be
  included, e.g. myocardial infarction.}

  \item{acute_regex}{A vector with the regular expressions
  that identify the acute diagnoses that should be excluded
  if \code{include_acute} is set to TRUE for the specific
  code. If not provided the code will not check for acute
  diagnoses.}
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
\code{\link{pr.elixhauser_Quan2005_regex}},
\code{\link{pr.charlson_Quan2005_regex}},
\code{\link{pr.charlson_Armitage2010_regex}},
\code{\link{pr.charlson_Sundarajan2004_regex}}
}

