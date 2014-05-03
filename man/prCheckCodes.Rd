\name{prCheckCodes}
\alias{prCheckCodes}
\title{Loops through score codes and checks for match}
\usage{
prCheckCodes(out, icdCode, score_codes, code_is_icd10, include_acute = FALSE,
  acute_identifier_regex)
}
\arguments{
  \item{out}{The out vector, see
  \code{\link{prGetOutVector}}}

  \item{icdCode}{The code that we want to test for match}

  \item{score_codes}{A list with different icd groups of
  interest.  Each icd-group should also have an icd-9 or
  icd-10 indicator}

  \item{code_is_icd10}{True if the \code{icdCode} is of
  icd-10 format}

  \item{include_acute}{Under some circumstances you want to
  avoid acute diagnoses to be included, for instance if a
  knee arthroplasty is performed the myocardial infarction
  has most probably occurred post-operatively and should
  not be part of the comorbidity calculation.  Set this to
  \code{TRUE} if you want the function to avoid acute
  diagnosis according to the \code{acute_identifier_regex}
  regular expression, default is \code{FALSE}.}

  \item{acute_identifier_regex}{The regular expression used
  if \code{avoid_acute} is set to \code{TRUE}.}
}
\value{
\code{vector} Returns the out with TRUE for those groups
where a match was found.
}
\description{
Fills in the out vector with the positive findings
}

