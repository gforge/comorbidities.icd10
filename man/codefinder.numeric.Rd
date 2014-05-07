\name{codefinder.numeric.ahrq_2010v3.5}
\alias{codefinder.numeric.ahrq_2010v3.5}
\alias{codefinder.numeric.charlson_Deyo1992}
\alias{codefinder.numeric.elixhauser_Elixhauser1998}
\title{Finds the matching codes for the different comorbidity groups}
\usage{
codefinder.numeric.ahrq_2010v3.5(icd_codes, out, country_code,
  include_acute = rep(TRUE, length(icd_codes)), icd_ver = 9)

codefinder.numeric.charlson_Deyo1992(icd_codes, out, country_code,
  include_acute = rep(TRUE, length(icd_codes)), icd_ver = 9)

codefinder.numeric.elixhauser_Elixhauser1998(icd_codes, out, country_code,
  include_acute = rep(TRUE, length(icd_codes)), icd_ver = 9)
}
\arguments{
  \item{icd_codes}{The icd code of interest, either a
  number or a string}

  \item{out}{If the function has been run previously there
  may already be matches for a particular group, if the out
  parameter is supplied with a vector equal to the number
  of Elixhauser comorbidities with their corresponding
  names only new findings will be appended.}

  \item{icd_ver}{The icd version of interest. Currently
  only ICD-9 is supported by the numeric matching
  algorithms. Leave empty.}

  \item{country_code}{The two-letter \code{ISO 3166-1
  alpha-2} code indicating the country of interest (the
  same as the top-level internet domain name of that
  country). As certain countries have adapted
  country-specific ICD-coding there may be minor
  differences between countries. Currently only US codes
  are implemented for the numeric algorithms.}

  \item{include_acute}{Certain codes may indicate a
  non-chronic disease such as heart infarction, stroke or
  similar. Under some circumstances these should be
  ignored, e.g. when studying predictors for hip
  arthroplasty re-operations codes during the admission for
  the surgery should not include myocardial infarction as
  this is most likely a postoperative condition not
  available for scoring prior to the surgery.}
}
\value{
\code{vector} Returns a vector with the names of each
comorbidity group. If the entry is FALSE this correspond to
that no code matched the other group otherwise it returns
TRUE.
}
\description{
The functions loop through all the comorbidity groups in
search for a matching group to the provided
\code{icd_codes}.
}
\details{
The \code{_regex} indicates that these functions use
regular expressions for identification of codes of
interest. The match is case-insensitive. The function can
also identify acute conditions and ignore those if
specified.
}
\examples{
codefinder.numeric.ahrq_2010v3.5(9320)
}
\references{
http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp

R. A. Deyo, D. C. Cherkin, and M. A. Ciol, "Adapting a
clinical comorbidity index for use with ICD-9-CM
administrative databases" Journal of Clinical Epidemiology,
vol. 45, no. 6, pp. 613-619, Jun. 1992.

Elixhauser A, Steiner C, Harris DR, Coffey RM. (1998)
Comorbidity measures for use with administrative data.  Med
Care. 36:8-27.
}

