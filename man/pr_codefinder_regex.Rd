\name{pr.charlson_Armitage2010_regex}
\alias{pr.charlson_Armitage2010_regex}
\alias{pr.charlson_Quan2005_regex}
\alias{pr.charlson_Sundarajan2004_regex}
\alias{pr.elixhauser_Quan2005_regex}
\title{Finds the matching codes for the different comorbidity groups}
\usage{
pr.charlson_Armitage2010_regex(icdCode, out, country_code,
  include_acute = rep(TRUE, times = length(icdCode)), icd_ver = rep(FALSE,
  times = length(icdCode)))

pr.charlson_Quan2005_regex(icdCode, out, country_code,
  include_acute = rep(TRUE, times = length(icdCode)), icd_ver = rep(FALSE,
  times = length(icdCode)))

pr.charlson_Sundarajan2004_regex(icdCode, out, country_code,
  include_acute = rep(TRUE, times = length(icdCode)), icd_ver = rep(FALSE,
  times = length(icdCode)))

pr.elixhauser_Quan2005_regex(icdCode, out, country_code,
  include_acute = rep(TRUE, times = length(icdCode)), icd_ver = rep(FALSE,
  times = length(icdCode)))
}
\arguments{
  \item{icdCode}{The icd code of interest, either a number
  or a string}

  \item{out}{If the function has been run previously there
  may already be matches for a particular group, if the out
  parameter is supplied with a vector equal to the number
  of Elixhauser comorbidities with their corresponding
  names only new findings will be appended.}

  \item{country_code}{The two-letter \code{ISO 3166-1
  alpha-2} code indicating the country of interest (the
  same as the top-level internet domain name of that
  country). As certain countries have adapted
  country-specific ICD-coding there may be minor
  differences between countries. Currently only Swedish
  (SE) and US codes are implemented. The function defaults
  to 'US'.}

  \item{include_acute}{Certain codes may indicate a
  non-chronic disease such as heart infarction, stroke or
  similar. Under some circumstances these should be
  ignored, e.g. when studying predictors for hip
  arthroplasty re-operations codes during the admission for
  the surgery should not include myocardial infarction as
  this is most likely a postoperative condition not
  available for scoring prior to the surgery.}

  \item{icd_ver}{The icd version of interest. If FALSE the
  software tries to automatically identify code version
  depending on the first letter,
  \code{\link{pr.get.icd.ver}}}
}
\value{
\code{vector} Returns a vector with the names of each
comorbidity group. If the entry is FALSE this correspond to
that no code matched the other group otherwise it returns
TRUE.
}
\description{
The functions loop through all the comorbidity groups in
search for a matching group to the provided \code{icdCode}.
}
\details{
The \code{_regex} indicates that these functions use
regular expressions for identification of codes of
interest. The match is case-insensitive. The function can
also identify acute conditions and ignore those if
specified.
}
\references{
J. N. Armitage and J. H. van der Meulen, "Identifying
co-morbidity in surgical patients using administrative data
with the Royal College of Surgeons Charlson Score" British
Journal of Surgery, vol. 97, no. 5, pp. 772-781, May 2010.

H. Quan, V. Sundararajan, P. Halfon, A. Fong, B. Burnand,
J.-C. Luthi, L. D. Saunders, C. A. Beck, T. E. Feasby, and
W. A. Ghali, "Coding algorithms for defining comorbidities
in ICD-9-CM and ICD-10 administrative data" Med Care, vol.
43, no. 11, pp. 1130-1139, Nov. 2005. - Charlson section

V. Sundararajan, T. Henderson, C. Perry, A. Muggivan, H.
Quan, and W. A. Ghali, "New ICD-10 version of the Charlson
comorbidity index predicted in-hospital mortality" J Clin
Epidemiol, vol. 57, no. 12, pp. 1288-1294, Dec. 2004.

H. Quan, V. Sundararajan, P. Halfon, A. Fong, B. Burnand,
J.-C. Luthi, L. D. Saunders, C. A. Beck, T. E. Feasby, and
W. A. Ghali, "Coding algorithms for defining comorbidities
in ICD-9-CM and ICD-10 administrative data" Med Care, vol.
43, no. 11, pp. 1130-1139, Nov. 2005. - Elixhauser section
}

