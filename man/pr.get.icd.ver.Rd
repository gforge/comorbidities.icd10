\name{pr.get.icd.ver}
\alias{pr.get.icd.ver}
\title{Checks if it is ICD-9 or ICD-10}
\usage{
pr.get.icd.ver(codes, icd_ver = rep(FALSE, times = length(codes)))
}
\arguments{
  \item{codes}{A vector with ICD-codes}

  \item{icd_ver}{A vector with icd versions, has to have
  the length of 1 or the same length as \code{code}
  argument. If FALSE then all are checked otherwise only
  those that are FALSE are checked.}
}
\value{
\code{boolean} Returns TRUE if the code is deemed to be and
ICD-10 code otherwise it returns FALSE.
}
\description{
Identifies if first letter is a number or V and then
suggests ICD-9. There are V-codes in ICD-10 although these
are only transportation related disease causes such as
biking and automobile accidents. It is therefore better to
sort any of these codes into the ICD-9 in case they
actually belong there.
}
\details{
Note that E-code for the "SUPPLEMENTARY CLASSIFICATION OF
EXTERNAL CAUSES OF INJURY AND POISONING (E800-E999)" are
not included in the code check.
}

