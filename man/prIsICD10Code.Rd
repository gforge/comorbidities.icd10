\name{prIsICD10Code}
\alias{prIsICD10Code}
\title{Checks if it is ICD-9 or ICD-10}
\usage{
prIsICD10Code(code)
}
\arguments{
  \item{code}{A string with the ICD-code}
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

