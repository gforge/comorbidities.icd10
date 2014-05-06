\name{translate.ICD9.from.Swedish}
\alias{translate.ICD9.from.Swedish}
\title{Translator from Swedish ICD-9 to regular ICD-9}
\usage{
translate.ICD9.from.Swedish(icd)
}
\arguments{
  \item{icd}{A vector or a single string with the
  ICD-9-code}
}
\value{
\code{vector} Returns a string/vector with the ICD-9 codes.
}
\description{
Sweden decided to make a few adaptations to the ICD-9
system. One of them was to change the last 4th digit into a
letter, A:H,W,X, corresponding to the numbers 0-9.
}

