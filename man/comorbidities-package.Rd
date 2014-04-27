\name{comorbidities-package}
\alias{comorbidities-package}
\alias{comorbidities}
\docType{package}
\title{Categorizes ICD-9-CM codes based on published comorbidity indices}
\description{
Methods to categorize ICD-9-CM codes into sensible disease categories have been developed and published by numerous
authors.  Two of the most widely used such methods are the Deyo adaptation of Charlson index and the Elixhauser
index.  This package has functions to categorize comorbidites into the Deyo-Charlson index, the original Elixhauser
index of 30 comorbidities, and the AHRQ comorbidity index (an update to the original Elixhauser method).
}
\details{
\tabular{ll}{
Package: \tab comorbidities\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-10-02\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
This package consists of 3 functions: deyo, elixhauser, and ahrq.  The functions are very similar in that they each
take as input a data frame structured such that each row contains a list of ICD-9-CM codes (e.g. discharge or 
admission diagnoses) attributed to a single patient. The function goes from row to row comparing the ICD-9-CM 
codes a patient has with the particular comorbidity index that function represents.  If a patient has a diagnosis
(as indicated by ICD-9-CM code) that is one of the diagnoses in the paritcular index chosen, then the patient is
considered to have this diagnosis.  Regardless of how many different ICD-9-CM codes a patient has corresponding to
a particular comorbidity category, a comorbidity is only counted once.

The value returned consists of a vector and one or two data frames. The vector is the total comorbidity count, or
in the case of the deyo() function, the total Charlson score.  The functions elixhauser() and ahrq() return one
data frame.  Each row in the data frame is devoted to a particular patient, and each column is a diagnosis.  
The data frame codes a 0 if the patient does not have that diagnosis and 1 if the patient does have that diagnosis.
The deyo() function returns a second data frame, which codes the point value of that particular diagnosis in the
Charlson score rather than a 1.
}
\author{
Paul Gerrard

Maintainer: Paul Gerrard <pbgerrard@partners.org>
}
\references{
Deyo RA, Cherkin DC, Ciol MA. (1992)
Adapting a clinical comorbidity index for use with ICD-9-CM administrative databases.
J Clin Epidemiol. 45(6):613-9.

Elixhauser A, Steiner C, Harris DR, Coffey RM. (1998) 
Comorbidity measures for use with administrative data. 
Med Care. 36:8-27.

Agency for Healthcare Research and Quality (2010)
CREATION OF FORMAT LIBRARY FOR COMORBIDITY GROUPS COMORBIDITY SOFTWARE, VERSION 3.6 
Available at: http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comformat2011.txt
Last accessed: 10/2/11
}

\keyword{ package }

\examples{
x <- matrix(0, nrow = 3, ncol = 2)
x[1,1] <- "41000"
x[1,2] <- "42800"
x[2,1] <- "57220"
x[2,2] <- "1961X"
x[3,1] <- "042XX"
x <- as.data.frame(x)
deyo(x)
}
