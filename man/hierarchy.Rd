\name{hierarchy.ahrq_2010v3.5}
\alias{hierarchy.ahrq_2010v3.5}
\alias{hierarchy.charlson_Armitage2010}
\alias{hierarchy.charlson_Deyo1992}
\alias{hierarchy.charlson_Quan2005}
\alias{hierarchy.charlson_Sundarajan2004}
\alias{hierarchy.elixhauser_Elixhauser1998}
\alias{hierarchy.elixhauser_Quan2005}
\title{Applies the comorbidity hierarchy to the comorbidity groups}
\usage{
hierarchy.ahrq_2010v3.5(out)

hierarchy.elixhauser_Elixhauser1998(out)

hierarchy.elixhauser_Quan2005(out)

hierarchy.charlson_Deyo1992(out)

hierarchy.charlson_Quan2005(out)

hierarchy.charlson_Sundarajan2004(out)

hierarchy.charlson_Armitage2010(out)
}
\arguments{
  \item{out}{A
  \code{matrix}/\code{data.frame}/\code{vector} with the
  comorbidity groups outputted from the
  \code{codefinder.*()} functions}
}
\value{
\code{out} Returns the same as the input but with the
hierarchy applied
}
\description{
The hierarchy functions generally remove less complicated
diseases when more complicated diseases are present. For
instance if a metastasis code is present then the solid
tumor code is obsolete.
}
\examples{
# AHRQ example
out <- codefinder.numeric.ahrq_2010v3.5("31323")
out[c("DM.COMP", "DM.UNCOMP")] <-TRUE
hierarchy.ahrq_2010v3.5(out)
# Deyo example
out <- codefinder.numeric.charlson_Deyo1992("31323")
out[grep("LIVER", names(out))] <-TRUE
hierarchy.charlson_Deyo1992(out)
# Armitage example
out <- codefinder.regex.charlson_Armitage2010("I252")
out[grep("LIVER", names(out))] <-TRUE
hierarchy.charlson_Armitage2010(out)
}
\seealso{
\code{\link{codefinder.numeric.ahrq_2010v3.5}}

\code{\link{codefinder.numeric.elixhauser_Elixhauser1998}}

\code{\link{codefinder.regex.elixhauser_Quan2005}}

\code{\link{codefinder.numeric.ahrq_2010v3.5}}

\code{\link{codefinder.regex.charlson_Quan2005}}

\code{\link{codefinder.regex.charlson_Sundarajan2004}}

\code{\link{codefinder.regex.charlson_Armitage2010}}
}

