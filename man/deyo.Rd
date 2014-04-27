\name{deyo}
\alias{deyo}
\title{
Produces a Charlson Score based on the Deyo ICD-9-CM adaptation
}
\description{
This function take as input a data frame structured such that each row contains a list of ICD-9-CM codes attributed 
to a single patient. The function goes from row to row comparing the ICD-9-CM codes a patient has with the Deyo
comorbidity index diagnosis codes.  If a patient has a diagnosis (as indicated by ICD-9-CM code) that belongs to one 
of the Deyo diagnoses, then the patient is considered to have this diagnosis.  Regardless of how many different 
ICD-9-CM codes a patient has corresponding to a particular comorbidity category, a comorbidity is only counted once.

The value returned consists of a vector and two data frames. The vector is the total Charlson score.  
In the first data frame returned each row in the data frame is devoted to a particular patient, and each column is 
a diagnosis.  The data frame codes a 0 if the patient does not have that diagnosis and 1 if the patient does have 
that diagnosis.  The second data frame, which codes the point value of that particular diagnosis in the
Charlson score rather than a 1.
}
\usage{
deyo(input.frame)
}

\arguments{
  \item{input.frame}{
This is a data frame with 5 character ICD-9-CM codes without decimal points.  
}
}
\details{
The ICD-9-CM codes must be 5 characters long and have no decimal poinst.If any codes are less than 5 digits, then
they must be led by zeroes or followed by the letter "X" to create a 5 character code.  No more than 2 "X" ' s can
be used at the tail end, or a meaningful ICD-9-CM code cannot be derived.  If your ICD-9-CM codes contain decimals
or some other filler character, then they must be converted to the format above for this function to work properly.
}
\value{
A list of one vector and 2 data frames
	\item{CHARLSON.SCORE}{A vector containing the Charlson score for each patient}
	\item{COMORBIDITIES}{A data frame denoting which diagnoses each patient has with a 1}
	\item{COMORBIDITIES.POINTS}{A data frame denoting which diagnoses each patient has with the point value of that diagnosis}

}
\references{
Deyo RA, Cherkin DC, Ciol MA. (1992)
Adapting a clinical comorbidity index for use with ICD-9-CM administrative databases.
J Clin Epidemiol. 45(6):613-9.
}
\author{
Paul Gerrard
}

\examples{
x <- matrix(0, nrow = 3, ncol = 2)
x[1,1] <- "41000"
x[1,2] <- "42800"
x[2,1] <- "57220"
x[2,2] <- "1961X"
x[3,1] <- "042XX"
x <- as.data.frame(x)
deyo(x)


## The function is currently defined as
function (input.frame) 
{
    apply.icd9.deyo <- function(input.frame) {
        ICD9.5digit.deyo <- function(icd.code) {
            if (is.na(icd.code)) {
                icd.code <- "00000"
            }
            icd9.3 <- substr(icd.code, 1, 3)
            icd9.4 <- substr(icd.code, 4, 4)
            icd9.5 <- substr(icd.code, 5, 5)
            if (icd9.4 == "X") {
                icd9.4 <- 0
            }
            if (icd9.5 == "X") {
                icd9.5 <- 0
            }
            icd9.result <- paste(icd9.3, icd9.4, icd9.5, sep = "")
            if (icd9.result == "V4340") {
                icd9.result <- 44390
            }
            return(as.numeric(icd9.result)/100)
        }
        n.rows <- length(input.frame[, 1])
        n.cols <- length(input.frame[1, ])
        output.frame <- matrix(0, nrow = n.rows, ncol = n.cols)
        for (i in 1:n.rows) {
            for (j in 1:n.cols) {
                output.frame[i, j] <- ICD9.5digit.deyo(input.frame[i, 
                  j])
            }
        }
        return(output.frame)
    }
    apply.convert.na <- function(input.frame) {
        convert.na <- function(input.val) {
            if (is.na(input.val)) {
                input.val <- 0
            }
            output.val <- input.val
            return(output.val)
        }
        n.rows <- length(input.frame[, 1])
        n.cols <- length(input.frame[1, ])
        output.frame <- matrix(0, nrow = n.rows, ncol = n.cols)
        for (i in 1:n.rows) {
            for (j in 1:n.cols) {
                output.frame[i, j] <- convert.na(input.frame[i, 
                  j])
            }
        }
        return(output.frame)
    }
    comorbidities.deyo <- function(input.frame) {
        mi <- c(seq(from = 410, to = 410.9, by = 0.01), 412)
        chf <- c(seq(from = 428, to = 428.9, by = 0.01))
        pvd <- c(443.9, 441, 441.9, 785.4)
        cvd <- c(seq(from = 430, to = 438, by = 0.01))
        dementia <- c(seq(from = 290, to = 290.9, by = 0.01))
        copd <- c(seq(from = 490, to = 496, by = 0.01), seq(from = 500, 
            to = 505, by = 0.01), 506.4)
        rheum <- c(710, 710.1, 710.4, seq(from = 714, to = 714.2, 
            by = 0.01), 714.81, 725)
        pud <- c(seq(from = 531, to = 534.9, by = 0.01))
        mild.liver <- c(571.2, 571.5, 571.6, seq(from = 571.4, 
            to = 571.49, by = 0.01))
        dm <- c(seq(from = 250, to = 250.3, by = 0.01), 250.7)
        dm.comp <- c(seq(from = 250.4, to = 250.6, by = 0.01))
        plegia <- c(344.1, seq(from = 342, to = 342.9, by = 0.01))
        renal <- c(seq(from = 582, to = 582.9, by = 0.01), seq(from = 583, 
            to = 583.7, by = 0.01), 585, 586, seq(from = 588, 
            to = 588.9, by = 0.01))
        malignancy <- c(seq(from = 140, to = 172.9, by = 0.01), 
            seq(from = 174, to = 195.8, by = 0.01), seq(from = 200, 
                to = 208.9, by = 0.01))
        severe.liver <- c(seq(from = 572.2, to = 572.8, by = 0.01), 
            seq(from = 456, to = 456.21, by = 0.01))
        mets <- c(seq(from = 196, to = 199.1, by = 0.01))
        hiv <- c(seq(from = 42, to = 44.93, by = 0.01))
        deyo.list <- list(mi, chf, pvd, cvd, dementia, copd, 
            rheum, pud, mild.liver, dm, dm.comp, plegia, renal, 
            malignancy, severe.liver, mets, hiv)
        n.rows <- length(input.frame[, 1])
        n.cols <- length(input.frame[1, ])
        output.frame <- matrix(0, nrow = n.rows, ncol = 17)
        for (i in 1:n.rows) {
            for (j in 1:n.cols) {
                for (k in 1:length(deyo.list)) {
                  for (m in 1:length(deyo.list[[k]])) {
                    if (input.frame[i, j] == deyo.list[[k]][m]) {
                      output.frame[i, k] <- 1
                    }
                  }
                }
            }
        }
        output.frame <- as.data.frame(output.frame)
        colnames(output.frame) <- c("MI", "CHF", "PVD", "CVD", 
            "DEMENTIA", "COPD", "RHEUM", "PUD", "MILD.LIVER", 
            "DM", "DM.COMP", "PLEGIA", "RENAL", "MALIGNANCY", 
            "SEVERE.LIVER", "METS", "HIV")
        return(output.frame)
    }
    convert.to.points <- function(input.frame) {
        n.rows <- length(input.frame[, 1])
        n.cols <- length(input.frame[1, ])
        output.frame <- input.frame
        output.frame[, 11] <- output.frame[, 11] * 2
        output.frame[, 12] <- output.frame[, 12] * 2
        output.frame[, 13] <- output.frame[, 13] * 2
        output.frame[, 14] <- output.frame[, 14] * 2
        output.frame[, 15] <- output.frame[, 15] * 3
        output.frame[, 16] <- output.frame[, 17] * 6
        output.frame[, 16] <- output.frame[, 17] * 6
        return(output.frame)
    }
    total.points <- function(input.frame) {
        n.rows <- length(input.frame[, 1])
        output.vector <- matrix(0, nrow = n.rows, ncol = 1)
        for (i in 1:n.rows) {
            output.vector[i] <- sum(input.frame[i, ])
        }
        return(output.vector)
    }
    interim.frame.1 <- apply.icd9.deyo(input.frame)
    interim.frame.2 <- apply.convert.na(interim.frame.1)
    interim.frame.3 <- comorbidities.deyo(interim.frame.2)
    interim.frame.4 <- convert.to.points(interim.frame.3)
    POINTS <- total.points(interim.frame.4)
    deyo.data <- list(POINTS, interim.frame.3, interim.frame.4)
    names(deyo.data) <- c("CHARLSON.SCORE", "COMORBIDITIES", 
        "COMORBIDITIES.POINTS")
    return(deyo.data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }

