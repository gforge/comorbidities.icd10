\name{ahrq}
\alias{ahrq}

\title{
Produces a comorbidity count based on the AHRQ comorbidity index v3.6, based on the Elixhauser method
}
\description{
This function take as input a data frame structured such that each row contains a list of ICD-9-CM codes attributed 
to a single patient. The function goes from row to row comparing the ICD-9-CM codes a patient has with the AHRQ
comorbidity index diagnosis codes.  If a patient has a diagnosis (as indicated by ICD-9-CM code) that belongs to one 
of the AHRQ diagnoses, then the patient is considered to have this diagnosis.  Regardless of how many different 
ICD-9-CM codes a patient has corresponding to a particular comorbidity category, a comorbidity is only counted once.

The value returned consists of a vector and a data frames. The vector is the total comorbidity count.  
In the data frame returned each row in the data frame is devoted to a particular patient, and each column is 
a diagnosis.  The data frame codes a 0 if the patient does not have that diagnosis and 1 if the patient does have 
that diagnosis.  
}
\usage{
ahrq(input.frame)
}

\arguments{
  \item{input.frame}{
This is a data frame with 5 character ICD-9-CM codes without decimal points
}
}
\details{
The ICD-9-CM codes must be 5 characters long and have no decimal poinst.If any codes are less than 5 digits, then
they must be led by zeroes or followed by the letter "X" to create a 5 character code.  No more than 2 "X" ' s can
be used at the tail end, or a meaningful ICD-9-CM code cannot be derived.  If your ICD-9-CM codes contain decimals
or some other filler character, then they must be converted to the format above for this function to work properly.
}
\value{
A list of one vector and one data frames
	\item{COMORBIDITY.CT}{A vector containing the number of comorbidtities for each patient}
	\item{COMORBIDITIES}{A data frame denoting which diagnoses each patient has with a 1}
}
\references{
Agency for Healthcare Research and Quality (2010)
CREATION OF FORMAT LIBRARY FOR COMORBIDITY GROUPS COMORBIDITY SOFTWARE, VERSION 3.6 
Available at: http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comformat2011.txt
Last accessed: 10/2/11
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
ahrq(x)

## The function is currently defined as
function (input.frame) 
{
    apply.icd9.ahrq <- function(input.frame) {
        ICD9.5digit.ahrq <- function(icd.code) {
            process.v.codes <- function(v.code) {
                icd9.2.5 <- as.numeric(substr(v.code, 2, 5))
                if (icd9.2.5 == 4220) {
                  v.code <- 9320
                }
                if (icd9.2.5 == 4330) {
                  v.code <- 9320
                }
                if (icd9.2.5 == 4340) {
                  v.code <- 44000
                }
                if (icd9.2.5 == 4200) {
                  v.code <- 58530
                }
                if (icd9.2.5 == 4510) {
                  v.code <- 58530
                }
                if ((icd9.2.5 >= 5600) & (icd9.2.5 <= 5632)) {
                  v.code <- 58530
                }
                if (icd9.2.5 == 5680) {
                  v.code <- 58530
                }
                if (icd9.2.5 == 4511) {
                  v.code <- 58530
                }
                if (icd9.2.5 == 4512) {
                  v.code <- 58530
                }
                if (icd9.2.5 == 4270) {
                  v.code <- 7022
                }
                if ((icd9.2.5 >= 8530) & (icd9.2.5 <= 8539)) {
                  v.code <- 2780
                }
                if ((icd9.2.5 >= 8541) & (icd9.2.5 <= 8545)) {
                  v.code <- 2780
                }
                if (icd9.2.5 == 8554) {
                  v.code <- 2780
                }
                return(v.code)
            }
            if (is.na(icd.code)) {
                icd.code <- "00000"
            }
            icd9.1 <- substr(icd.code, 1, 1)
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
            if (icd9.1 == "V") {
                icd9.result <- process.v.codes(icd9.result)
            }
            return(as.numeric(icd9.result)/100)
        }
        n.rows <- length(input.frame[, 1])
        n.cols <- length(input.frame[1, ])
        output.frame <- matrix(0, nrow = n.rows, ncol = n.cols)
        for (i in 1:n.rows) {
            for (j in 1:n.cols) {
                output.frame[i, j] <- ICD9.5digit.ahrq(input.frame[i, 
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
    points.ahrq <- function(input.frame) {
        chf <- c(398.91, seq(from = 428, to = 428.9, by = 0.01), 
            402.01, 402.11, 402.91, 404.01, 404.11, 404.91, 404.03, 
            404.13, 404.93)
        valve <- c(seq(from = 93.2, to = 93.24, by = 0.01), seq(from = 394, 
            to = 397.1, by = 0.01), 397.9, seq(from = 424, to = 424.99, 
            by = 0.01), seq(from = 746.3, to = 746.6, by = 0.01))
        pulm.circ <- c(seq(from = 415.11, to = 415.19, by = 0.01), 
            seq(from = 416, to = 416.9, by = 0.01), 417.9)
        pvd <- c(seq(from = 440, to = 440.9, by = 0.01), seq(from = 440, 
            to = 441.9, by = 0.01), seq(from = 442, to = 442.9, 
            by = 0.01), seq(from = 443.1, to = 443.9, by = 0.01), 
            444.21, 441.22, 447.1, 449, 557.1, 557.9)
        htn <- c(401.1, 401.9, seq(from = 642, to = 642.04, by = 0.01), 
            401, 437.2, seq(from = 642.2, to = 642.24, by = 0.01), 
            402, 402.1, 402.9, 405.09, 405.19, 405.99, 402.01, 
            402.11, 402.91, 403, 403.1, 403.9, 405.01, 405.11, 
            405.91, seq(from = 642.1, to = 642.14, by = 0.01), 
            403.01, 403.11, 403.91, 404, 404.1, 404.9, 404.01, 
            404.11, 404.91, 404.02, 404.12, 404.92, 404.03, 404.13, 
            404.93, seq(from = 642.7, to = 642.74, by = 0.01), 
            seq(from = 642.9, to = 642.94, by = 0.01))
        paralysis <- c(seq(from = 342, to = 344.9, by = 0.01), 
            seq(from = 438.2, to = 438.53, by = 0.01), 780.72)
        neuro.other <- c(seq(from = 330, to = 331.9, by = 0.01), 
            332, 333.4, 333.5, 333.7, 333.71, 333.72, 333.79, 
            333.85, 333.94, seq(from = 334, to = 335.9, by = 0.01), 
            338, 340, seq(from = 341.1, to = 341.9, by = 0.01), 
            seq(from = 345, to = 345.11, by = 0.01), seq(from = 345.2, 
                to = 345.3, by = 0.01), seq(from = 345.4, to = 345.91, 
                by = 0.01), 347, 347.01, 347.1, 347.11, seq(from = 649.4, 
                to = 649.44, by = 0.01), 786.7, seq(from = 786.7, 
                to = 786.73, by = 0.01), 780.3, 780.31, 780.32, 
            780.39, 780.97, 784.3)
        chronic.pulm <- c(seq(from = 490, to = 492.8, by = 0.01), 
            seq(from = 493, to = 493.92, by = 0.01), seq(from = 494, 
                to = 494.1, by = 0.01), seq(from = 495, to = 505, 
                by = 0.01), 506.4)
        dm.uncomp <- c(seq(from = 250, to = 250.33, by = 0.01), 
            seq(from = 648, to = 648.04, by = 0.01), seq(from = 249, 
                to = 249.31, by = 0.01))
        dm.comp <- c(seq(from = 250.4, to = 250.93, by = 0.01), 
            775.1, seq(from = 249.4, to = 249.91, by = 0.01))
        hypothyroid <- c(seq(from = 243, to = 244.2, by = 0.01), 
            244.8, 244.9)
        renal <- c(585.3, 585.4, 585.5, 585.6, 585.9, 403.01, 
            403.11, 403.91, 404.02, 404.12, 404.92, 404.03, 404.13, 
            404.93)
        liver <- c(70.22, 70.23, 70.32, 70.33, 70.44, 70.54, 
            456, 456.1, 456.2, 456.21, 571, 571.2, 571.3, seq(from = 571.4, 
                to = 571.49, by = 0.01), 571.5, 571.6, 571.8, 
            571.9, 572.3, 572.8)
        pud <- c(531.41, 531.51, 531.61, 531.7, 531.71, 531.91, 
            532.41, 532.51, 532.61, 532.7, 532.71, 532.91, 533.41, 
            533.51, 533.61, 533.7, 533.71, 533.91, 534.41, 534.51, 
            534.61, 534.7, 534.71, 534.91)
        hiv <- c(seq(from = 42, to = 44.9, by = 0.01))
        lymphoma <- c(seq(from = 200, to = 202.38, by = 0.01), 
            seq(from = 202.5, to = 203.01, by = 0.01), 238.6, 
            273.3, seq(from = 203.02, to = 203.82, by = 0.01))
        mets <- c(seq(from = 196, to = 199.1, by = 0.01), seq(from = 209.7, 
            to = 209.75, by = 0.01), 209.79, 789.51)
        solid.tumor <- c(seq(from = 140, to = 172.9, by = 0.01), 
            seq(from = 174, to = 175.9, by = 0.01), seq(from = 179, 
                to = 195.8, by = 0.01), seq(from = 209, to = 209.24, 
                by = 0.01), seq(from = 209.25, to = 209.3, by = 0.01), 
            seq(from = 209.31, to = 209.36, by = 0.01), seq(from = 258.01, 
                to = 258.03, by = 0.01))
        rheum <- c(701, seq(from = 710, to = 710.9, by = 0.01), 
            seq(from = 714, to = 714.9, by = 0.01), seq(from = 720, 
                to = 720.9, by = 0.01), 725)
        coag <- c(seq(from = 286, to = 286.9, by = 0.01), 287.1, 
            seq(from = 287.3, to = 287.5, by = 0.01), seq(from = 649.3, 
                to = 649.34, by = 0.01), 289.84)
        obesity <- c(278, 278.01, 278.03, seq(from = 649.1, to = 649.14, 
            by = 0.01), 793.91)
        wt.loss <- c(seq(from = 260, to = 263.9, by = 0.01), 
            783.21, 783.22)
        lytes <- c(seq(from = 276, to = 276.9, by = 0.01))
        anemia.loss <- c(280, seq(from = 648.2, to = 648.24, 
            by = 0.01))
        anemia.def <- c(seq(from = 280.1, to = 281.9, by = 0.01), 
            seq(from = 285.21, to = 285.29, by = 0.01), 285.9)
        etoh <- c(seq(from = 291, to = 291.3, by = 0.01), 291.5, 
            291.8, 291.81, 291.82, 291.89, 291.9, seq(from = 303, 
                to = 303.93, by = 0.01), seq(from = 305, to = 305.03, 
                by = 0.01))
        drugs <- c(292, seq(from = 292.82, to = 292.89, by = 0.01), 
            292.9, seq(from = 304, to = 304.93, by = 0.01), seq(from = 305.2, 
                to = 305.93, by = 0.01), seq(from = 648.3, to = 648.34, 
                by = 0.01))
        psychoses <- c(seq(from = 295, to = 298.9, by = 0.01), 
            299.1, 299.11)
        depression <- c(300.4, 301.12, 309, 309.1, 311)
        ahrq.list <- list(chf, valve, pulm.circ, pvd, htn, paralysis, 
            neuro.other, chronic.pulm, dm.uncomp, dm.comp, hypothyroid, 
            renal, liver, pud, hiv, lymphoma, mets, solid.tumor, 
            rheum, coag, obesity, wt.loss, lytes, anemia.loss, 
            anemia.def, etoh, drugs, psychoses, depression)
        n.rows <- length(input.frame[, 1])
        n.cols <- length(input.frame[1, ])
        output.frame <- matrix(0, nrow = n.rows, ncol = 29)
        for (i in 1:n.rows) {
            for (j in 1:n.cols) {
                for (k in 1:length(ahrq.list)) {
                  for (m in 1:length(ahrq.list[[k]])) {
                    if (input.frame[i, j] == ahrq.list[[k]][m]) {
                      output.frame[i, k] <- 1
                    }
                  }
                }
            }
        }
        for (i in 1:length(output.frame[, 1])) {
            if (output.frame[i, 10] == 1) {
                output.frame[i, 9] <- 0
            }
            if (output.frame[i, 17] == 1) {
                output.frame[i, 18] <- 0
            }
        }
        output.frame <- as.data.frame(output.frame)
        colnames(output.frame) <- c("CHF", "VALVE", "PULM.CIRC", 
            "PVD", "HTN", "PARALYSIS", "NEURO.OTHER", "CHRONIC.PULM", 
            "DM.UNCOMP", "DM.COMP", "HYPOTHYROID", "RENAL", "LIVER", 
            "PUD", "HIV", "LYMPHOMA", "METS", "SOLID.TUMOR", 
            "RHEUM", "COAG", "OBESITY", "WT.LOSS", "LYTES", "ANEMIA.LOSS", 
            "ANEMIA.DEF", "ETOH", "DRUGS", "PSYCHOSES", "DEPRESSION")
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
    interim.frame.1 <- apply.icd9.ahrq(input.frame)
    interim.frame.2 <- apply.convert.na(interim.frame.1)
    interim.frame.3 <- points.ahrq(interim.frame.2)
    POINTS <- total.points(interim.frame.3)
    elixhauser.data <- list(POINTS, interim.frame.3)
    names(elixhauser.data) <- c("COMORBIDITY.CT", "COMORBIDITIES")
    return(elixhauser.data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }

