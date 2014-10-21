#' Score points for Charlson's original comorbidity weights
#'
#' The following function adds the weights of the Charlson
#' original weighting system where comorbidities were score
#' between 1 and 6.
#'
#' @param ds This is a \code{data.frame} or \code{matrix} with
#'  the comorbidity groups as returned by any of the Charlson functions
#' @return Weighted comorbidities
#' @examples
#' out <- cmrbdt.finder.regex.charlson_Quan2005("I252")
#' weight.Charlsons.org(out)
#' @export
#' @family weight functions
weight.Charlsons.org <- function(ds) {
  ds <- ds*1
  # Set all columns that have 2 points
  for (var in c("PLEGIA", "DM.COMP",
                "MALIGNANCY", "RENAL")){
    multiplier <- 2
    if (is.null(dim(ds))){
      ds[var] <- ds[var] * multiplier
    }else{
      ds[,var] <- ds[,var] * multiplier
    }
  }

  # Set all columns that have 3 points
  for (var in c("SEVERE.LIVER")){
    multiplier <- 3
    if (is.null(dim(ds))){
      ds[var] <- ds[var] * multiplier
    }else{
      ds[,var] <- ds[,var] * multiplier
    }
  }

  # Set all columns that have 6 points
  for (var in c("METASTASIS", "HIV")){
    multiplier <- 6
    if (is.null(dim(ds))){
      ds[var] <- ds[var] * multiplier
    }else{
      ds[,var] <- ds[,var] * multiplier
    }
  }

  return(ds)
}


#' Score points for Quan's updated weight for the Charlson index
#'
#' The following function adds the weights of the Charlson
#' according to an update in 2011 by Quan et al where the maximum
#' score is 24 and the weights are between 0-6, i.e. a few comorbidities
#' are no longer considered of importance (MI, PVD, etc.)
#'
#' @param ds This is a \code{data.frame} or \code{matrix} with
#'  the comorbidity groups as returned by any of the Charlson functions
#' @return Weighted comorbidities
#' @references H. Quan, B. Li, C. M. Couris, K. Fushimi, P. Graham, P. Hider, J.-M.
#'  Januel, and V. Sundararajan, “Updating and Validating the Charlson Comorbidity
#'  Index and Score for Risk Adjustment in Hospital Discharge Abstracts Using Data
#'  From 6 Countries,” Am. J. Epidemiol., vol. 173, no. 6, pp. 676–682, Mar. 2011.
#' @examples
#' out <- cmrbdt.finder.regex.charlson_Quan2005("I252")
#' weight.Charlsons.Quan2011(out)
#' @export
#' @family weight functions
weight.Charlsons.Quan2011 <- function(ds){
  ds <- ds*1
  # Set all columns that have 0 points
  for (var in c("MI", "PVD", "CVD", 
                "PUD", "DM")){
    multiplier <- 0
    if (is.null(dim(ds))){
      ds[var] <- ds[var] * multiplier
    }else{
      ds[,var] <- ds[,var] * multiplier
    }
  }

  # Scores with 1
  #c("COPD", "RHEUM", "DM.COMP")

  # Set those with 2 points
  for (var in c("MALIGNANCY", "MILD.LIVER", 
                "CHF", "DEMENTIA", "PLEGIA")){
    multiplier <- 2
    if (is.null(dim(ds))){
      ds[var] <- ds[var] * multiplier
    }else{
      ds[,var] <- ds[,var] * multiplier
    }
  }


  for (var in c("SEVERE.LIVER", "HIV")){
    multiplier <- 4
    if (is.null(dim(ds))){
      ds[var] <- ds[var] * multiplier
    }else{
      ds[,var] <- ds[,var] * multiplier
    }
  }

  for (var in c("METASTASIS")){
    multiplier <- 6
    if (is.null(dim(ds))){
      ds[var] <- ds[var] * multiplier
    }else{
      ds[,var] <- ds[,var] * multiplier
    }
  }

  return(ds)
}

#' Score points for van Walraven et al's weighting of the Elixhausers index
#'
#' The following function adds the weights of the Elixhausers index
#' according to an article by van Walraven et al. Note that a few disease categories
#' have a negative weight, i.e. are protective according to their article.
#'
#' @param ds This is a \code{data.frame} or \code{matrix} with
#'  the comorbidity groups as returned by the Elixhauser function
#' @return Weighted comorbidities
#' @references C. van Walraven, P. C. Austin, A. Jennings, H. Quan,
#'  and A. J. Forster, "A Modification of the Elixhauser Comorbidity
#'  Measures Into a Point System for Hospital Death Using Administrative
#'  Data" Medical Care, vol. 47, no. 6, pp. 626-633, Jun. 2009.
#' @examples
#' out <- cmrbdt.finder.regex.elixhauser_Quan2005("C69")
#' weight.Elixhausers.VanWalraven2009(out)
#' @export
#' @family weight functions
weight.Elixhausers.VanWalraven2009 <- function(ds){
  ds <- ds*1
  weights <- list(
    `-1` = c(# Valvular disease -1
      "VALVE"),
    `2` = c(# Peripheral vascular disorders 2
      "PVD",
      # Blood loss anemia 2
      "ANEMIA.LOSS",
      # Deficiency anemia 2
      "ANEMIA.DEF"),
    `3` = c(# Chronic pulmonary disease 3
      "CHRONIC.PULM",
      # Coagulopathy 3
      "COAG",
      # Depression 3
      "DEPRESSION"),
    `4` = c(# Pulmonary circulation disorders 4
      "PULM.CIRC",
      # Solid tumour without metastasis 4
      "SOLID.TUMOR",
      # Obesity 4
      "OBESITY"),
    `5` = c(# Cardiac arrhythmias 5
      "ARRHYTHMIA",
      # Renal failure 5
      "RENAL",
      # Fluid and electrolyte disorders 5
      "LYTES"),
    `6` = c(# Neurodegenerative disorders 6
      "NEURO.OTHER",
      # Weight loss 6
      "WT.LOSS"),
    `7` = c(# Congestive heart 7
      "CHF",
      # Paralysis  7
      "PARALYSIS",
      # Drug abuse 7
      "DRUGS"),
    `9` = c(# Lymphoma 9
      "LYMPHOMA"),
    `11` = c(# Liver disease 11
      "LIVER"),
    `12` = c(# Metastatic cancer 12
      "METS"))

  # Apply the weights
  for (sn in names(weights)){
    weight <- as.double(sn)
    for (var_name in weights[[sn]]){
      if (is.null(dim(ds))){
        ds[var_name] <- ds[var_name]*weight
      }else{
        ds[,var_name] <- ds[,var_name]*weight
      }
    }
  }

  return(ds)
}