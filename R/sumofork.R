##' Platform independent sumo
##'
##' @param model A .mod, .lst, .ext or .cov file.
##' @param tableType Passed to extTransform
##' @param format.estimate How to format the estimated parameter values.
##' @param format.rse How to format the relative standard error estimates.
##' @details This is a fork of nonmem2R::sumo with very little
##'     modification. Many thanks to Magnus for a great job!
##' @family Nonmem
##' @import nonmem2R

sumofork <- function (model, tableType = 2, format.estimate = "% -#6.4g", 
                      format.rse = "%#6.3g") 
{
    file.path <- ""
    model.path.ok <- FALSE
    if (exists("model.path")) {
        eval(parse(text = "model.path.ok<-dir.exists(model.path)"))
        if (model.path.ok) {
            eval(parse(text = "file.path<-model.path"))
        }
    }
    if (substr(model, nchar(model) - 3, nchar(model)) %in% c(".ext", 
                                                             ".cov", ".lst", ".mod")) {
        model = substr(model, 1, nchar(model) - 4)
    }
    lst.file <- paste(file.path, model, ".lst", sep = "")

    d0 <- read.table(file = lst.file, sep = "?", comment.char = "", 
                     stringsAsFactors = F)[, 1]
    minSuccessful <- suppressWarnings(length(grep("0MINIMIZATION SUCCESSFUL", 
                                                  d0)) > 0)
    roundingErrors <- suppressWarnings(length(grep("DUE TO ROUNDING ERRORS", 
                                                   d0)) > 0)
    k1 <- suppressWarnings(max(grep(" PARAMETER:", d0)))
    k2 <- suppressWarnings(max(grep(" GRADIENT", d0)))
    tmp <- gsub("GRADIENT:", "", d0[k2:(k2 + k2 - k1 - 1)])
    tmp0 <- paste(tmp, collapse = " ")
    tmp1 <- gsub("^\\s+|\\s+$", "", tmp0)
    tmp2 <- gsub(" +", " ", tmp1)
    final.Grad <- as.numeric(strsplit(tmp2, split = " ")[[1]])
    finalZeroGradient <- sum(final.Grad == 0)
    resetHessian <- suppressWarnings(length(grep("RESET HESSIAN", 
                                                 d0)))
    k <- suppressWarnings(grep("0COVARIANCE STEP OMITTED:", 
                               d0))
    covRun <- gsub(" +", " ", d0[k]) == "0COVARIANCE STEP OMITTED: NO"
    covSuccessful <- suppressWarnings(length(grep("0COVARIANCE STEP ABORTED", 
                                                  d0)) == 0) & suppressWarnings(length(grep("STANDARD ERROR OF ESTIMATE", 
                                                                                            d0)) > 0)
    digitsFinalEst <- NA
    k <- suppressWarnings(grep("DIGITS IN FINAL EST", d0))
    if (length(k) > 0) {
        digitsFinalEst <- as.numeric(substr(d0[k], 35, 46))
    }
    k <- suppressWarnings(grep("TOT. NO. OF OBS RECS:", d0))
    totNoOfObservations <- as.numeric(substr(d0[k], 25, 46))
    k <- suppressWarnings(grep("TOT. NO. OF INDIVIDUALS:", 
                               d0))
    totNoOfIndividuals <- as.numeric(substr(d0[k], 26, 46))
    etaShrink <- NA
    epsShrink <- NA
    k <- suppressWarnings(grep("ETAshrink(", d0, fixed = TRUE))
    if (length(k) > 0 & nchar(d0[k]) > 20) {
        tmp0 <- substr(d0[k], 15, 300)
        tmp1 <- gsub("^\\s+|\\s+$", "", tmp0)
        tmp2 <- gsub(" +", " ", tmp1)
        etaShrink <- as.numeric(strsplit(tmp2, split = " ")[[1]])
    }
    k <- suppressWarnings(grep("EPSshrink(", d0, fixed = TRUE))
    if (length(k) > 0 & nchar(d0[k]) > 20) {
        tmp0 <- substr(d0[k], 15, 300)
        tmp1 <- gsub("^\\s+|\\s+$", "", tmp0)
        tmp2 <- gsub(" +", " ", tmp1)
        epsShrink <- as.numeric(strsplit(tmp2, split = " ")[[1]])
    }
    Ext <- nonmem2R::extload(model)
    Cov <- diag(0, length(Ext$theta))
    conditionNumber <- NA
    if (covSuccessful) {
        Cov <- covload(model, theta.only = FALSE)
        ii <- diag(Cov) > 0
        cor2 <- cov2cor(Cov[ii, ii])
        eigenvals <- eigen(cor2, TRUE, only.values = TRUE)$values
        conditionNumber <- (max(eigenvals)/min(eigenvals))
    }
    k <- suppressWarnings(grep("#OBJV:", d0))
    OFV <- as.numeric(gsub("*", "", substr(d0[k], 8, 200), 
                           fixed = TRUE))
    y1 <- extTransform(Ext, type = tableType)
    coef <- extFormat(y1, format.estimate = format.estimate, 
                      format.rse = format.rse)
    res <- list(model = paste(file.path, model, sep = ""), 
                minSuccessful = minSuccessful, roundingErrors = roundingErrors, 
                finalZeroGradient = finalZeroGradient, resetHessian = resetHessian, 
                covRun = covRun & minSuccessful, covSuccessful = covSuccessful, 
                digitsFinalEst = digitsFinalEst, totNoOfObservations = totNoOfObservations, 
                totNoOfIndividuals = totNoOfIndividuals, OFV = OFV, 
                conditionNumber = conditionNumber, etaShrink = etaShrink, 
                epsShrink = epsShrink, Ext = Ext, Cov = Cov, coef = coef)
    class(res) <- append(class(res), "sumoR")
    res
}

