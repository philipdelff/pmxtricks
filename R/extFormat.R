##' there seems to be a bug in nonmem2R::extFormat. This function tries to solve that.
##' @description The code is copied from nonmem2R and only slightly modified.
##' @param ext the result ofname of the lst file without the .lst extension.
##' @param format.estimate format for estimated value, passed to sprintf
##' @param format.rse format for rse, passed to sprintf
##' @family Nonmem


##        y[i, 1] <- sprintf("%3d", res.table[i, 1])
##         y[i, 1] <- sprintf("%3.0f", res.table[i, 1])

extFormat <- function (ext, format.estimate = "% -#6.4g", format.rse = "%#6.3g") 
{
    to.vec <- function(x, prefix = "rc") {
        kk <- lower.tri(x, diag = T)
        rr <- matrix(rep(1:ncol(x), ncol(x)), ncol = ncol(x))[kk]
        cc <- matrix(rep(1:ncol(x), each = ncol(x)), ncol = ncol(x))[kk]
        xV <- x[kk]
        names(xV) <- paste(prefix, rr, cc, "", sep = ".")
        xV
    }
    ext$omega <- to.vec(ext$omega, prefix = "OMEGA")
    ext$omega.sd <- to.vec(ext$omega.sd, prefix = "OMEGA")
    ext$sigma <- to.vec(ext$sigma, prefix = "SIGMA")
    ext$sigma.sd <- to.vec(ext$sigma.sd, prefix = "SIGMA")
    thetaii <- !(names(ext$theta) %in% ext$fix)
    omegaii <- !(names(ext$omega) %in% ext$fix)
    sigmaii <- !(names(ext$sigma) %in% ext$fix)
    np <- max(c(sum(thetaii), sum(omegaii), sum(sigmaii)))
    res.table <- matrix(NA, nrow = np, ncol = 9)
    if (sum(thetaii) > 0) {
        res.table[1:sum(thetaii), 1] <- which(thetaii)
        res.table[1:sum(thetaii), 2] <- ext$theta[thetaii]
        res.table[1:sum(thetaii), 3] <- ext$theta.sd[thetaii]
    }
    tmp <- gsub("OMEGA.", "", names(ext$omega)[omegaii])
    omega.index <- substr(tmp, 1, nchar(tmp) - 1)
    if (length(tmp) > 0) {
        res.table[1:sum(omegaii), 4] <- as.numeric(omega.index)
        res.table[1:sum(omegaii), 5] <- ext$omega[omegaii]
        res.table[1:sum(omegaii), 6] <- ext$omega.sd[omegaii]
    }
    tmp <- gsub("SIGMA.", "", names(ext$sigma)[sigmaii])
    sigma.index <- substr(tmp, 1, nchar(tmp) - 1)
    if (length(tmp) > 0) {
        res.table[1:sum(sigmaii), 7] <- as.numeric(sigma.index)
        res.table[1:sum(sigmaii), 8] <- ext$sigma[sigmaii]
        res.table[1:sum(sigmaii), 9] <- ext$sigma.sd[sigmaii]
    }
    y <- matrix("", nrow(res.table), ncol(res.table))
    colnames(y) <- c("", "  Theta", "", "", "  Omega", "", "", 
        "Sigma", "")
    rownames(y) <- rep("", nrow(y))
    for (i in 1:nrow(res.table)) {
        for (j in c(2, 3, 5, 6, 8, 9)) {
            if (!is.na(res.table[i, j])) {
                if (j %in% c(3, 6, 9)) {
                  y[i, j] <- sprintf(format.rse, res.table[i, 
                    j])
                  y[i, j] <- paste("(", y[i, j], ")", sep = "")
                }
                else {
                  y[i, j] <- sprintf(format.estimate, res.table[i, 
                    j])
                }
            }
        }
        y[i, 1] <- sprintf("%3.0f", res.table[i, 1])
    }
    ii <- !is.na(res.table[, 2]) & is.na(res.table[, 3])
    y[ii, 3] <- "(...)"
    ii <- !is.na(res.table[, 5]) & is.na(res.table[, 6])
    y[ii, 6] <- "(...)"
    ii <- !is.na(res.table[, 8]) & is.na(res.table[, 9])
    y[ii, 6] <- "(...)"
    if (length(omega.index) > 0) {
        ii <- 1:length(omega.index)
        y[ii, 4] <- gsub(".", ",", paste("  [", omega.index, 
            "]", sep = ""), fixed = TRUE)
    }
    if (length(sigma.index) > 0) {
        ii <- 1:length(sigma.index)
        y[ii, 7] <- gsub(".", ",", paste("  [", sigma.index, 
            "]", sep = ""), fixed = TRUE)
    }
    y
}
