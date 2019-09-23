##' round to a given number of digits - and respect zeros.
##' @param x A vector to round.
##' @param digits Number of digits to
##' @family DataWrangling
##' @export
roundResp0=function(x,digits) {
    res <- formatC(signif(x,digits=digits), digits=digits,format="fg", flag="#")
    res <- sub("\\.$","",res)
    res
}
