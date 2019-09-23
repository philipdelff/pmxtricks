##' A very simple simulated PK dataset
##'
##' This dataset is mainly used to illustrate functionality of functions in the package.
##' @format A data.table with 76 rows, 6 columns. 4 subjects, 19 observations each.
##' \describe{
##' \item{ROW}{A row identifier}
##' \item{ID}{Subject ID.}
##' \item{TIME}{Time since first dose.}
##' \item{EVID}{0 for dose.}
##' \item{CMT}{Compartment identifier. 2 for central compartment.}
##' \item{DV}{Observation value.}
##' \item{MDV}{Is observation missing?}
##' }
##' @source simulated
"pksim1"
