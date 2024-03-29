% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newnames.R
\name{newNames}
\alias{newNames}
\title{rename list elements with standard evaluation}
\usage{
newNames(data, names, overwrite = F, skipMissing = TRUE, debug = F)
}
\arguments{
\item{data}{A list (which can be a data.frame)}

\item{names}{A list that contains elements "old" and "new" of equal length.}

\item{overwrite}{If names$new contains a name of an existing element in
data, should it be overwritten?}

\item{skipMissing}{Just skip if names refers to non-existing elements?}

\item{debug}{Start by calling browser()?}
}
\description{
rename list elements with standard evaluation
}
\details{
This function is a pmxtricks internal. When using data.table, use
    setnames instead which is based on the same idea as this.
}
\seealso{
Other DataWrangling: 
\code{\link{rbindIntersect}()},
\code{\link{rbindUnion}()},
\code{\link{roundDF}()},
\code{\link{roundResp0}()}
}
\concept{DataWrangling}
