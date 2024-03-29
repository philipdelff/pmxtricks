% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_thalf.R
\name{thalf_trans_1c}
\alias{thalf_trans_1c}
\title{Thalf for 1st order models, single-depot abs through any number of transit compartments.}
\usage{
thalf_trans_1c(pars, cl = "CL", ka = "KA1", vc = "VC", debug = F)
}
\arguments{
\item{pars}{A parameter table. Thalf will be calculated for all rows.}

\item{cl}{The name of the column with clearance values. Can represent CL/F if volume variables do the same.}

\item{ka}{The name of the column representing absorption rate.}

\item{vc}{The name of the column with central volume (see cl as well).}

\item{debug}{Start by calling browser()?}
}
\description{
Thalf for 1st order models, single-depot abs through any number of transit compartments.
}
\seealso{
Other Calc: 
\code{\link{thalf_1a1c}()},
\code{\link{thalf_1a2c}()}
}
\concept{Calc}
