% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_HealthCare2.R
\name{encode_HealthCare2}
\alias{encode_HealthCare2}
\alias{decode_HealthCare2}
\title{Encode the Healthcareworker columns}
\usage{
encode_HealthCare2(x)

decode_HealthCare2(x, incl_broad = FALSE)
}
\arguments{
\item{x}{For encode, the character vector of \code{HealthcareWorker}. For
decode, the vector so encoded.}

\item{incl_broad}{If \code{FALSE}, the default, only \code{HealthcareWorker}
will be returned; if \code{TRUE}, only \code{HealthcareWorkerBroad} is returned.
If \code{NA} a list of both is returned.}
}
\description{
Since the \code{HealthcareWorkerBroad} column is entirely defined
 by \code{HealthcareWorker}, this encoding only uses \code{HealthcareWorker},
 but provides decoding for both columns.
}
