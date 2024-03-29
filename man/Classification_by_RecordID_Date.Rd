% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classification_by_RecordID_Date.R
\name{Classification_by_RecordID_Date}
\alias{Classification_by_RecordID_Date}
\title{Determine the classification of a case on a certain date}
\usage{
Classification_by_RecordID_Date(record_id = NULL, date = NULL, EventLog)
}
\arguments{
\item{record_id}{The \code{RecordID} for which the classification is requested.}

\item{date}{The date for which the classification is requested.}

\item{EventLog}{A \code{data.table} consisting of at least the columns
\code{RecordID}, \code{EventClassification}, and \code{EventClassificationDate}.

Missing values in any of the columns are not permitted.}
}
\value{
A list of three elements:
}
\description{
Determine the classification of a case on a certain date
}
