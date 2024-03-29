% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_form_names.R
\name{add_form_names}
\alias{add_form_names}
\title{Add form names to long format RC data}
\usage{
add_form_names(
  long_data,
  pooled_vars = NULL,
  data_dict = getOption("redcap_bundle")$data_dict,
  mappings = getOption("redcap_bundle")$mappings,
  id_field = getOption("redcap_bundle")$id_field
)
}
\arguments{
\item{long_data}{Dataframe. A long format data frame, as created by
\code{rc_missing}. At minimum there should be a record_id column, a
redcap_event_name column, and a column containing variable names.
Wide-to-long conversion functions include reshape2::melt()
(used in rctools), tidyr::gather(), and tidyr::pivot_longer().}

\item{pooled_vars}{Dataframe. Dataframe attribute appended to record_data by
\code{pooled_vars}.}

\item{data_dict}{Dataframe. REDCap project data data_dictionary. By default,
this will be fetched from the REDCap bundle option, as created by \code{rc_bundle}.
Otherwise, a data.frame containing the project data dictionary must be supplied.}

\item{mappings}{Dataframe. Redcap metadata that maps forms to events.}

\item{id_field}{Character. Field name corresponding to the 'record_id' field.}
}
\description{
Adds a form name column, taking pooled data into account.
This is an internal function only
}
\author{
Marcus Lehr
}
