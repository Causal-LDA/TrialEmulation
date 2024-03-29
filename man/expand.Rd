% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_extension.R
\name{expand}
\alias{expand}
\title{Expand Function}
\usage{
expand(
  sw_data,
  outcomeCov_var,
  where_var,
  use_censor,
  maxperiod,
  minperiod,
  keeplist
)
}
\arguments{
\item{sw_data}{datatable to expand}

\item{outcomeCov_var}{A list of individual baseline variables used in final model}

\item{where_var}{Variables used in where conditions used in subsetting the data used in final analysis (where_case),
the variables not included in the final model}

\item{use_censor}{Use censoring for per-protocol analysis - censor person-times once a person-trial stops taking the
initial treatment value}

\item{maxperiod}{Maximum period}

\item{minperiod}{Minimum period}

\item{keeplist}{A list contains names of variables used in final model}
}
\description{
This function performs the data expansion for a given dataset
}
\keyword{internal}
