% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trial_sequence.R
\name{set_expansion_options}
\alias{set_expansion_options}
\alias{set_expansion_options,trial_sequence_ITT-method}
\alias{set_expansion_options,trial_sequence_PP-method}
\title{Set expansion options}
\usage{
set_expansion_options(object, ...)

\S4method{set_expansion_options}{trial_sequence_ITT}(
  object,
  output,
  chunk_size,
  first_period = 0,
  last_period = Inf
)

\S4method{set_expansion_options}{trial_sequence_PP}(
  object,
  output,
  chunk_size,
  first_period = 0,
  last_period = Inf
)

\S4method{set_expansion_options}{trial_sequence_ITT}(
  object,
  output,
  chunk_size,
  first_period = 0,
  last_period = Inf
)
}
\arguments{
\item{object}{A \link{trial_sequence} object}

\item{...}{Arguments used in methods}

\item{output}{A \link[=te_datastore-class]{te_datastore} object as created by a \verb{save_to_*} function.}

\item{chunk_size}{An integer specifying the number of patients to include in each expansion iteration}

\item{first_period}{An integer specifying the first period to include in the expansion}

\item{last_period}{An integer specifying the last period to include in the expansion}
}
\value{
\code{object} is returned with \verb{@expansion} set
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
output_dir <- file.path(tempdir(check = TRUE), "expanded_data")
ITT_trial <- trial_sequence("ITT") |>
  set_data(data = data_censored) |>
  set_expansion_options(output = save_to_csv(output_dir), chunk_size = 500)

# Delete directory
unlink(output_dir, recursive = TRUE)
}
\seealso{
Other save_to: 
\code{\link{save_to_csv}()},
\code{\link{save_to_datatable}()},
\code{\link{save_to_duckdb}()}
}
\concept{save_to}
