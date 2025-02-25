% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_drug_poisoning_data.R
\name{process_drug_poisoning_data}
\alias{process_drug_poisoning_data}
\title{Process drug poisoning data}
\usage{
process_drug_poisoning_data(
  data,
  date_of = "occurrence",
  years = NULL,
  groups = NULL,
  drug_groups = "total",
  misuse_only = TRUE
)
}
\arguments{
\item{data}{a data frame, the output of get_drug_poisoning_deaths()}

\item{date_of}{Either "occurrence" (year of death) or "registration" (year of registration).}

\item{years}{An integer vector of years to include in the analysis.}

\item{misuse_only}{TRUE filters deaths not recorded as related to drug misuse.}

\item{drug_group}{a string vector. "all" group by drug group, "total" total deaths drug group only, can also be a vector of any of the drug groups in the column \code{drug_group}}

\item{group_by}{Grouping variable, any of: "area", "age", "sex".}
}
\value{
A data frame with a count of drug death occurrences or registrations, aggergated by the grouping variables and filtered by the selected years.
}
\description{
Filters and groups the data and aggregates based on user-defined parameters: 'date of' years, and grouping columns (age, sex, area, and drug group, and misuse).
}
\details{
Date of (\code{date_of = "occurrence"})
The data has both the year-month of death ("occurrence") and of registration of the death. Registration is preferable when looking for trends that include the latest dates available due to registration lag, In most other contexts occurrence is preferable.

Misuse (\code{misuse_only = TRUE})
Each observation is flagged as being recorded by the ONS as "related to drug misuse" or not. Set \code{misuse = TRUE} to filter out deaths that the ONS did not record as related to drug misuse.
}
