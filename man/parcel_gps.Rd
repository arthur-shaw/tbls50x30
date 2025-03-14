% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_planting.R
\name{parcel_gps}
\alias{parcel_gps}
\title{Make parcel GPS measurement table}
\usage{
parcel_gps(
  parcel_df,
  cases,
  gps_var,
  not_measured_val = -9999,
  why_no_gps_var = NULL,
  group_var
)
}
\arguments{
\item{parcel_df}{Data frame of parcels.}

\item{cases}{Data frame of cases to include in analysis}

\item{gps_var}{Character. Name of column that includes GPS-measured area.}

\item{not_measured_val}{Numeric. Special value that indicates area was not measured.}

\item{why_no_gps_var}{Character. Name of column that captures reason why no GPS measurement was done.}

\item{group_var}{Character. Name of grouping variable column (e.g., team, region, etc.).}
}
\value{
{gt} table object
}
\description{
Make parcel GPS measurement table
}
