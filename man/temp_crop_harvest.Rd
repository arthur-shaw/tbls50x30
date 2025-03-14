% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_harvest.R
\name{temp_crop_harvest}
\alias{temp_crop_harvest}
\title{Create temporary crop harvest table}
\usage{
temp_crop_harvest(
  parcel_plot_crop_df,
  cases,
  crop_id_var,
  crop_vals,
  harvest_var,
  harvest_val = 1,
  why_not_harvest_var,
  group_var,
  json_qnr_path
)
}
\arguments{
\item{parcel_plot_crop_df}{Data frame of parcel-plot-crop-level observations.}

\item{cases}{Data frame of cases to include in analysis. Data frame must contain \code{interview__id} and the grouping variable indicated in the \code{group_var} parameter}

\item{crop_id_var}{Atomic character vector. Name of crop ID variable as a character.}

\item{crop_vals}{Numeric vector. Codes of the crops that are temporary crops.}

\item{harvest_var}{Atomic character vector. Name of variable indicating whether the crop was harvested.}

\item{harvest_val}{Atomic numeric vector. Value of \code{harvest_var} that indicates the crop was harvested.}

\item{why_not_harvest_var}{Atomic character vector. Name of variable, as it appears in Designer, that captures the reason(s) the crop was not harvested.}

\item{group_var}{Atomic character vector. Name of the grouping variable}

\item{json_qnr_path}{Character. Path to JSON file that describes the questionnaire.}
}
\value{
{gt} table object
}
\description{
Create temporary crop harvest table
}
