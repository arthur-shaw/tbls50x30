% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_planting.R
\name{crops_per_plot}
\alias{crops_per_plot}
\title{Make crops per plot table}
\usage{
crops_per_plot(crop_df, cases, parcel_id_var, plot_id_var, group_var)
}
\arguments{
\item{crop_df}{Data frame of parcel-plot-crops}

\item{cases}{Data frame of cases to include in analysis.}

\item{parcel_id_var}{Character. Name of parcel ID column.}

\item{plot_id_var}{Character. Name of plot ID column.}

\item{group_var}{Character. Name of grouping variable column (e.g., team, region, etc.).}
}
\value{
{gt} table object
}
\description{
Make crops per plot table
}
