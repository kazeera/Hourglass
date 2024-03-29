% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_objects_run_hourglass.R
\name{get_feat_sets}
\alias{get_feat_sets}
\title{Read in and format feat_sets.}
\usage{
get_feat_sets(
  file_xl,
  sets_sheet = "FeatureSets",
  params_sheet = "FeatureParameters"
)
}
\arguments{
\item{file_xl}{The name of file path to an Excel file for custom analyses.}

\item{sets_sheet}{The name of the worksheet in file_xl containing FeatureSets.}

\item{params_sheet}{The name of the worksheet in file_xl containing matching FeatureParameters.}
}
\value{
feat_sets object = list with 2 elements - data frames: 1) sets, 2) params (for "feature parameters") See documentation for more info.
}
\description{
Create feat_sets variable for run_hourglass function from input Excel file.
}
