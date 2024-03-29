% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paired.R
\name{get_paired_ds}
\alias{get_paired_ds}
\title{Finds means of pairs of duplicated ids in different groups}
\usage{
get_paired_ds(ds, rowAnn1, pair_id = 1)
}
\arguments{
\item{ds}{A dataset object (a list with vals, rowAnn, colAnn, comparison, name).}

\item{rowAnn1}{A column name in df indicating which groups to stratify by}

\item{pair_id}{Name or column index in ds$rowAnn that count as patients or groups to pair by.}
}
\value{
A new dataset object averaged for each rowAnn pair_id
}
\description{
Produces a dataset in which duplicated pair_id are averaged across different groups specified in
}
