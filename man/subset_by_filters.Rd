% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset.R
\name{subset_by_filters}
\alias{subset_by_filters}
\title{Subsets a dataframe rows based on filters}
\usage{
subset_by_filters(df, filters, delim = ";")
}
\arguments{
\item{df}{A dataframe}

\item{filters}{A string in the form of filters delimited by default ";". Each filter has 3 parts: 1) column name in df, 2) operator either != or ==, 3) value in column to exclude/include}

\item{delim}{A string/character to seperate individuals filter by, default is ";"}
}
\value{
logical vector of length = nrow(df) indicating rows to keep with respect to inclusion/exclusion criteria
}
\description{
Subsets a dataframe rows based on filters
}
\details{
subset_by_filters(df, "Smoker==Yes;Cancer.subtype!=NA") # positively select for smokers and remove NA from Cancer.subtype column
}
