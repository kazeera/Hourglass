% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colors.R
\name{clean_colors}
\alias{clean_colors}
\title{Cleans and returns color palette.}
\usage{
clean_colors(pal, to_remove = "#ffffffff", is_df = T)
}
\arguments{
\item{pal}{Either 1) Data frame, where column 1 is variables and column 2 is hex codes, 2) List object indicating color palette, where elements are hex codes and element names are variables. e.g. list("Tissue.type-Tumour"="#2f4f4Fff", "Tissue.type-Stroma"="#d2691eff")}

\item{to_remove}{Vector of hex codes to remove from palette, default is white ("#ffffffff"(,)}

\item{is_df}{Boolean. Is the "pal" value a data frame object?}
}
\value{
List of colors, where elements are hex codes and element names are variables. As a result of "cleaning", element names don't have 2 parts, are duplicated, and is white e.g. list("Tumour"="#2f4f4Fff", "Stroma"="#d2691eff")
}
\description{
Cleans color palette object for use by run_hourglass function.
}
