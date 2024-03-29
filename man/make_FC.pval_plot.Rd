% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FC.pval.R
\name{make_FC.pval_plot}
\alias{make_FC.pval_plot}
\title{Plot FC-p value heatmap}
\usage{
make_FC.pval_plot(
  df,
  x_lab = "",
  y_lab = "",
  plot_title = "",
  out_dir = ".",
  pval.label = "p.signif",
  gradient_palette = "RdBu",
  group_name_sep = "/",
  trim_x = 3,
  pval_size = 8,
  pval_color = "white",
  log2FC = F,
  scale_FC = "cap_outliers",
  rescale_to = c(0, 1),
  x_axis_angle = 0,
  save.to.file = F,
  font_size = 10,
  line_size = 1,
  alphabetical_row = F
)
}
\arguments{
\item{df}{A data frame with these exact columns 1) group (ie. group comparisons on x-axis), 2) p.value, 3) Fold.change, 4) Var (y-axis), created using \code{\link{make_FC.pval_df}}.}

\item{plot_title}{Title of plot.}

\item{out_dir}{The output directory where the plot will be saved, default is current working directory.}

\item{pval.label}{How p-values are represented. Allowed values are "p.signif" (stars) and "p.format" (number).}

\item{gradient_palette}{RColorBrewer palette for fold-change values. See RColorBrewer::display.brewer.all() for all options.}

\item{group_name_sep}{A character indicating how the labels for groups should be presented, "/" in low/high}

\item{trim_x}{Number, indicating the number of characters for each part (that is, length of truncated output string).}

\item{pval_size}{Size of p-values.}

\item{pval_color}{Color of p-values.}

\item{log2FC}{Logical (TRUE/FALSE). Should log2 transformation be applied to Fold.change column before plotting?}

\item{scale_FC}{Either "scale_column", "scale_row", "none", or "cap_outliers" (default). Should scale be applied to FC for each group (column), Var (row)? In "cap_outliers", non-outliers are unscaled, but upper and lower outliers (points outside 1st/3rd quartiles respectively) become upper and lower values of range of non-outliers.}

\item{rescale_to}{A numeric vector of length 2, indicating lower and upper limits of scale. Default is 0 to 1: c(0,1). Only applied if "scale_FC" parameter is not "none".}

\item{x_axis_angle}{Angle of the x-axis label. Default is 0 (horizontal), 90 is vertical.}

\item{save.to.file}{If TRUE, save plot to file in out_dir. If FALSE, print to panel.}

\item{font_size}{The size of text labels plot. legend title. The size of plot title, axis text, legend text is font_size. The size of plot subtitle is font_size / 1.5.}

\item{line_size}{The thickness of grid lines.}

\item{alphabetical_row}{Logical; should the y axis be sorted alphabetically or preserve the order of df$Var?}

\item{xlab}{X axis label.}

\item{ylab}{Y axis label.}
}
\value{
Plot object if save.to.file is FALSE.
}
\description{
Plot grid of FC values as fill (with color scale) and p-value as stars/numbers in the centre of each tile.
}
\section{Example of input data frame}{

group   p.value       Fold.change      Var
neo/non 0.6747883 1.5617445    CD11B
neo/non 0.4820339 1.7443427      CD3
neo/non 0.9082967 1.0589647      CD4
neo/non 0.9691466 0.9896168     CD45
}

