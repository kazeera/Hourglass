% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_comparisons.R
\name{run_comparison_helper}
\alias{run_comparison_helper}
\title{Runs main analysis for all parameter combos specified in colAnns and creates plots.}
\usage{
run_comparison_helper(
  ds,
  rowAnns = 1,
  colAnns = NA,
  out_dir = ".",
  feat_sets = NULL,
  var_colors = NULL,
  gradient_palette = NULL,
  corr_method = "pearson",
  pval.test = "t.test",
  pval.label = "p.signif",
  boxplot_log10_y = F,
  paired_analysis_column = NA,
  make.QC.param = F,
  make.QC.feature = F,
  make.feat.plots = F,
  make.het.plot = F,
  make.indiv.boxplot = F,
  make.overview.boxplot = F,
  make.heatmap = F,
  make.corrplot = F,
  make.overview.corrscatt = F,
  make.indiv.corrscatt = F,
  make.barplot = F,
  make.FC.pval.plot = F,
  save_table = F
)
}
\arguments{
\item{ds}{A dataset object (a list with vals, rowAnn, colAnn, comparison, name).}

\item{rowAnns}{A character vector of 1-2 column names in ds$rowAnn. c(MainComparison, Subgroup)}

\item{colAnns}{A character vector of 1-2 column names in ds$colAnn. c(Parameter, Feature/Stain/Gene)}

\item{out_dir}{The output directory where the plot will be saved, default is current working directory.}

\item{feat_sets}{A list of 2 data frames for feature sets and parameters.}

\item{var_colors}{A named vector with colors as values and annotations/groups as names.}

\item{gradient_palette}{RColorBrewer palette name for gradients (e.g. heatmap, correlation plots). See RColorBrewer::display.brewer.all() for all options.}

\item{corr_method}{Method for correlation (one of "pearson","spearman","kendall").}

\item{pval.test}{Which two-samples testing should be used? String corresponding to "method" parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "t.test" and "wilcox.test".}

\item{pval.label}{How to display p-values? String corresponding to "label" parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "p.signif" (stars) and "p.format" (number).}

\item{boxplot_log10_y}{Log10 the values on y axis for boxplots and patient paired slopegraphs? Logical (T/F). Default is FALSE.}

\item{paired_analysis_column}{column name in ds$rowAnn to create paired analysis plots for, e.g. PatientID if ds is data for all cores}

\item{make.QC.param}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.QC.feature}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.feat.plots}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.het.plot}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.indiv.boxplot}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.overview.boxplot}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.heatmap}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.corrplot}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.overview.corrscatt}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.indiv.corrscatt}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.barplot}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{make.FC.pval.plot}{Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.}

\item{save_table}{Print MainComparison + ID data to csv file.}
}
\description{
Runs main analysis for all parameter combos specified in colAnns and creates plots.
}
