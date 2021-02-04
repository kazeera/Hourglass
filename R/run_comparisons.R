#' Run boxplot and heatmap analysis in streamlined approach
#'
#' Create the plots for each comparison. Run the same analysis on dataset object (ds parameter) and transformed values (ds.imp parameter, optional).
#'
#' @param ds A dataset object (a list with vals, rowAnn, colAnn, comparison, name).
#' @param rowAnns A character vector of 1-2 column names in ds$rowAnn.
#' @param colAnns A character vector of 1-2 column names in ds$colAnn.
#' @param heatmap_folder The main output folder for heatmaps and all other plots (for custom analysis and within parameter analysis)
#' @param boxplot_folder The main output folder for all boxplots (individual, overview, paired).
#' @param run A one row data frame or list object with logicals for what to run, names: make.boxplots, make.paired.boxplots, make.heatmaps, make.surv.curve
#' @param ds.imp A dataset object similar to ds with imputed or another transformation values.
#' @param customAn A list of 2 data frames for plotting specific rows and columns. @seealso customAn read in using \code{\link{import_custom_analysis_file}}, see description of file.
#' @export
run_comparisons <- function(ds, rowAnns, colAnns = NA, heatmap_folder = ".", boxplot_folder = ".", run = list(), ds.imp = NULL, customAn = NULL) {
  # Make comparison label which will be the main out directory
  current_comparison <- get_out_dir(current_dir = rowAnns[1], rowAnn2 = rowAnns[2], EXC_HRD = isTRUE(run$EXC_HRD), EXC_NEO = isTRUE(run$EXC_NEO))
  ds$comparison <- current_comparison

  # If name is NULL, redirect all output to home
  if (is.null(ds$name)) {
    ds$name <- "Dataset"
  }

  # Heatmaps
  if (isTRUE(run$make.heatmaps)) {
    run_discrete_barplot_analysis(ds, rowAnns[1], colAnns,
      parameters = "Het.Score",
      out_dir = create_folder(paste(heatmap_folder, ds$name, ds$comparison, sep = "/"))
    )
    run_heatmap_analysis(ds, rowAnns, colAnns,
      heatmap_folder = create_folder(paste(heatmap_folder, ds$name, ds$comparison, sep = "/")), customAn
    )
  }
  # # Boxplots
  if (isTRUE(run$make.boxplots)) {
    run_boxplot_analysis(ds, rowAnns, colAnns, out_dir = create_folder(paste(boxplot_folder, ds$name, sep = "/")))
  }

  # Paired analysis
  if (isTRUE(run$make.paired.boxplots)) {
    run_paired_analysis(ds, rowAnns, colAnns, out_dir = create_folder(sprintf("%s/%s/Paired %s", boxplot_folder, ds$name, ds$comparison)))
  }

  # Imputed
  if (!is.null(ds.imp)) {
    # Use imputed data
    ds <- ds.imp
    ds$comparison <- current_comparison

    # If it's a custom analysis, make new annotation column of any numeric column in the ds matrix(low, intermediate, high)
    if (!is.null(run$Custom.Column) & !is.na(run$Custom.Column)) {
      l <- sep_param_by_levels(ds, custom_name = run$Custom.Column)
      rowAnn1 <- l$rowAnn1
      ds$rowAnn <- l$rowAnn
      rm(l)
    } else {
      rowAnn1 <- rowAnns[1]
    }

    run_heatmap_analysis(ds,
      rowAnns = c(rowAnn1, rowAnns[2]), colAnns,
      heatmap_folder = create_folder(paste(heatmap_folder, ds$name, ds$comparison, sep = "/")), customAn
    )

    run_discrete_barplot_analysis(ds, rowAnn1, colAnns,
      parameters = "Het.Score",
      out_dir = create_folder(paste(heatmap_folder, ds$name, ds$comparison, sep = "/"))
    )
  }
}
