#' Functions defined in this file:
#'   run_from_excel
#'   test_hourglass
#'   run_hourglass

#' Run hourglass from single Excel file input.
#'
#' Run hourglass according to user specifications indicated in Excel file input.
#'
#' @param xl_file The path to an Excel file with 4 worksheets: Colors, Comparisons, FeatureSets, FeatureParameters. See documentation for more information.
#' @export
run_from_excel <- function(xl_file) {
  # Read in color palette
  var_colors <- get_colors(xl_file, sheet = "Colors")

  # Read in all comparisons (includes data file paths and advanced options)
  comparisons <- get_comparisons(xl_file, sheet = "Comparisons")

  # Read in custom analysis
  feat_sets <- get_feat_sets(xl_file, "FeatureSets", "FeatureParameters")

  # Run hourglass
  run_hourglass(comparisons, var_colors, feat_sets)
}

#' Test hourglass R package from py interface.
#'
#' Used to test hourglass from py interface by saving the in-built iris dataset to a csv file in specified output directory.
#'
#' @param out_dir The output directory for a file.
#' @param filename The name of the output csv file (minus the .csv extension).
#' @export
test_hourglass <- function(out_dir = ".", filename = "test_iris") {
  print(paste("Out_dir:", out_dir))
  print(paste("Getwd:", getwd()))
  # Save a note in current directory that prints name of excel file
  write.csv(iris, file=sprintf("%s/%s.csv", out_dir, filename))
}

#' Run hourglass.
#'
#' Run hourglass according to user specifications. See documentation for format of input parameters.
#'
#' @param comparisons   A data frame containing comparisons to run compatible with run_hourglass function (i.e. column names are options, rows are each comparison to run). See ?get_comparisons for more info.
#' @param var_colors List of colors, where elements are hex codes and element names are rowAnn variables. e.g. list("Tumour"="#2f4f4Fff", "Stroma"="#d2691eff") See ?get_colors for more info.
#' @param feat_sets Custom analysis also known as feature sets. A list of 2 data frames for plotting specific rows and columns. See ?get_feat_sets for more info.
#' @param datasets Optional. List of 2 elements 1) sample, 2) patient, which are the dataset objects for BySample and ByPatient analysis respectively.
#' @param keep_column_colAnn Optional. Column name in colAnn of which columns to keep in vals, important for QC plots
#' @export
run_hourglass <- function(comparisons, var_colors, feat_sets, datasets = NULL, keep_column_colAnn = "Keep.In.Analysis") {

  # Do we need to run a ByPatient analysis?
  run_bypatient <- any(comparisons$ByPatient) & !is.na(comparisons$paired_id_column[1]) # TODO see what output of excelwriter from kivy is - NA if missing or NULL?

  # Get datasets
  dss <- get_datasets(comparisons, datasets)
  samples <- dss[["samples"]]
  samples_imp <- dss[["samples_imp"]]
  patients <- dss[["patients"]]
  patients_imp <- dss[["patients_imp"]]
  rm(dss)

  # For each row in the comparisons excel file df, get run criteria from excel file
  for (i in 1:nrow(comparisons)) {
    # Current comparison
    run <- comparisons[i, ]

    # Pick whether you want to look at all samples or samples averaged across patients
    for (sample.or.patient in c("BySample", "ByPatient")) { # Note: ByPatient second so paired_id_column can be NA
      # If the value is FALSE, skip
      if (!run[, sample.or.patient]) next

      # If it is TRUE, pick whether we are currently looking at all samples and/or samples averaged across patients
      if (sample.or.patient == "BySample") {
        ds <- samples # dataset
        ds.imp <- samples_imp
      } else {
        ds <- patients
        ds.imp <- patients_imp
      }

      # What rows are we keeping based on filters (inclusion/exclusion criteria)?
      rows_to_keep <- subset_by_filters(ds$rowAnn, run$Filter)

      # What columns are we keeping in the analysis?
      # all values in columns are NA
      cols_to_keep <- !apply(ds$vals, 2, function(x) all(is.na(x)))

      if (keep_column_colAnn %in% colnames(ds$colAnn)) {
        cols_to_keep <- cols_to_keep & ds$colAnn[, keep_column_colAnn]
      }

      # Subset dataset
      ds <- subset_dataset(ds, rows_to_keep, cols_to_keep)
      ds.imp <- subset_dataset(ds.imp, rows_to_keep, cols_to_keep) # will be NULL

      # Get some parameters for current analysis
      rowAnn1 <- run$MainComparison
      rowAnn2 <- run$Subgroup

      # Check whether the main comparison is continuous/numeric or not
      is_continuous <- ifelse(is.na(run$MainComparison), F, ifelse(is.numeric(ds$rowAnn[, run$MainComparison]), T, F))

      # If it's a numeric/continuous column from ds$vals or ds$rowAnn,
      # make new ds$rowAnn column for level (low, intermediate, high)
      if (!is.na(run$CustomComparison) | is_continuous) {
        # Define variable
        col_name <- ifelse(!is.na(run$CustomComparison), run$CustomComparison, run$MainComparison)
        # ds 1: Raw data
        new <- add_to_rowAnn(ds, col_name)
        rowAnn1 <- new$rowAnn1
        ds$rowAnn <- new$rowAnn
        # ds 2: Imputed
        new <- add_to_rowAnn(ds.imp, col_name)
        ds.imp$rowAnn <- new$rowAnn
      }

      # Remove NAs in MainComparison
      ds <- subset_dataset(ds, rows_to_keep = !is.na(ds$rowAnn[, rowAnn1]))
      ds.imp <- subset_dataset(ds.imp, rows_to_keep = !is.na(ds$rowAnn[, rowAnn1]))

      # Make comparison label which will be the main out directory
      if (is.null(ds$comparison)) {
        current_comparison <- get_comparison_name(rowAnn1, filters = run$Filter, all_out_dirs = list.dirs(ds$name), rowAnn2 = rowAnn2)
        ds$comparison <- current_comparison
        if (!is.null(ds.imp)) {
          ds.imp$comparison <- current_comparison
        }
      }

      # Run analysis
      run_comparison(
        ds,
        rowAnns = c(rowAnn1, rowAnn2),
        colAnns = c(run$param_column, run$feature_column),
        output_folder = ds$name,
        ds.imp = ds.imp,
        feat_sets = feat_sets,
        var_colors = var_colors,
        gradient_palette = run$color_gradient,
        corr_method = run$corr_method,
        pval.test = run$pval_test,
        pval.label = ifelse(grepl("star", run$pval_label), "p.signif", "p.format"),
        paired_analysis_column = ifelse(sample.or.patient == "BySample", run$paired_id_column, NA),
        make.QC.param = run$qc_param_boxplots,
        make.QC.feature = run$qc_feature_boxplots,
        discrete_stacked_params =  run$discrete_params,
        make.het.plot = run$barplot_het,
        make.indiv.boxplot = run$boxplot_indiv,
        make.overview.boxplot = run$boxplot_overview,
        make.heatmap = run$heatmap,
        make.corrplot = run$corrplot,
        make.overview.corrscatt = run$corrscatt_overview,
        make.FC.pval.plot = run$pval_FC_heatmap,
        make.barplot = run$barplot_profile,
      )

      # Check whether user wants to divide cohort
      sub_analyses <- strsplit(run$WithinGroup, ";") %>%
        unlist() %>%
        trimws()
      if (length(sub_analyses) == 0 | isTRUE(is.na(sub_analyses))) next

      # For each within group analysis, divide cohort and run hourglass within groups
      for (rowAnn_col in sub_analyses) {
        if (!rowAnn_col %in% colnames(ds$rowAnn)) next
        # Get unique groups
        # e.g. If rowAnn_column is "Sex" with unique values NA, "F", "M", groups returns "F" and "M"
        groups <- ds$rowAnn[, rowAnn_col] %>%
          unique() %>%
          na.omit() %>%
          as.character()

        # Run hourglass within cohorts
        for (group in groups) {
          # Positively select group
          ds2 <- subset_dataset(ds, rows_to_keep = ds$rowAnn[, rowAnn_col] == group)
          ds2.imp <- subset_dataset(ds.imp, rows_to_keep = ds$rowAnn[, rowAnn_col] == group)

          # Rename comparison
          ds2$comparison <- paste(current_comparison, group)
          if (!is.null(ds.imp)) {
            ds2.imp$comparison <- paste(current_comparison, group)
          }

          # Run analysis
          run_comparison(
            ds = ds2,
            rowAnns = c(rowAnn1, rowAnn2),
            colAnns = c(run$param_column, run$feature_column),
            output_folder = ds2$name,
            ds.imp = ds2.imp,
            feat_sets = feat_sets,
            var_colors = var_colors,
            gradient_palette = run$color_gradient,
            corr_method = run$corr_method,
            pval.test = run$pval_test,
            pval.label = ifelse(grepl("star", run$pval_label), "p.signif", "p.format"),
            paired_analysis_column = ifelse(sample.or.patient == "BySample", run$paired_id_column, NA),
            make.QC.param = run$qc_param_boxplots,
            make.QC.feature = run$qc_feature_boxplots,
            discrete_stacked_params = run$discrete_params,
            make.het.plot = run$barplot_het,
            make.indiv.boxplot = run$boxplot_indiv,
            make.overview.boxplot = run$boxplot_overview,
            make.heatmap = run$heatmap,
            make.corrplot = run$corrplot,
            make.overview.corrscatt = run$corrscatt_overview,
            make.FC.pval.plot = run$pval_FC_heatmap,
            make.barplot = run$barplot_profile,
          )
        }
      }
    }
  }
}
