#' Run Hourglass from single Excel file input.
#'
#' Run Hourglass according to user specifications indicated in Excel file input.
#'
#' @param xl_file The path to an Excel file with 4 worksheets: Colors, Comparisons, FeatureSets, FeatureParameters. See documentation for more information.
#' @export
run_from_excel <- function(xl_file){
  # Read in color palette
  var_colors <- get_colors(xl_file, sheet = "Colors")

  # Read in all comparisons (includes data file paths and advanced options)
  comparisons <- get_comparisons(xl_file, sheet = "Comparisons")

  # Read in custom analysis
  feat_sets <- get_feat_sets(xl_file, "FeatureSets", "FeatureParameters")

  # Go to where excel fle is located # remove everything after last forward slash (escape character)
  main_folder <- ifelse(grepl("\\/",xl_file), sub("\\/[^\\/]+$", "",xl_file), ".")
  main_folder <- create_folder(paste0(main_folder, "/", format(Sys.Date(), "%y%m%d"), " Hourglass"))

  # Run Hourglass
  run_Hourglass(comparisons, var_colors, feat_sets, main_folder)
}


#' Test Hourglass R package from py interface.
#'
#' Used to test Hourglass from py interface by saving the in-built iris dataset to a csv file in specified output directory.
#'
#' @param out_dir The output directory for a file.
#' @param filename The name of the output csv file (minus the .csv extension).
#' @export
test_Hourglass <- function(out_dir = ".", filename = "test_iris") {
  print(paste("Out_dir:", out_dir))
  print(paste("Getwd:", getwd()))
  # Save a note in current directory that prints name of excel file
  write.csv(iris, file=sprintf("%s/%s.csv", out_dir, filename))
}

#' Run Hourglass.
#'
#' Run Hourglass according to user specifications. See documentation for format of input parameters.
#'
#' @param comparisons   A data frame containing comparisons to run compatible with run_hourglass function (i.e. column names are options, rows are each comparison to run). See ?get_comparisons for more info.
#' @param var_colors List of colors, where elements are hex codes and element names are rowAnn variables. e.g. list("Tumour"="#2f4f4Fff", "Stroma"="#d2691eff") See ?get_colors for more info.
#' @param feat_sets A list of 2 data frames for plotting specific rows and columns. See ?get_feat_sets for more info.
#' @param main_folder The output directory for Hourglass analysis, default is working directory.
#' @param datasets Optional. List of 2 elements 1) sample, 2) patient, which are the dataset objects for BySample and ByPatient analysis respectively.
#' @param keep_column_colAnn Optional. Column name in colAnn of which columns to keep in vals, important for QC plots
#' @export
run_Hourglass <- function(comparisons, var_colors, feat_sets, main_folder = ".", datasets = NULL, keep_column_colAnn = "Keep.In.Analysis"){
  # Save start time to a variable
  start_time <- Sys.time()
  print(sprintf("Run started on %s.", format(start_time, "%a %b %d %X %Y")))

  # Do we need to run a ByPatient analysis?
  run_bypatient <- any(comparisons$ByPatient) & !is.na(comparisons$patient_id_column[1]) # TODO see what output of excelwriter from kivy is - NA if missing or NULL?

  # Get datasets
  dss <- get_datasets(comparisons, datasets)
  samples <- dss[["samples"]]
  samples_imp <- dss[["samples_imp"]]
  patients <- dss[["patients"]]
  patients_imp <- dss[["patients_imp"]]
  rm(dss)

  # Create main output
  if(any(comparisons$ByPatient & comparisons$do_survival_analysis)){
    surv_folder <- create_folder(paste0(main_folder, "/ByPatient/Survival"))
  }

  # For each row in the comparisons excel file df, get run criteria from excel file
  for (i in 1:nrow(comparisons)) {
    # Current comparison
    run <- comparisons[i, ]

    # Pick whether you want to look at all samples or samples averaged across patients
    for (sample.or.patient in c("BySample", "ByPatient")) { # Note: ByPatient second so patient_id_column can be NA
      # If the value is FALSE, skip
      if (!run[, sample.or.patient]) next
      tryCatch({
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
          new <- add_to_rowAnn(ds, col_name, as.integer(run$n_custom_quantiles))
          rowAnn1 <- new$rowAnn1
          ds$rowAnn <- new$rowAnn
          # ds 2: Imputed
          if(isFALSE(is.null(ds.imp))){
            new <- add_to_rowAnn(ds.imp, col_name, as.integer(run$n_custom_quantiles))
            ds.imp$rowAnn <- new$rowAnn
          }
        }

        # Remove NAs in MainComparison
        ds <- subset_dataset(ds, rows_to_keep = !is.na(ds$rowAnn[, rowAnn1]))
        if(isFALSE(is.null(ds.imp))){
          ds.imp <- subset_dataset(ds.imp, rows_to_keep = !is.na(ds$rowAnn[, rowAnn1]))
        }

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
          output_folder = main_folder,
          ds.imp = ds.imp,
          feat_sets = feat_sets,
          var_colors = var_colors,
          gradient_palette = run$color_gradient,
          corr_method = run$corr_method,
          pval.test = run$pval_test,
          pval.label = ifelse(grepl("star", run$pval_label), "p.signif", "p.format"),
          boxplot_log10_y = ifelse(is.null(run$boxplot_log10y), F, run$boxplot_log10y),
          paired_analysis_column = run$patient_id_column,
          do_paired_analysis = ifelse(sample.or.patient == "BySample", run$do_paired_analysis, FALSE),
          make.QC.param = run$qc_param_boxplots,
          make.QC.feature = run$qc_feature_boxplots,
          make.feat.plots = ifelse(is.null(run$feature_plots), T, run$feature_plots),
          discrete_stacked_params =  run$discrete_params,
          make.het.plot = run$barplot_het,
          make.indiv.boxplot = run$boxplot_indiv,
          make.overview.boxplot = run$boxplot_overview,
          make.heatmap = run$heatmap,
          make.corrplot = run$corrplot,
          make.overview.corrscatt = run$corrscatt_overview,
          make.FC.pval.plot = run$pval_FC_heatmap,
          make.barplot = run$barplot_profile,
          save_table = run$save_table
        )

        # Survival analysis
        if(isTRUE(run$do_survival_analysis) & sample.or.patient == "ByPatient"){
          run_surv_analysis(ds, rowAnn1, run, surv_folder)
        }

        # Check whether user wants to divide cohort
        sub_analyses <- strsplit(run$WithinGroup, ",") %>%
          unlist() %>%
          trimws()
        if (length(sub_analyses) == 0 | isTRUE(is.na(sub_analyses))) next

        # For each within group analysis, divide cohort and run Hourglass within groups
        for (rowAnn_col in sub_analyses) {
          if (!rowAnn_col %in% colnames(ds$rowAnn)) next
          # Get unique groups
          # e.g. If rowAnn_column is "Sex" with unique values NA, "F", "M", groups returns "F" and "M"
          groups <- ds$rowAnn[, rowAnn_col] %>%
            unique() %>%
            na.omit() %>%
            as.character()

          # Run Hourglass within cohorts
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
              output_folder = main_folder,
              ds.imp = ds2.imp,
              feat_sets = feat_sets,
              var_colors = var_colors,
              gradient_palette = run$color_gradient,
              corr_method = run$corr_method,
              pval.test = run$pval_test,
              pval.label = ifelse(grepl("star", run$pval_label), "p.signif", "p.format"),
              boxplot_log10_y = ifelse(is.null(run$boxplot_log10y), F, run$boxplot_log10y),
              paired_analysis_column = run$patient_id_column,
              do_paired_analysis = ifelse(sample.or.patient == "BySample", run$do_paired_analysis, FALSE),
              make.QC.param = run$qc_param_boxplots,
              make.QC.feature = run$qc_feature_boxplots,
              make.feat.plots = ifelse(is.null(run$feature_plots), T, run$feature_plots),
              discrete_stacked_params = run$discrete_params,
              make.het.plot = ifelse(sample.or.patient == "BySample", run$barplot_het, FALSE),
              make.indiv.boxplot = run$boxplot_indiv,
              make.overview.boxplot = run$boxplot_overview,
              make.heatmap = run$heatmap,
              make.corrplot = run$corrplot,
              make.overview.corrscatt = run$corrscatt_overview,
              make.FC.pval.plot = run$pval_FC_heatmap,
              make.barplot = run$barplot_profile,
              save_table = run$save_table
            )
          }
        }
      },
      error = function(err) {
        print(sprintf("%s", err))
      })
    }
  }

  # Print time difference
  end_time <- Sys.time()
  print(sprintf("Run completed on %s.", format(end_time, "%a %b %d %X %Y")))
  print(end_time - start_time)
}
