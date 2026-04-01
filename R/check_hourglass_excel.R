#' Check Hourglass Excel template
#'
#' Validate the Hourglass Excel template before running analysis.
#' This function checks workbook structure, file paths, comparison settings,
#' colors, feature sets, and feature-parameter mappings.
#'
#' @param xlsx_path Path to the Hourglass Excel workbook.
#' @param base_dir Base directory used to resolve relative file paths.
#'   Defaults to the workbook directory.
#' @param stop_on_error If TRUE, stop with an error when validation fails.
#'
#' @return A list with elements:
#' \describe{
#'   \item{valid}{Logical. TRUE if no errors were found.}
#'   \item{errors}{Character vector of validation errors.}
#'   \item{warnings}{Character vector of validation warnings.}
#'   \item{data}{Parsed workbook and source tables used for checks.}
#' }
#' @export
check_hourglass_excel <- function(xlsx_path, base_dir = dirname(xlsx_path), stop_on_error = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' is required.")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) stop("Package 'RColorBrewer' is required.")

  trim_na <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- NA_character_
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  as_bool <- function(x) {
    x2 <- toupper(trimws(as.character(x)))
    ifelse(x2 %in% c("TRUE", "FALSE"), x2 == "TRUE", NA)
  }
  split_delim <- function(x, delim = ",") {
    if (is.na(x) || !nzchar(trimws(x))) return(character())
    vals <- trimws(strsplit(x, delim, fixed = TRUE)[[1]])
    vals[nzchar(vals)]
  }
  parse_filters <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(list(ok = TRUE, filters = list(), bad = character()))
    parts <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    out <- vector("list", length(parts)); ok <- TRUE; bad <- character()
    for (i in seq_along(parts)) {
      p <- parts[[i]]
      if (grepl("!=", p, fixed = TRUE)) {
        sp <- strsplit(p, "!=", fixed = TRUE)[[1]]; op <- "!="
      } else if (grepl("=", p, fixed = TRUE)) {
        sp <- strsplit(p, "=", fixed = TRUE)[[1]]; op <- "="
      } else { ok <- FALSE; bad <- c(bad, p); next }
      if (length(sp) != 2) { ok <- FALSE; bad <- c(bad, p); next }
      col <- trimws(sp[[1]])
      vals <- trimws(strsplit(sp[[2]], "|", fixed = TRUE)[[1]])
      vals <- vals[nzchar(vals)]
      if (!nzchar(col) || length(vals) == 0) { ok <- FALSE; bad <- c(bad, p); next }
      out[[i]] <- list(column = col, op = op, values = vals)
    }
    list(ok = ok, filters = out, bad = bad)
  }
  normalize_first_colname <- function(df) {
    if (!is.null(df) && ncol(df) > 0 && names(df)[1] %in% c("Unnamed: 0", "...1", "X")) names(df)[1] <- "Unique_ID"
    df
  }
  read_tbl_auto <- function(path) {
    ext <- tolower(tools::file_ext(path))
    if (ext == "csv") return(readr::read_csv(path, show_col_types = FALSE))
    if (ext %in% c("txt", "tsv")) return(readr::read_tsv(path, show_col_types = FALSE))
    stop("Supported files are csv, txt, and tsv.")
  }
  is_hex_color <- function(x) grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)
  is_continuous_like <- function(v) {
    v <- suppressWarnings(as.numeric(v)); v <- v[!is.na(v)]
    length(v) >= 2 && length(unique(v)) > 2
  }
  is_discrete_like <- function(v) {
    if (is.factor(v) || is.character(v) || is.logical(v)) return(TRUE)
    v2 <- suppressWarnings(as.numeric(v))
    if (all(is.na(v2))) return(TRUE)
    v2 <- v2[!is.na(v2)]
    length(unique(v2)) <= 10
  }
  build_custom_labels <- function(n) {
    n <- suppressWarnings(as.integer(n))
    if (is.na(n) || n < 2) return(character())
    if (n == 2) return(c("Custom-low", "Custom-high"))
    if (n == 3) return(c("Custom-low", "Custom-int", "Custom-high"))
    c("Custom-low", paste0("Custom-int", seq_len(n - 2)), "Custom-high")
  }

  errors <- character(); warnings <- character()
  add_error <- function(sheet, location, message) {
    loc <- if (nzchar(location)) paste0(" [", location, "]") else ""
    errors <<- c(errors, paste0(sheet, loc, ": ", message))
  }
  add_warning <- function(sheet, location, message) {
    loc <- if (nzchar(location)) paste0(" [", location, "]") else ""
    warnings <<- c(warnings, paste0(sheet, loc, ": ", message))
  }

  if (!file.exists(xlsx_path)) stop("Excel file not found: ", xlsx_path)

  sheets <- openxlsx::getSheetNames(xlsx_path)
  required_sheets <- c("Comparisons", "Colors", "FeatureSets", "FeatureParameters")
  missing_sheets <- setdiff(required_sheets, sheets)
  if (length(missing_sheets) > 0) add_error("Workbook", "", paste("Missing required sheet(s):", paste(missing_sheets, collapse = ", ")))

  comp_raw <- tryCatch(openxlsx::read.xlsx(xlsx_path, sheet = "Comparisons", colNames = FALSE), error = function(e) NULL)
  colors <- tryCatch(openxlsx::read.xlsx(xlsx_path, sheet = "Colors", colNames = TRUE), error = function(e) NULL)
  featuresets <- tryCatch(openxlsx::read.xlsx(xlsx_path, sheet = "FeatureSets", colNames = TRUE), error = function(e) NULL)
  featureparams <- tryCatch(openxlsx::read.xlsx(xlsx_path, sheet = "FeatureParameters", colNames = TRUE), error = function(e) NULL)

  parsed <- list(comparisons = NULL, vals = NULL, rowAnn = NULL, colAnn = NULL,
                 colors = colors, featuresets = featuresets, featureparameters = featureparams)

  if (!is.null(comp_raw) && nrow(comp_raw) > 0 && ncol(comp_raw) >= 2) {
    option_names <- trim_na(comp_raw[[1]])
    comp_mat <- comp_raw[, -1, drop = FALSE]
    colnames(comp_mat) <- paste0("Comparison_", seq_len(ncol(comp_mat)))
    keep_cols <- vapply(comp_mat, function(x) any(!is.na(trim_na(x))), logical(1))
    comp_mat <- comp_mat[, keep_cols, drop = FALSE]
    comparisons <- as.data.frame(comp_mat, stringsAsFactors = FALSE)
    rownames(comparisons) <- option_names
    parsed$comparisons <- comparisons

    getv <- function(cc, rn) {
      if (!rn %in% rownames(comparisons)) return(NA_character_)
      trim_na(as.character(comparisons[rn, cc][[1]]))
    }
    get_row_vals <- function(rn) {
      if (!rn %in% rownames(comparisons)) return(rep(NA_character_, ncol(comparisons)))
      trim_na(as.character(unlist(comparisons[rn, , drop = TRUE])))
    }
    unique_non_na <- function(x) unique(x[!is.na(x)])

    fp_matrix_unique <- unique_non_na(get_row_vals("filepath_matrix"))
    fp_rowann_unique <- unique_non_na(get_row_vals("filepath_rowAnn"))
    fp_colann_unique <- unique_non_na(get_row_vals("filepath_colAnn"))

    if (length(fp_matrix_unique) == 0) add_error("Comparisons", "filepath_matrix", "filepath_matrix is missing.")
    if (length(fp_rowann_unique) == 0) add_error("Comparisons", "filepath_rowAnn", "filepath_rowAnn is missing.")
    if (length(fp_colann_unique) == 0) add_error("Comparisons", "filepath_colAnn", "filepath_colAnn is missing.")
    if (length(fp_matrix_unique) > 1) add_error("Comparisons", "filepath_matrix", "filepath_matrix differs across comparison columns.")
    if (length(fp_rowann_unique) > 1) add_error("Comparisons", "filepath_rowAnn", "filepath_rowAnn differs across comparison columns.")
    if (length(fp_colann_unique) > 1) add_error("Comparisons", "filepath_colAnn", "filepath_colAnn differs across comparison columns.")

    resolve_path <- function(p) {
      if (is.na(p) || !nzchar(p)) return(NA_character_)
      if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
      p2 <- file.path(base_dir, p)
      if (file.exists(p2)) return(normalizePath(p2, winslash = "/", mustWork = FALSE))
      NA_character_
    }

    vals_path <- resolve_path(fp_matrix_unique[1])
    rowann_path <- resolve_path(fp_rowann_unique[1])
    colann_path <- resolve_path(fp_colann_unique[1])

    if (is.na(vals_path)) add_error("Comparisons", "filepath_matrix", paste("filepath_matrix does not exist:", fp_matrix_unique[1]))
    if (is.na(rowann_path)) add_error("Comparisons", "filepath_rowAnn", paste("filepath_rowAnn does not exist:", fp_rowann_unique[1]))
    if (is.na(colann_path)) add_error("Comparisons", "filepath_colAnn", paste("filepath_colAnn does not exist:", fp_colann_unique[1]))

    if (!is.na(vals_path)) parsed$vals <- normalize_first_colname(read_tbl_auto(vals_path))
    if (!is.na(rowann_path)) parsed$rowAnn <- normalize_first_colname(read_tbl_auto(rowann_path))
    if (!is.na(colann_path)) parsed$colAnn <- normalize_first_colname(read_tbl_auto(colann_path))

    vals <- parsed$vals; rowAnn <- parsed$rowAnn; colAnn <- parsed$colAnn
    rowAnn_names <- names(rowAnn) %||% character()
    vals_names <- names(vals) %||% character()
    colAnn_names <- names(colAnn) %||% character()

    if (!is.null(vals) && !is.null(rowAnn)) {
      if (!identical(as.character(vals[[1]]), as.character(rowAnn[[1]]))) {
        add_warning("Comparisons", "filepath_matrix/filepath_rowAnn", "vals first column and rowAnn first column are not identical in order/content.")
      }
    }
    if (!is.null(vals) && !is.null(colAnn)) {
      if (!identical(as.character(names(vals)[-1]), as.character(colAnn[[1]]))) {
        add_warning("Comparisons", "filepath_colAnn", "vals column names and colAnn first column are not identical in order/content.")
      }
    }

    for (cc in colnames(comparisons)) {
      main_comp <- getv(cc, "MainComparison")
      custom_comp <- getv(cc, "CustomComparison")
      subgroup <- getv(cc, "Subgroup")
      within_group <- getv(cc, "WithinGroup")
      filter_str <- getv(cc, "Filter")
      bysample <- as_bool(getv(cc, "BySample"))
      bypatient <- as_bool(getv(cc, "ByPatient"))
      param_column <- getv(cc, "param_column")
      feature_column <- getv(cc, "feature_column")
      do_remove_outliers <- as_bool(getv(cc, "do_remove_outliers"))
      do_impute <- as_bool(getv(cc, "do_impute"))
      impute_with_mean <- getv(cc, "impute_with_mean")
      save_table <- as_bool(getv(cc, "save_table"))
      n_custom_quantiles <- getv(cc, "n_custom_quantiles")
      boxplot_log10y <- as_bool(getv(cc, "boxplot_log10y"))
      corr_method <- getv(cc, "corr_method")
      pval_test <- getv(cc, "pval_test")
      pval_label <- getv(cc, "pval_label")
      color_gradient <- getv(cc, "color_gradient")
      patient_id_column <- getv(cc, "patient_id_column")
      do_paired_analysis <- as_bool(getv(cc, "do_paired_analysis"))
      do_survival_analysis <- as_bool(getv(cc, "do_survival_analysis"))
      surv_time_column <- getv(cc, "surv_time_column")
      surv_status_column <- getv(cc, "surv_status_column")
      discrete_params <- getv(cc, "discrete_params")
      qc_feature_boxplots <- as_bool(getv(cc, "qc_feature_boxplots"))
      qc_param_boxplots <- as_bool(getv(cc, "qc_param_boxplots"))
      feature_plots <- as_bool(getv(cc, "feature_plots"))
      boxplot_indiv <- as_bool(getv(cc, "boxplot_indiv"))
      boxplot_overview <- as_bool(getv(cc, "boxplot_overview"))
      heatmap <- as_bool(getv(cc, "heatmap"))
      corrplot <- as_bool(getv(cc, "corrplot"))
      corrplot_individual <- as_bool(getv(cc, "corrplot_individual"))
      corrscatt_overview <- as_bool(getv(cc, "corrscatt_overview"))
      pval_FC_heatmap <- as_bool(getv(cc, "pval_FC_heatmap"))
      barplot_profile <- as_bool(getv(cc, "barplot_profile"))
      barplot_het <- as_bool(getv(cc, "barplot_het"))

      bool_fields <- c(BySample = bysample, ByPatient = bypatient, do_remove_outliers = do_remove_outliers,
                       do_impute = do_impute, save_table = save_table, boxplot_log10y = boxplot_log10y,
                       do_paired_analysis = do_paired_analysis, do_survival_analysis = do_survival_analysis,
                       qc_feature_boxplots = qc_feature_boxplots, qc_param_boxplots = qc_param_boxplots,
                       feature_plots = feature_plots, boxplot_indiv = boxplot_indiv, boxplot_overview = boxplot_overview,
                       heatmap = heatmap, corrplot = corrplot, corrplot_individual = corrplot_individual,
                       corrscatt_overview = corrscatt_overview, pval_FC_heatmap = pval_FC_heatmap,
                       barplot_profile = barplot_profile, barplot_het = barplot_het)
      bad_bool <- names(bool_fields)[is.na(bool_fields)]
      if (length(bad_bool) > 0) add_error("Comparisons", cc, paste("These fields must be TRUE/FALSE:", paste(bad_bool, collapse = ", ")))

      if (is.na(main_comp) && is.na(custom_comp)) add_error("Comparisons", cc, "Either MainComparison or CustomComparison must be filled.")
      if (!is.na(main_comp) && !main_comp %in% rowAnn_names) add_error("Comparisons", paste(cc, "MainComparison", sep = " / "), "MainComparison must match a rowAnn column.")
      if (!is.na(subgroup) && !subgroup %in% rowAnn_names) add_error("Comparisons", paste(cc, "Subgroup", sep = " / "), "Subgroup must match a rowAnn column.")
      if (!is.na(within_group) && !within_group %in% rowAnn_names) add_error("Comparisons", paste(cc, "WithinGroup", sep = " / "), "WithinGroup must match a rowAnn column.")

      fchk <- parse_filters(filter_str)
      if (!fchk$ok) add_error("Comparisons", paste(cc, "Filter", sep = " / "), paste("Invalid Filter syntax:", paste(fchk$bad, collapse = "; ")))
      if (!is.na(custom_comp)) {
        source_vec <- NULL
        if (custom_comp %in% rowAnn_names) source_vec <- rowAnn[[custom_comp]]
        if (is.null(source_vec) && custom_comp %in% vals_names) source_vec <- vals[[custom_comp]]
        if (is.null(source_vec)) add_error("Comparisons", paste(cc, "CustomComparison", sep = " / "), "CustomComparison not found in rowAnn or vals.")
        else if (!is_continuous_like(source_vec)) add_error("Comparisons", paste(cc, "CustomComparison", sep = " / "), "CustomComparison must be continuous numeric-like.")
      }
      if (!is.na(main_comp)) {
        source_vec <- NULL
        if (main_comp %in% rowAnn_names) source_vec <- rowAnn[[main_comp]]
        if (is.null(source_vec) && main_comp %in% vals_names) source_vec <- vals[[main_comp]]
        if (is.null(source_vec)) add_error("Comparisons", paste(cc, "MainComparison", sep = " / "), "MainComparison not found in rowAnn or vals.")
        else if (!is_discrete_like(source_vec)) add_error("Comparisons", paste(cc, "MainComparison", sep = " / "), "MainComparison must be discrete-like.")
        else {
          nonmiss <- unique(as.character(source_vec[!is.na(source_vec)]))
          if (length(nonmiss) < 2) add_error("Comparisons", paste(cc, "MainComparison", sep = " / "), "MainComparison must have at least 2 non-missing groups.")
        }
      }

      if (is.na(param_column) || !param_column %in% colAnn_names) add_error("Comparisons", paste(cc, "param_column", sep = " / "), "param_column must match a colAnn column.")
      if (is.na(feature_column) || !feature_column %in% colAnn_names) add_error("Comparisons", paste(cc, "feature_column", sep = " / "), "feature_column must match a colAnn column.")
      if (isTRUE(do_impute) && is.na(suppressWarnings(as.numeric(impute_with_mean)))) add_error("Comparisons", paste(cc, "impute_with_mean", sep = " / "), "impute_with_mean must be numeric when do_impute is TRUE.")
      if (!is.na(custom_comp)) {
        n_q <- suppressWarnings(as.integer(n_custom_quantiles))
        if (is.na(n_q) || n_q < 2) add_error("Comparisons", paste(cc, "n_custom_quantiles", sep = " / "), "n_custom_quantiles must be integer >= 2 when CustomComparison is used.")
      }
      if ((isTRUE(corrplot) || isTRUE(corrplot_individual) || isTRUE(corrscatt_overview)) &&
          (is.na(corr_method) || !corr_method %in% c("pearson", "spearman", "kendall"))) {
        add_error("Comparisons", paste(cc, "corr_method", sep = " / "), "corr_method must be pearson, spearman, or kendall.")
      }
      if (is.na(pval_test) || !pval_test %in% c("t.test", "wilcox.test")) add_error("Comparisons", paste(cc, "pval_test", sep = " / "), "pval_test must be t.test or wilcox.test.")

      any_boxplot_like <- any(c(qc_feature_boxplots, qc_param_boxplots, feature_plots, boxplot_indiv, boxplot_overview, do_paired_analysis), na.rm = TRUE)
      if (isTRUE(any_boxplot_like) && (is.na(pval_label) || !pval_label %in% c("stars", "text"))) {
        add_error("Comparisons", paste(cc, "pval_label", sep = " / "), "pval_label must be stars or text.")
      }
      if (is.na(color_gradient) || !color_gradient %in% rownames(RColorBrewer::brewer.pal.info)) add_error("Comparisons", paste(cc, "color_gradient", sep = " / "), "color_gradient must match an RColorBrewer palette name.")
      if ((isTRUE(bypatient) || isTRUE(do_paired_analysis)) && (is.na(patient_id_column) || !patient_id_column %in% rowAnn_names)) add_error("Comparisons", paste(cc, "patient_id_column", sep = " / "), "patient_id_column must exist in rowAnn when ByPatient or do_paired_analysis is TRUE.")
      if (isTRUE(do_paired_analysis) && !isTRUE(bysample)) add_error("Comparisons", paste(cc, "do_paired_analysis", sep = " / "), "do_paired_analysis = TRUE requires BySample = TRUE.")
      if (isTRUE(do_survival_analysis)) {
        if (is.na(surv_time_column) || !surv_time_column %in% rowAnn_names) add_error("Comparisons", paste(cc, "surv_time_column", sep = " / "), "surv_time_column must exist in rowAnn.")
        if (is.na(surv_status_column) || !surv_status_column %in% rowAnn_names) add_error("Comparisons", paste(cc, "surv_status_column", sep = " / "), "surv_status_column must exist in rowAnn.")
      }
      if (!is.na(discrete_params) && !is.na(param_column) && param_column %in% colAnn_names) {
        dparams <- split_delim(discrete_params, ",")
        allowed_params <- unique(as.character(colAnn[[param_column]]))
        bad_params <- setdiff(dparams, allowed_params)
        if (length(bad_params) > 0) add_error("Comparisons", paste(cc, "discrete_params", sep = " / "), paste("discrete_params not found in colAnn$", param_column, ": ", paste(bad_params, collapse = ", "), sep = ""))
      }
    }
  } else {
    add_error("Comparisons", "", "Sheet is empty or malformed.")
  }

  if (!is.null(colors)) {
    needed_cols <- c("Variable", "HexCode")
    miss <- setdiff(needed_cols, names(colors))
    if (length(miss) > 0) add_error("Colors", "columns", paste("Missing columns:", paste(miss, collapse = ", ")))
    else {
      colors$Variable <- trim_na(colors$Variable); colors$HexCode <- trim_na(colors$HexCode)
      if (any(is.na(colors$Variable))) add_error("Colors", "Variable", "Variable has missing values.")
      if (any(is.na(colors$HexCode))) add_error("Colors", "HexCode", "HexCode has missing values.")
      bad_hex <- colors$HexCode[!is.na(colors$HexCode) & !is_hex_color(colors$HexCode)]
      if (length(bad_hex) > 0) add_error("Colors", "HexCode", paste("Invalid hex code(s):", paste(unique(bad_hex), collapse = ", ")))
      if (anyDuplicated(colors$Variable)) add_warning("Colors", "Variable", "Duplicate Variable entries found.")

      comps <- parsed$comparisons; rowAnn <- parsed$rowAnn
      if (!is.null(comps) && !is.null(rowAnn)) {
        required_color_vars <- character()
        for (cc in colnames(comps)) {
          main_comp <- trim_na(as.character(comps["MainComparison", cc]))
          custom_comp <- trim_na(as.character(comps["CustomComparison", cc]))
          n_custom <- trim_na(as.character(comps["n_custom_quantiles", cc]))
          if (!is.na(main_comp) && main_comp %in% names(rowAnn)) {
            vals <- unique(as.character(rowAnn[[main_comp]]))
            vals <- vals[!is.na(vals) & nzchar(vals)]
            if (length(vals) >= 2) required_color_vars <- c(required_color_vars, paste0(main_comp, "-", vals))
          }
          if (!is.na(custom_comp)) required_color_vars <- c(required_color_vars, build_custom_labels(n_custom))
        }
        required_color_vars <- unique(required_color_vars)
        missing_defs <- setdiff(required_color_vars, colors$Variable)
        if (length(missing_defs) > 0) add_error("Colors", "Variable", paste("Missing Variable definitions for:", paste(missing_defs, collapse = ", ")))
      }
    }
  }

  if (!is.null(featuresets)) {
    needed_cols <- c("GroupName", "GroupList", "Alternative")
    miss <- setdiff(needed_cols, names(featuresets))
    if (length(miss) > 0) add_error("FeatureSets", "columns", paste("Missing columns:", paste(miss, collapse = ", ")))
    else {
      featuresets$GroupName <- trim_na(featuresets$GroupName)
      featuresets$GroupList <- trim_na(gsub(";", ",", featuresets$GroupList))
      alt_bool <- as_bool(featuresets$Alternative)
      if (any(is.na(featuresets$GroupName))) add_error("FeatureSets", "GroupName", "GroupName must be filled in for all rows.")
      if (any(is.na(alt_bool))) add_error("FeatureSets", "Alternative", "Alternative must be TRUE or FALSE.")

      comps <- parsed$comparisons; colAnn <- parsed$colAnn
      feature_col_name <- NA_character_
      if (!is.null(comps) && "feature_column" %in% rownames(comps)) {
        fc_vals <- unique(trim_na(as.character(unlist(comps["feature_column", , drop = TRUE]))))
        fc_vals <- fc_vals[!is.na(fc_vals)]
        if (length(fc_vals) >= 1) feature_col_name <- fc_vals[[1]]
      }
      valid_features <- character()
      if (!is.null(colAnn) && !is.na(feature_col_name) && feature_col_name %in% names(colAnn)) {
        valid_features <- unique(as.character(colAnn[[feature_col_name]]))
      }
      allowed_group_names <- featuresets$GroupName[!is.na(featuresets$GroupName)]
      for (i in seq_len(nrow(featuresets))) {
        gl <- featuresets$GroupList[[i]]
        if (is.na(gl) || !nzchar(gl)) { add_warning("FeatureSets", paste0("row ", i), "GroupList is empty."); next }
        members <- split_delim(gl, ",")
        bad <- setdiff(members, c(valid_features, allowed_group_names))
        if (length(bad) > 0) add_error("FeatureSets", paste0("row ", i, " / GroupList"), paste("Unknown entries:", paste(bad, collapse = ", ")))
      }
    }
  }

  if (!is.null(featureparams)) {
    needed_cols <- c("Feature", "Standard_Parameter", "Alternative_Parameter")
    miss <- setdiff(needed_cols, names(featureparams))
    if (length(miss) > 0) add_error("FeatureParameters", "columns", paste("Missing columns:", paste(miss, collapse = ", ")))
    else {
      featureparams$Feature <- trim_na(featureparams$Feature)
      featureparams$Standard_Parameter <- trim_na(featureparams$Standard_Parameter)
      featureparams$Alternative_Parameter <- trim_na(featureparams$Alternative_Parameter)

      comps <- parsed$comparisons; colAnn <- parsed$colAnn
      feature_col_name <- NA_character_; param_col_name <- NA_character_
      if (!is.null(comps)) {
        fc_vals <- unique(trim_na(as.character(unlist(comps["feature_column", , drop = TRUE]))))
        pc_vals <- unique(trim_na(as.character(unlist(comps["param_column", , drop = TRUE]))))
        fc_vals <- fc_vals[!is.na(fc_vals)]; pc_vals <- pc_vals[!is.na(pc_vals)]
        if (length(fc_vals) >= 1) feature_col_name <- fc_vals[[1]]
        if (length(pc_vals) >= 1) param_col_name <- pc_vals[[1]]
      }
      if (is.null(colAnn) || is.na(feature_col_name) || !feature_col_name %in% names(colAnn)) {
        add_error("FeatureParameters", "Feature", "Could not validate Feature because feature_column is invalid.")
      } else {
        valid_features <- unique(as.character(colAnn[[feature_col_name]]))
        bad_features <- setdiff(featureparams$Feature[!is.na(featureparams$Feature)], valid_features)
        if (length(bad_features) > 0) add_error("FeatureParameters", "Feature", paste("Feature not found in colAnn$", feature_col_name, ": ", paste(unique(bad_features), collapse = ", "), sep = ""))
      }
      if (!is.null(colAnn) && !is.na(feature_col_name) && feature_col_name %in% names(colAnn) &&
          !is.na(param_col_name) && param_col_name %in% names(colAnn)) {
        for (i in seq_len(nrow(featureparams))) {
          feat <- featureparams$Feature[[i]]
          stdp <- featureparams$Standard_Parameter[[i]]
          altp <- featureparams$Alternative_Parameter[[i]]
          if (is.na(feat)) next
          allowed_params <- colAnn |>
            dplyr::filter(.data[[feature_col_name]] == feat) |>
            dplyr::pull(.data[[param_col_name]]) |>
            as.character() |>
            unique()
          if (!is.na(stdp) && !stdp %in% allowed_params) add_error("FeatureParameters", paste0("row ", i, " / Standard_Parameter"), paste0("Parameter '", stdp, "' does not match Feature '", feat, "'."))
          if (!is.na(altp) && !altp %in% allowed_params) add_error("FeatureParameters", paste0("row ", i, " / Alternative_Parameter"), paste0("Parameter '", altp, "' does not match Feature '", feat, "'."))
        }
      }
    }
  }

  out <- list(valid = length(errors) == 0, errors = unique(errors), warnings = unique(warnings), data = parsed)
  if (stop_on_error && length(out$errors) > 0) {
    message(paste(out$errors, collapse = "\n"))
  }
  out
}
