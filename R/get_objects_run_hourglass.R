#' Get comparisons to run from input Excel file.
#'
#' Get comparisons to run data frame for run_hourglass function from input Excel file.
#'
#' @param file_xl Excel file (.xlsx) containing user options for hourglass run.
#' @param sheet Name of worksheet in file_xl that contains comparisons to run.
#' @return A data frame containing comparisons to run compatible with run_hourglass function (i.e. column names are options, rows are each comparison to run)
#' @export
get_comparisons <- function(file_xl, sheet = "Comparisons") {
  # Read in table where row 1 = option names, each column is a comparison
  df <- read.xlsx(xlsxFile = file_xl, sheet = sheet, colNames = F)

  # Transpose to data frame
  df2 <- df[, -1] %>%
    t() %>%
    data.frame(stringsAsFactors = F)

  # Name columns
  colnames(df2) <- df[, 1]

  # String to logical for certain columns
  for (i in 1:ncol(df2)) {
    if (any(df2[, i] %in% c("True", "False", "TRUE", "FALSE"))) {
      df2[, i] <- as.logical(df2[, i])
    }
  }
  return(df2)
}

#' Read in and format feat_sets.
#'
#' Create feat_sets variable for run_hourglass function from input Excel file.
#'
#' @param file_xl The name of file path to an Excel file for custom analyses.
#' @param sets_sheet The name of the worksheet in file_xl containing FeatureSets.
#' @param params_sheet The name of the worksheet in file_xl containing matching FeatureParameters.
#' @return feat_sets object = list with 2 elements - data frames: 1) sets, 2) params (for "feature parameters") See documentation for more info.
#' @export
get_feat_sets <- function(file_xl, sets_sheet = "FeatureSets", params_sheet = "FeatureParameters") {
  # If file not found
  if (length(file_xl) == 0) {
    return(NULL)
  }

  # return a list, where each element is a relevant worksheet
  list(
    sets = read.xlsx(file_xl, sheet = sets_sheet), #keys
    params = read.xlsx(file_xl, sheet = params_sheet) #values
  )
}

#' Get color palette from input Excel file.
#'
#' Get color palette for all rowAnn variables for use by run_hourglass function from input Excel file.
#'
#' @param file_xl Excel file (.xlsx) containing user options for hourglass run.
#' @param sheet Name of worksheet in file_xl containing table where column 1 is variables and column 2 is hex code
#' @return List of colors, where elements are hex codes and element names are variables. As a result of "cleaning", element names don't have 2 parts, are duplicated, and is white e.g. list("Tumour"="#2f4f4Fff", "Stroma"="#d2691eff")
#' @export
get_colors <- function(file_xl, sheet = "Colors") {
  read.xlsx(xlsxFile = file_xl, sheet = sheet) %>%
    clean_colors()
}

#' Prepare dataset objects in run_hourglass function.
#'
#' Prepare dataset objects: samples, patients (and imputed datasets) based on user options.#'
#' @param comparisons   A data frame containing comparisons to run compatible with run_hourglass function (i.e. column names are options, rows are each comparison to run). See ?get_comparisons for more info.
#' @param datasets Optional. List of 2 elements 1) samples, 2) patients, which are the dataset objects for BySample and ByPatient analysis respectively.
#' @return Returns a list of 4 dataset objects with names: "samples", "samples_imp", "patients", "patients_imp"
#' @export
get_datasets <- function(comparisons, datasets = NULL) {
  # Do we need to run a ByPatient analysis?
  run_bypatient <- any(comparisons$ByPatient) & !is.na(comparisons$patient_id_column[1]) 

  # Prepare datasets
  samples <- datasets[["samples"]]
  patients <- datasets[["patients"]]

  # If not given, make them from user option parameters
  if (is.null(datasets)) {
    # Read in data (BySample) and create dataset object
    samples <- make_dataset_ob(
      vals = read_file(comparisons$filepath_matrix[1]),
      rowAnn = read_file(comparisons$filepath_rowAnn[1]),
      colAnn = read_file(comparisons$filepath_colAnn[1]),
      name = "BySample" #comparisons$dataset_name[1]
    )

    # Average dataset to make ByPatient
    if (run_bypatient) {
      patients <- avg_dataset(samples, as.character(comparisons$patient_id_column[1]), "ByPatient") # , rows_to_keep = rows_to_keep)
      
      # Reorder columns to match samples
      patients$vals <- patients$vals[,colnames(samples$vals)]
      # Ensure columns and rows are in same order
      patients$colAnn <- patients$colAnn[colnames(patients$vals),]
      patients$rowAnn <- patients$rowAnn[rownames(patients$vals),]
    }
  }

  # Remove outliers from datasets, i.e. only keep points between quartile(Q)1 and Q3
  if (any(comparisons$do_remove_outliers)) {
    samples$vals <- samples$vals %>% remove_outliers_df()
    if (run_bypatient) {
      patients$vals <- patients$vals %>% remove_outliers_df() 
      
    }
  }

  # Initialize imputed dataset with NULL value (prevents it from running)
  samples_imp <- patients_imp <- NULL
  # Impute if specified
  if (any(comparisons$do_impute)) {
    # Get value = percent to impute around mean
    x <- comparisons$impute_with_mean[1] %>% as.numeric()

    # Impute datasets
    samples_imp <- impute_ds(samples, x)
    if (run_bypatient) {
      patients_imp <- impute_ds(patients, x)
    }
  }

  # Return
  list(
    samples = samples,
    samples_imp = samples_imp,
    patients = patients,
    patients_imp = patients_imp
  )
}
