#' Subset dataset for custom analysis
#'
#' Get a subset of dataset list object from custom analysis table
#'
#' @inheritParams run_comparison
#' @param i Row number in feat_sets$sets.
#' @param std.or.alt string, specifies to use either "Standard" or "Alternative" parameters.
#' @param feat_sets_order Logical, should we preserve feat_sets order?
#' @return A list object specifying: ds, colAnns, feat_sets_name (name of custom analysis)
#' @export
subset_feat_sets_ds <- function(ds, feat_sets, i, colAnns, std.or.alt = "Standard", feat_sets_order = T) {
  # Rename dataframes
  sets <- feat_sets$sets
  sets$GroupName <- trimws(sets$GroupName)
  params <- feat_sets$params
  
  # Parameter column
  param_col <- paste(std.or.alt, "Parameter", sep="_")

  # Name of analysis
  name <- sets[i, 1]
  
  # Find list of features ---
  feats <- strsplit(sets$GroupList[sets$GroupName == name], ",") %>% unlist %>% trimws %>% .[!is.na(.)]
  
  while(any(feats %in% sets$GroupName)){
    # Match group names to features
    feats <- suppressMessages(plyr::mapvalues(feats, from=sets$GroupName, to=sets$GroupList))
    # Split strings by commas
    feats <- strsplit(feats, split = ",") %>% unlist() %>% trimws %>% .[!is.na(.)]
  }
  
  # Check whether any colann1/"Feature" combo is duplicated, e.g. TIMP1-Pos.Pix.Perc shows up in more than one place
  # Prevents error: duplication leads to incorrect dimensions for colAnn
  dup <- duplicated(params[, c(param_col, "Feature")])
  # Find logical vector of rows to keep
  rows_feat <- !dup & params$Feature %in% feats

  # Subset to columns in column annotation of interest
  cols_to_keep <- interaction(ds$colAnn[, c(colAnns[2], colAnns[1])]) %in%
    interaction(params[rows_feat, c("Feature", param_col)])
  # # Do not continue with analysis if the parameter and stains don't match the columns in the input data
  if (sum(cols_to_keep) < 3)
    errorCondition(sprintf("In get_feat_sets_ds, can't continue with %s because less than 3 columns.", name))

  # Subset dataset object accordingly
  ds_sub <- subset_dataset(ds, cols_to_keep = cols_to_keep)

  # Prevents error: column order of ds$vals and ds$colAnn are not the same
  if (any(rownames(ds_sub$colAnn) != colnames(ds_sub$vals))) {
    errorCondition(sprintf("In get_feat_sets_ds, can't continue with %s because column names of colAnn and vals don't match.", name))
  }

  # If custom analysis order should be preserved, apply to ds_sub
  if (feat_sets_order) {
    # Get order from custom analysis
    rows_feat_order <-
      paste(params[rows_feat, "Feature"], params[rows_feat, param_col], sep = "_")
    # Get current order
    colAnn_order <- paste(ds_sub$colAnn[, colAnns[2]], ds_sub$colAnn[, colAnns[1]], sep = "_")
    # Get new order
    new_order <- match(rows_feat_order, colAnn_order) %>%
      colnames(ds_sub$vals)[.]

    # Rename rows
    ds_sub <- sort_dataset(ds_sub, col_order = new_order)
  }

  # Return result as list
  list(
    feat_sets_name = ifelse(std.or.alt == "Alternative", paste0(name, "_alt"), name),
    ds = ds_sub,
    colAnns = colAnns
  )
}
