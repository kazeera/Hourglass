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
  # Name of analysis
  feat_sets_name <- feat_sets$sets[i, 1]
  if(std.or.alt == "Alternative")
    feat_sets_name <- paste0(feat_sets_name, "_alt")
  # Find list of features ---
  # This line splits the features into a vector: eg. "1,2,3,4" turns into "1" "2" "3" "4"
  all <- feats <- strsplit(feat_sets$sets[i, 2], split = ",") %>% unlist()
  # Independent features (not part of a group)
  indiv_feats <- paste(all[! all %in% feat_sets$sets$GroupName], collapse=",")
  # Find features of each group recursively
  while(any(feats %in% feat_sets$sets$GroupName)){
    true <- feats %in% feat_sets$sets$GroupName
    # Match group names to features
    feats <- suppressMessages(plyr::mapvalues(feats[true], from=feat_sets$sets$GroupName, to=feat_sets$sets$GroupList))
    # Split strings by commas
    feats <- strsplit(feats, split = ",") %>% unlist()
  }
  # Combine independent features and features from groups
  if(any(all %in% feat_sets$sets$GroupName)){
    indiv_feats <- paste(paste(feats, collapse=","), indiv_feats, sep=",")
  }

  # Check whether any colann1/"Feature" combo is duplicated, e.g. TIMP1-Pos.Pix.Perc shows up in more than one place
  # Prevents error: duplication leads to incorrect dimensions for colAnn
  dup <- duplicated(feat_sets$params[, c(param_col, "Feature")])
  # Find logical vector of rows to keep
  rows_feat <- !dup & feat_sets$params$Feature %in%  unlist(strsplit(indiv_feats, split = ","))

  # Parameter column
  param_col <- paste(std.or.alt, "Parameter", sep="_")

  # Subset to columns in column annotation of interest
  cols_to_keep <- interaction(ds$colAnn[, c(colAnns[2], colAnns[1])]) %in%
    interaction(feat_sets$params[rows_feat, c("Feature", param_col)])
  # # Do not continue with analysis if the parameter and stains don't match the columns in the input data
  if (sum(cols_to_keep) < 3)
    errorCondition(sprintf("In get_feat_sets_ds, can't continue with %s because less than 3 columns.", feat_sets_name))

  # Subset dataset object accordingly
  ds_sub <- subset_dataset(ds, cols_to_keep = cols_to_keep)

  # Prevents error: column order of ds$vals and ds$colAnn are not the same
  if (any(rownames(ds_sub$colAnn) != colnames(ds_sub$vals))) {
    errorCondition(sprintf("In get_feat_sets_ds, can't continue with %s because column names of colAnn and vals don't match.", feat_sets_name))
  }

  # If custom analysis order should be preserved, apply to ds_sub
  if (feat_sets_order) {
    # Get order from custom analysis
    rows_feat_order <-
      paste(feat_sets$params[rows_feat, "Feature"], feat_sets$params[rows_feat, param_col], sep = "_")
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
    feat_sets_name = feat_sets_name,
    ds = ds_sub,
    colAnns = colAnns
  )
}
