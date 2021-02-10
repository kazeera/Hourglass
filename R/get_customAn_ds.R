#' Subset dataset for custom analysis
#' 
#' Get a subset of dataset list object from custom analysis table
#'
#' @param ds A dataset object (a list with vals, rowAnn, colAnn, comparison, name).
#' @param customAn A list of 2 data frames for plotting specific rows and columns. @seealso customAn read in using \code{\link{import_custom_analysis_file}}, see description of file.
#' @param i Row number in customAn$keys. 
#' @param new_colAnns Name of new column annotations in custom annotations file
#' @param customAn_order Logical, should we preserve customAn order?
#'
#' @return A list object specifying: ds, colAnns, customAn_name (name of custom analysis)
#' @export
#'
#' @examples results <- get_customAn_ds(ds, customAn, i = 1)
get_customAn_ds <- function(ds, customAn, i, new_colAnns = NA, customAn_order = T){

  # Get columns to annotate from Custom Analysis, ie, Stain, ECMorImmune, etc
  if(all(is.na(new_colAnns))) {
    n <- unique(customAn$keys[, 2]) %>% length() # num col annotations
    new_colAnns <- ncol(customAn$values) - 2 * n # num of new colAnns, this is correct
    new_colAnns <- colnames(customAn$values)[2:new_colAnns] # name of col anns
  }
  
  # Name of analysis
  customAn_name <- customAn$keys[i, 1]
  # This line splits the group numbers into a vector: eg. "1,2,3,4" turns into "1" "2" "3" "4"
  grp.num <- strsplit(customAn$keys[i, "Group.Numbers"], split = ",") %>% unlist()
  # Get type: ex. standard or PPC?
  grp <- customAn$keys[i, "Group"]
  # Get the rows in stain info
  rows_customAn <- customAn$values[, paste(grp, "Group.Numbers", sep = "_")] %in% grp.num
  # Which columns are we stratifying by in colAnn
  cn <- colnames(customAn$values)
  colAnn2 <- cn[1] # Stain, this is right
  # Get the column of interest, ex. "Parameter" from "PPC_Parameter"
  colAnn1 <- (grepl(grp, cn) & !grepl("Group.Numbers", cn)) %>%
    cn[.] %>%
    get_nth_part("_", 2)
  colAnn1_custom <- paste(grp, colAnn1, sep = "_")
  customAn$values[, colAnn1] <- customAn$values[, colAnn1_custom]
  # Get all new column annotations
  colAnns <- c(colAnn1, colAnn2, new_colAnns) %>% unique()
  
  # Check whether any colann1/colann2 combo is duplicated, e.g. TIMP1-Pos.Pix.Perc shows up in more than one place
  # Prevents error: duplication leads to incorrect dimensions for colAnn
  dup <- duplicated(customAn$values[, c(colAnn2, colAnn1_custom)])
  rows_customAn <- !dup & rows_customAn
  
  # Subset to columns in column annotation of interest
  cols_to_keep <- interaction(ds$colAnn[, c(colAnn2, colAnn1)]) %in%
    interaction(customAn$values[rows_customAn, c(colAnn2, colAnn1_custom)])
  # # Do not continue with analysis if the parameter and stains don't match the columns in the input data
  if (sum(cols_to_keep) < 3) next # next in loop
  
  # Subset dataset object accordingly
  ds_sub <- subset_dataset(ds, cols_to_keep = cols_to_keep)
  
  # Prevents error: column order of ds$vals and ds$colAnn are not the same
  if (any(rownames(ds_sub$colAnn) != colnames(ds_sub$vals))) {
    errorCondition(sprintf("In get_customAn_ds, can't continue with %s because column names of colAnn and vals don't match.", customAn_name))
    # colnames(ds_sub$vals) <- paste(ds_sub$colAnn[, colAnn2], ds_sub$colAnn[, colAnn1], sep = "_")
    # # Make unique column to merge by
    # df <- reform_ann_df(customAn$values[rows_customAn, ], new_colAnns)
    # df$MergeID <- paste(customAn$values[rows_customAn, colAnn2], customAn$values[rows_customAn, colAnn1_custom], sep = "_")
    # ds_sub$colAnn$MergeID <- paste(ds_sub$colAnn[, colAnn2], ds_sub$colAnn[, colAnn1], sep = "_")
    # 
    # # Merge with annotations from values Excel sheet
    # ds_sub$colAnn <- merge(x = df[c("MergeID", new_colAnns)], y = ds_sub$colAnn, by = "MergeID")
    # # Rename rows
    # rownames(ds_sub$colAnn) <- ds_sub$colAnn$MergeID
    # ds_sub <- sort_dataset(ds_sub, col_order = colnames(ds_sub$vals))
  }
  
  # If custom analysis order should be preserved, apply to ds_sub
  if(customAn_order){
    # Get order from custom analysis
    rows_customAn_order <- 
      paste(customAn$values[rows_customAn, colAnn2], customAn$values[rows_customAn, colAnn1_custom], sep = "_")
    # Get current order
    colAnn_order <- paste(ds_sub$colAnn[, colAnn2], ds_sub$colAnn[, colAnn1], sep = "_")
    # Get new order
    new_order <- match(rows_customAn_order, colAnn_order) %>% 
      colnames(ds_sub$vals)[.]
    
    # Rename rows
    ds_sub <- sort_dataset(ds_sub, col_order = new_order)
  }
  
  # Return result as list
  list(
    customAn_name = customAn_name, 
    ds = ds_sub,
    colAnns = colAnns
  )
}
