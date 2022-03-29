#' Average a dataframe with continuous and discrete variables columnwise.
#'
#' Average a dataframe with continuous and discrete variables columnwise.
#'
#' @param df A data frame to average across. Note: Only numeric columns will be averaged, while discrete columns will retain the first instance for each group.
#' @param group_by Name or column of index to average valuesby.
#' @param rows_to_keep Numeric, logical or character vectors of rows to keep in subset (length must be number of rows of df). Note: character for named rows only
#' @param cols_to_keep Numeric, logical or character vectors of columns to keep in subset (length must be number of columns of df). Note: character for named columns only
#' @param sort_cols logical, should we sort columns alphabetically?
#' @return A data frame averaged column by column.
#' @export
avg_dataframe <- function(df, group_by = 1, rows_to_keep = NULL, cols_to_keep = NULL, sort_cols = FALSE) {
  # Subset data
  df <- subset_dataframe(df, rows_to_keep, cols_to_keep)

  # Sort rows by patient (Case ID)
  df <- df[, group_by] %>%
    order() %>%
    df[., ]

  # Numeric columns in scaffold
  num_cols <- lapply(df, is.numeric) %>% unlist()

  # b) Get means of rows base on Case_ID duplicates, remove NA values
  # https://stackoverflow.com/questions/3505701/grouping-functions-tapply-by-aggregate-and-the-apply-family

  # Get an average across group IDs
  df1 <- aggregate(df[, num_cols], by = list(df[, group_by]), mean, na.rm = TRUE)
  df1 <- rename_column(df1, "Group.1", group_by)

  # Get non-duplicated rows from non-numeric cols (values should be the same for each Case_ID)
  x <- df[, group_by] %>% duplicated()
  df2 <- df[!x, !num_cols, drop=F]

  # Merge the numeric and non numeric columns
  avg_df <- merge(x = df2, y = df1, all = T, by = group_by)

  # If there are any NA values in Case ID, let's remove them
  if (any(is.na(avg_df[, group_by]))) {
    x <- avg_df[, group_by] %>% is.na()
    avg_df <- avg_df[!x, ]
  }

  # Sort columns alphabetically
  if (sort_cols) {
    avg_df <- avg_df[, c(group_by, sort(colnames(avg_df)[-1]))]
  }
  return(avg_df)
}

#' Averages a dataset object.
#'
#' Averages a dataset object.
#'
#' @param ds A dataset object (a list with any of vals, rowAnn, colAnn).
#' @param group_by Name or index of column in ds$rowAnn to average values by.
#' @param rows_to_keep Numeric, logical or character vectors of rows to keep in ds$vals (length must be number of rows of ds$vals). Note: character if rows are named.
#' @param cols_to_keep Numeric, logical or character vectors of columns to keep in ds$vals(length must be number of columns of ds$vals). Note: character if columns are named.
#' @param sort_cols logical, should we sort columns alphabetically?
#' @return A dataset subsetted specified rows/columns and averaged.
#' @export
avg_dataset <- function(ds, group_by = 2, new_name = "Averaged", rows_to_keep = NULL, cols_to_keep = NULL) {
  # Get column name of group_by if it's an index
  if (is.numeric(group_by)) {
    group_by <- colnames(ds$rowAnn)[group_by]
  }

  # Average rowAnn
  rowAnn <- avg_dataframe(ds$rowAnn, group_by, rows_to_keep)

  # Make values data frame by appending group ID as first column
  vals <- data.frame(first = ds$rowAnn[, group_by], ds$vals)
  colnames(vals)[1] <- group_by

  # Average vals across group ID
  vals <- avg_dataframe(vals, group_by, sort_cols = T)
  
  # Row names
  rownames(vals) <- rownames(rowAnn) <- vals[, 1]
  vals <- vals[, -1]

  # Make column annotations
  colAnn <- subset_dataframe(ds$colAnn, rows_to_keep = cols_to_keep)

  # Return new dataset
  list(
    vals = vals,
    rowAnn = rowAnn,
    colAnn = colAnn,
    name = new_name
  )
}
