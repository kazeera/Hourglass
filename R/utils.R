#' Install/load packages from library into environment
#'
#' Checks whether R packages are installed from CRAN and loads
#' all. Does not yet support installation of BioConductor packages.
#'
#' @param pkgs A character vector of package names.
#' @examples
#' load_packages(c("openxlsx", "dplyr"))
#' @export
load_packages <- function(pkgs) {
  # Install packages that are not found
  new.pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new.pkgs)) {
    install.packages(new.pkgs)
  }
  # Load all packages
  lapply(pkgs, require, character.only = TRUE)
}



#' Closes all open file connections
#'
#' @export
turn_off_null_devices <- function() {
  suppressWarnings(
    # While there are open file connections, turn off dev.
    while (!is.null(dev.list())) {
      dev.off()
    }
  )
}


#' Extracts the first (or nth) entry from each split.
#' @param x A character vector.
#' @param delimiter A character to split by.
#' @param entry_no A number indicating which part of split to return.
#' @return A character vector containing parts
#' @examples
#' x <- c("2wk.1", "2wk.2", "2wk.3", "2wk.4", "2wk.5", "2wk.6", "2wk.7", "2wk.8", "2wk.9", "2wk.10")
#' # delimiter = "\\." # double slashes are escape characters for special characters like "."
#' # entry_no = 1L
#' get_nth_part(x, "\\.", "\\.")
#' # The return value is c("2wk", "2wk", "2wk", "2wk", "2wk", "2wk", "2wk", "2wk", "2wk", "2wk", "2wk", "2wk")
#' get_nth_part(x, "wk", 1)
#' @export
get_nth_part <- function(x, delimiter, entry_no) {
  sapply(strsplit(x, delimiter), `[`, entry_no)
}

#' Coerce a data.frame to a numeric.data.frame
#'
#' @param df Numeric data frame or matrix
#' @param row.or.col A number indicating direction to apply function: either 1 for row-wise, 2 (default) for column-wise
#' @return A data frame in which all columns/rows are numeric
#' @export
df_to_numeric <- function(df, row.or.col = 2) {
  # Coerce each row or column to numeric
  df2 <- apply(df, row.or.col, function(x) {
    if (is.numeric(x)) {
      x
    } else {
      suppressWarnings(as.numeric(x))
    }
  }) %>% as.data.frame()

  # Reinstate row and column names
  rownames(df2) <- rownames(df)
  colnames(df2) <- colnames(df)

  return(df2)
}

#' Subset data frame to rows that are duplicated at least once
#'
#' @param d A data frame
#' @param col Column in d that you want duplicates from
#' @param rm.NA Column you want to remove duplicates or zeros from prior to finding duplication (optional)
#' @return A data frame in which only duplicated rows of col exist
#' @export
get_duplicated_cases <- function(d, col, rm.NA = NA) {
  # Remove NAs from the columns specified
  if (!is.na(rm.NA)) {
    d <- d[!is.na(d[, rm.NA]) | d[, rm.NA] == 0, ]
  }

  # Get "col" groups that are duplicated at least once
  r <- table(d[, col]) %>%
    .[. > 1] %>%
    names()

  # Subset to only these rows
  d <- d[d[, col] %in% r, ]

  return(d)
}

#' Rename a column in a data frame.
#'
#' @param df A data frame.
#' @param current_col Column name in df that you wish to rename.
#' @param new_name New column name.
#' @return The dataframe with the column renamed.
#' @export
rename_column <- function(df, current_col, new_name){
  if(current_col %in% colnames(df)){
    colnames(df)[which(colnames(df) == current_col)] <- new_name
  }
  return(df)
}

#' Checks whether a vector has at least a certain number of non NA/Nan/NULL values
#'
#' @param v A vector (any type)
#' @param n A threshold of non NA values
#' @param row.or.col A number indicating direction to apply function: either 1 for row-wise, 2 (default) for column-wise.
#' @return A logical indicating whether the number of non-NA values is above the threshold
#' @export
has_at.least_n.vals <- function(v, n = 0, row.or.col = 1) {
  # For each row/column, get number of values that are not NA or NaN
  y <- apply(v, row.or.col, function(x) sum(!is.na(x)))

  # Return T/F whether each row/col meets the threshold
  y >= n
}

#' Checks which rows or columns have number of NAs less than or equal to NA threshold
#'
#' @param df Data frame or matrix
#' @param row.or.col A number indicating direction to apply function: either 1 for row-wise, 2 (default) for column-wise.
#' @param NA_thres A number for NA threshold. 0 means that row/col has no NAs
#' @return A logical vector indicating whether the number of non-NA values is above the threshold: number of NA values <= NA threshold
#' @export
has_less.than.eq.to_NA.thres <- function(df, NA_thres, row.or.col = 1) {
  # Get number of elements per row/col and direction (dir) for apply function
  # Count number of non-NAs per row/column
  num_NAs <- apply(mat, row.or.col, function(x) sum(is.na(x)))

  # Say first row has 0 NAs and NA_threshold is 0 (meaning all cores complete), The first element of the returned vector will be TRUE
  num_NAs <= NA_thres
}

#'  Replaces values in a vector that equal to certain values to a new value
#'
#'  @param v A vector
#'  @param orig_vals A vector of values in v to replace
#'  @param new_val The value to replace original values with
#'  @return A vector like v, except the orig_vals have been replaced with new_val
#'  @export
bin_vars <- function(v, orig_vals, new_val) {
  v[v %in% orig_vals] <- new_val
  return(v)
}

#' Abbreviate elements
#'
#' Split string elements in character vector or words in a string and abbreviate to specified number of characters.
#'
#' @param v A character vector.
#' @param split Character delimiter to split by. Note: escape characters still apply, e.g. for ".", split = "\\."
#' @param trim_x Number, indicating the number of characters for each part (that is, length of truncated output string).
#' @param paste_back Logical - paste the split elements back together?
#' @return The abbreviated charactere elements
#' @examples
#' trim_each_part("abbreviate this", sep = " ")
#' z <- paste(rep("high", 4), rep(c("low", "intermediate"), each = 2), sep = "/")
#' # [1] "high/low"          "high/low"          "high/intermediate" "high/intermediate"
#' trim_each_part(paste(x, y, sep = "/"), split = "/")
#' # [1] "hig/low" "hig/low" "hig/int" "hig/int"
#' trim_each_part(z, split = "/", trim_x = 2)
#' # [1] "hi/lo" "hi/lo" "hi/in" "hi/in"
#' trim_each_part(z, split = "/", paste_back = F)
#' # [1] "hig" "low" "hig" "low" "hig" "int" "hig" "int"
#' @export
trim_each_part <- function(v, split = ",", trim_x = 3, paste_back = T) {
  # split groups e.g. "x/y" --> "x" "y"
  v_list <- strsplit(as.character(v), split = split) %>%
    # Trim each part to numeric specified
    lapply(function(x) substring(x, 1, trim_x))

  # Paste back together
  if (paste_back) {
    v_list <- lapply(v_list, function(x) paste(x, collapse = split))
  }

  # Return
  unlist(v_list)
}

#' Split string and iteratively return parts pasted to first part
#'
#' @param x A character vector.
#' @param delimiter A character to split by.
#' @param un_list A logical value whether to unlist
#' @examples
#' split_one.by.one("Heatmaps/BY.CORE/TMA.stromal.subtype_all excl HRD excl neo dots Moffitt/Surv", "/")
#' returns:
# [1] "Heatmaps"
# [2] "Heatmaps/BY.CORE"
# [3] "Heatmaps/BY.CORE/TMA.stromal.subtype_all excl HRD excl neo dots Moffitt"
# [4] "Heatmaps/BY.CORE/TMA.stromal.subtype_all excl HRD excl neo dots Moffitt/Surv"
#' @return
#' @export
split_one.by.one <- function(x, delimiter, un_list = T) {
  # Split vector by delimiter
  x <- unlist(strsplit(x, split = delimiter))
  # Recursively add
  l <- lapply(1:length(x), function(i) {
    paste(x[1:i], collapse = delimiter)
  })
  # Return list/vector
  if (un_list) {
    unlist(l)
  } else {
    l
  }
}

#' Get map of levels in numeric vector
#'
#' Maps the values in a numeric vector as quantiles, default is low, intermediate, and high,
#'
#' @param v A numeric vector
#' @param n_quantiles Number of quantiles/levels to separate v by. Default 3. Minimum 2.
#' @param return_num Logical indicating whether to return the number of quantile or not
#' @param add A string to add to the beginning of each levels in the result, e.g If add is "gene", the result will be "gene.low", "gene.int", gene.high"
#' @param levels A list of 3 elements (l, i, h) representing quantiles, default is 1 = low, 2 = intermediate, 3 = high. If n_quantiles > 3, the "middle" levels will be "int", "int.plus", "int.plus.plus" and so on. Note: If "LEVELS" is defined in global environment, this variable will be used.
#' @return A vector with the same length as v, where each element represents the quantile, either in numeric form (if return_num is TRUE) or characters with the legend represented by the levels parameter
#' @examples
#' get_levels(1:9)
#' # [1] "low"      "low"      "low"      "intermed" "intermed" "intermed" "high"     "high"     "high"
#' get_levels(1:9, return_num = T)
#' # [1] 1 1 1 2 2 2 3 3 3
#' @export
get_levels <- function(v, n_quantiles = 3, add = NA, return_num = F) {

  if (!"LEVELS" %in% ls(envir = .GlobalEnv)) {
    LEVELS <- list(l = "low", i = "int", h = "high")
  }
  # Assign quantile to vector # e.g. if n_quantiles = 3, we will assign each value in v to which quartile it belongs in (1 to 4)
  w <- as.integer(cut(v, quantile(v, 0:n_quantiles / n_quantiles, na.rm = T, names = FALSE), include = TRUE))

  # If the quantile number is just needed, return
  if (return_num) {
    return(w)
  } else {
    # usually n_quantiles is 3, so we only have 3 levels (hi, med, low), but if n_quantiles > 3 then there are more intermediates
    # Account for intermediate - labelled as intermediate, intermediate.1, intermediate.2, etc. depending on n_quantiles
    if (n_quantiles > 2){
      # Get intermediate levels (numeric), e.g. 1,2
      int_values <- 2:(n_quantiles - 1)
      # Get intermediate levels (string), e.g. "int_1","int_2"
      int_labels <- make.unique.2(rep(LEVELS$i, times = length(int_values)), sep = "_")
      # If there is only 1 level, it's just "int"
      if (length(int_values) == 1) {
        int_labels <- "int"
      }
      # Replace numbers with string labels
      y <- mapvalues(w, c(1, int_values, n_quantiles), c(LEVELS$l, int_labels, LEVELS$h))
    } else {
      # Just "low", "high"
      y <- mapvalues(w, c(1, n_quantiles), c(LEVELS$l, LEVELS$h))
    }
    # Add the "add" label to beginning, eg. "low" becomes "TIMP1.low"; NAs stay as NAs
    if (!is.na(add)) {
      y[!is.na(y)] <- paste(add, y[!is.na(y)], sep = ".")
    }
    return(y)
  }
}

#' R make.unique starting in 1
#'
#' R make.unique starting in 1. Example: x,x.1,x.2,x.3 becomes x.1,x.2,x.3,x.4.
#'
#' @param x A numeric vector
#' @param sep Delimeter between value and number
make.unique.2 <- function(x, sep='.'){
  ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
}

#' Make a custom row annotation column
#'
#' Make new rowAnn column of expression level (low, int, high) of a specific continuos variable in ds$rowAnn or ds$vals
#'
#' @param ds A dataset object (a list with vals, rowAnn, colAnn, comparison, name).
#' @param col_name A column name of a continuous numeric variable in either ds$rowAnn or ds$vals.
#' @param n_quantiles Number of quantiles/levels to separate custom column by. Default 3.
#' @return A list of 2 elements named: 1) rowAnn1 = new row annotation column name, 2) rowAnn = new ds$rowAnn with the rowAnn1 column
#' @export
add_to_rowAnn <- function(ds, col_name, n_quantiles = 3) {
  # Make a new column name in rowAnn for the stain expression (low, high, med)
  rowAnn1 <- col_name

  # Retrieve the values of the column/continuous variable
  if (col_name %in% colnames(ds$vals)) {
    v1 <- ds$vals[, col_name]
  }
  if (col_name %in% colnames(ds$rowAnn)) {
    v1 <- ds$rowAnn[, col_name]
    rowAnn1 <- paste(rowAnn1, "level")
  }

  # Assign each value in this rowAnn to a quantile (n=3)
  v2 <- get_levels(v1, n_quantiles = n_quantiles)

  # Add new group to row annotations
  ds$rowAnn[, rowAnn1] <- v2

  # Return two new groups
  list(
    rowAnn1 = rowAnn1,
    rowAnn = ds$rowAnn
  )
}

#' Prints the number of unique elements in each column
#'
#' @param msg String description of data frame
#' @param df A data frame
#' @param column_names A character vector of which columns to search
#' @export
print_unique_elements_count <- function(msg, df, column_names) {
  # Analyze variables in scaffold
  print(msg)
  dim(df)

  # Number of unique values in scaffold variables:
  unique_elements_lengths <- data.frame(Length_Unique_Elements = apply(df, 2, function(x) {
    return(length(unique(x)))
  }))

  # print("Number of unique values in scaffold variables:")
  impt_classifiers <- which(colnames(df) %in% column_names)
  for (i in impt_classifiers) {
    print(colnames(df)[i])
    print(table(df[, i]))
  }
}

#' Converts factor to number
#'
#' @param x A factor
#' @return A numeric with factor levels??
#' @examples
#' str(ToothGrowth)
#' x <- ToothGrowth$supp
#' @export
as_numeric_factor <- function(x) {
  as.numeric(levels(x))[x]
}

#' Subset data frame by columns
#'
#' Prevents subset of data frame with one column from becoming a vector.
#'
#' @param ann_df A data frame with row names
#' @param anns Column names in ann_df desired in output
#' @return The data frame, ann_df, with columns subsetted to names specified by anns or NA if
#' @examples
#' reform_ann_df(DNase, "conc")
reform_ann_df <- function(ann_df, anns) {
  found <- anns %in% colnames(ann_df)
  # Return NA if all annotations are not found in column names
  if(isTRUE(all(is.na(anns) |!found)))
    return(NA)
  anns <- anns[found]
  ann_df2 <-
    # only annotation columns that are not NA
    ann_df[, anns[!is.na(anns)]] %>%
    # Prevents from one column annotation from becoming a vector
    data.frame() %>%
    # Rename columns: df %>% rename_at(vars(oldnames), ~ newnames)
    rename_at(colnames(.), ~ c(anns[!is.na(anns)]))
  # Rename rows to match original df
  rownames(ann_df2) <- rownames(ann_df)

  return(ann_df2)
}
