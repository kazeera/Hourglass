#' Get color palette in the form of colorRampPalette functions.
#'
#' Get color palette in the form of colorRampPalette color interpolation functions. Use with get_col_gradient() or get_element_colors() from this package.
#'
#' @param brew_pal character string  or NA. Exact name of RColorBrewer palette. e.g. "RdBu".
#' @param custom  Na or character vector with colors specifying palette order. e.g. c("red", "white", "blue"). Any hex codes or R colors (see colors()) can be used.
#' @param rev logical; should the brew_pal color palette order be reversed?
#' @details Only one of the parameters (brew_pal or custom) must be specified (not NA), otherwise function will use custom only.
#' @examples
#' # Make a color palette of red to blue (RColorBrewer)
#' # The two lines of code below are equivalent
#' get_col_palette(brew_pal = "RdBu")
#' colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))
#' #' # Reverse the palette (i.e. blue to red)
#' get_col_palette(brew_pal = "RdBu", rev = T)
#'
#' # Make a custom color palette of forestgreen to darkorchid4 (RColorBrewer)
#' # The two lines of code below are equivalent
#' get_col_palette(custom = c("forestgreen", "darkorchid4"))
#' colorRampPalette(c("forestgreen", "darkorchid4"))
#' @seealso colorRampPalette function from {grDevices}. See ?colorRampPalette.
#' @return A colorRampPalette color interpolation functions.
#' @export
get_col_palette <- function(brew_pal = NA, custom = NA, rev = F) {
  # If colors for palette aren't specified, return warning message
  if (is.na(brew_pal) & is.na(custom)) {
    stop("Please specify either 'brew_pal' or 'custom'.")
  }

  # Get custom palette based on vector of colors
  if (!is.na(custom)) {
    colorRampPalette(custom) # c(low_col, high_col))
  }

  # Get RColorBrewer palette
  if (!is.na(brew_pal)) {
    # Get max colors of palette
    max_n <- brewer.pal.info[brew_pal, "maxcolors"]
    # Create brewer pal
    # Reverse if required
    if (rev) {
      brew <- rev(brewer.pal(n = max_n, name = brew_pal))
    } else {
      brew <- brewer.pal(n = max_n, name = brew_pal)
    }
    # Return palette
    colorRampPalette(brew)
  }
}

#' Get color gradient
#'
#' Get color gradient for a colorRampPalette with n number of colors.
#'
#' @param colRamp colorRampPalette functions object containing colors for palette. Use function get_col_palette() to create.
#' @param n numeric. Number of colors in gradient
#' @examples
#' x <- LETTERS[1:6]
#' # The three lines of code below are equivalent
#' get_col_gradient(get_col_palette("RdBu"), 100)
#' get_col_palette("RdBu")(100)
#' colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(100)
#' @return A character vector, names of colors (hex).
#' @export
get_col_gradient <- function(colRamp, n = 50) {
  # Create and return gradient
  colRamp(n)
}

#' Get colors for a named vector based on a color palette.
#'
#' Get colors for a named vector based on a color ramp palette.
#'
#' @param v character vector. Unique elements to get colors for.
#' @param colRamp colorRampPalette functions object containing colors for palette. Use function get_col_palette() to create. See ?colorRampPalette .
#' @param rearr logical; should vector be arranged so a color gradient isn't created with the original vector order?
#' @examples
#' x <- LETTERS[1:6]
#' get_element_colors(x, get_col_palette("RdBu"))
#' get_element_colors(x, colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu")))
#' @return A character vector of colors with the same length as v. The names of this vector are the unique elements specified by v.
#' @export
get_element_colors <- function(v, colRamp, rearr = F) {
  # Get unique elements and rearrange
  v <- unique(v)
  # Rearrange
  if (rearr) {
    v <- sample(v)
  }
  # Get color gradient
  myColors <- colRamp(length(v))
  names(myColors) <- v
  # Return character vector of color names
  return(myColors)
}

#' Get color names from unique groups in data frame
#'
#' @param df A data frame with the columns of interest
#' @param ann_cols The columns of interest (get colors for)
#' @param pal A named list. Existing colors in palette
#' @return A vector with color names from unique groups in data frame
#' @export
get_ann_colors <- function(df, ann_cols, pal = "") {
  ann_cols <- ann_cols[ann_cols %in% colnames(df)]
  pal2 <- lapply(ann_cols, function(c) {
    elem <- unique(df[, c])
    # If any colors exist in palette
    pal2 <- pal[names(pal) %in% elem]

    color_exists <- elem %in% names(pal)
    # Otherwise get new element colors (random)
    if (any(!color_exists)) {
      # If the max number of elements exceeds the max brewer palette, then use custom gradient
      if (all(length(elem[!color_exists]) > brewer.pal.info$maxcolors)) {
        colRamp <- get_col_palette(custom = c("forestgreen", "darkorchid4"))
      } else {
        # Get colors from a random palette from RColorBrewer
        repeat{
          palette <- brewer.pal.info %>%
            rownames() %>%
            sample(size = 1) # RColorBrewer and dplyr
          # Exit do-while loop when the max number of colors (max 12) in palette can accommodate all unique elements
          if (brewer.pal.info[palette, "maxcolors"] >= length(elem)) {
            break
          }
        }
        colRamp <- get_col_palette(palette)
      }
      # Get colors of elements
      pal2 <- c(pal2, get_element_colors(elem[!color_exists], colRamp, rearr = T))
    }
    # Return named list
    unlist(pal2, use.names = T)
  })
  names(pal2) <- ann_cols
  # Return new palette
  return(pal2)
}

#' Get color palette for dataset row annotations
#'
#' @param ds A dataset object (a list with vals, rowAnn (required), colAnn)
#' @param rowAnns A character vector of 1-2 column names in ds$rowAnn.
#' @param var_colors A named vector with colors as values and annotations/groups as names.
#' @return A list of 2 elements: 1) ds without NAs in rowAnn1, 2) rowAnns
#' @export
get_rowAnn_color_pal <- function(ds, rowAnns, var_colors = NULL) {
  # Get unique row annotation elements
  elements <- ds$rowAnn[, rowAnns[1]] %>%
    as.character() %>%
    unique()

  # In row annotation 2, replace NA with a code so the cores/samples still show up in box plot
  if (!is.na(rowAnns[2])) {
    # Replace all NAs with NA symbol (so it doesn't get dropped in color coded boxplots)
    na_index <- is.na(ds$rowAnn[, rowAnns[2]]) & !is.na(ds$rowAnn[, rowAnns[1]])
    ds$rowAnn[na_index, rowAnns[2]] <- "NA_"
    # Update unique elements for color palette
    elements <- ds$rowAnn[, rowAnns[2]] %>%
      as.character() %>%
      c(., elements) %>%
      unique()
  }

  # Get color palette
  if (isFALSE(is.null(var_colors))) {
    pal <- var_colors[names(var_colors) %in% elements]
    # If this is a new type of analysis, get new colors
    if (length(pal) == 0) {
      elements <- elements[!is.na(elements)]
      pal <- get_element_colors(elements, colRamp = get_col_palette("Spectral"))
    }
    # Remove the elements not mentioned in the color palette (ie. unncessary elements)
    ds <- subset_dataset(ds, rows_to_keep = ds$rowAnn[, rowAnns[1]] %in% names(pal))
    # Replace the labels in rowAnn2 not found in the palette with NA as well
    if (!is.na(rowAnns[2])) {
      excl_index <- !ds$rowAnn[, rowAnns[2]] %in% names(pal)
      # If any are true (not found in color palette), replace with NA
      if (any(excl_index)) {
        ds$rowAnn[excl_index, rowAnns[2]] <- "NA_"
        pal <- c(pal, var_colors["NA_"])
      }
    }
  } else {
    pal <- get_element_colors(elements, colRamp = get_col_palette("Spectral"))
  }
  return(list(ds = ds, pal = pal))
}

#' Cleans and returns color palette.
#'
#' Cleans color palette object for use by run_hourglass function.
#'
#' @param pal Either 1) Data frame, where column 1 is variables and column 2 is hex codes, 2) List object indicating color palette, where elements are hex codes and element names are variables. e.g. list("Tissue.type-Tumour"="#2f4f4Fff", "Tissue.type-Stroma"="#d2691eff")
#' @param to_remove Vector of hex codes to remove from palette, default is white ("#ffffffff"(,)
#' @return List of colors, where elements are hex codes and element names are variables. As a result of "cleaning", element names don't have 2 parts, are duplicated, and is white e.g. list("Tumour"="#2f4f4Fff", "Stroma"="#d2691eff")
#' @export
clean_colors <- function(pal, to_remove = "#ffffffff", is_df = T) {
  # Convert from dataframe to list
  if (isTRUE(is_df)) {
    x <- pal[, 1]
    pal <- as.list(pal[, 2])
    names(pal) <- x
  }

  # Replace column names with "-"
  # e.g. "Stromal.subtype-intermediary" --> "intermediary"
  names(pal) <- lapply(names(pal), function(x) {
    y <- get_nth_part(x, "-", 1)
    gsub(paste0(y, "-"), "", x)
  }) %>% unlist()

  # Add necessary colors for NA (mainly for when rowAnn2/Subgroup dots needs to be displayed in boxplots)
  pal <- c(pal, NA_ = "#000000ff")

  # Remove duplicated element names
  pal <- pal[unique(names(pal))]

  # Remove white/other colors and return
  pal[!pal %in% to_remove]
}
