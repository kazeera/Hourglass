#' Functions defined in this file:
#'   cluster_df
#'   cluster_corrmat

#' Re-orders a matrix by hierarchical clustering
#'
#' @param df3 Numeric matrix or data frame
#' @param dist_method The distance measure to be used. Accepts the same values as method parameter in ?dist.
#' @param link_method The agglomeration/clustering method to be used. Accepts the same values as method parameter in ?hclust.
#' @param clust.by.row Logical indicating whether to cluster by row, else columnwise.
#' @return An ordered matrix based on hierarchical clustering
#' @export
cluster_df <- function(df3, dist_method = "euclidean", link_method = "ward.D", clust.by.row = T) {
  # cluster_method <- "spearman"
  if (clust.by.row) {
    # # Cluster rows
    # Ward Hierarchical Clustering
    d <- dist(df3, method = dist_method) # distance matrix
    hc <- hclust(d, method = link_method)
    # Reorder rows based on hierarchal clustering
    df3 <- df3[hc$order, ]
  } else {
    # Cluster columns (genes)
    # hc <- hclust(as.dist(1-cor(df3, method = cluster_method)))
    # Ward Hierarchical Clustering
    d <- dist(t(df3), method = dist_method) # distance matrix
    hc <- hclust(d, method = link_method)
    # Reorder columns based on hierarchal clustering
    df3 <- df3[, hc$order]
  }
  return(df3)
}

#' Re-orders a correlation matrix by hierarchical clustering
#'
#' @param mat Correlation matrix or data frame
#' @param dist_method The distance measure to be used. Accepts the same values as method parameter in ?dist.
#' @param link_method The agglomeration/clustering method to be used. Accepts the same values as method parameter in ?hclust.
#' @return An ordered matrix based on hierarchical clustering
#' @export
cluster_corrmat <- function(mat, dist_method = "euclidean", link_method = "complete") {
  # Use correlation between variables as distance
  dist_mat <- dist((1 - mat) / 2, method = dist_method)
  hc <- hclust(dist_mat, method = link_method)
  # Returned reorder mat
  mat[hc$order, hc$order]
}
