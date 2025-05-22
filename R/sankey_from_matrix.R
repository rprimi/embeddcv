#' Visualize a Matrix as a Sankey Diagram
#'
#' Creates an interactive Sankey diagram from a matrix, linking rows (sources) to columns (targets)
#' with link thickness proportional to the matrix values. Optionally filters out links below a threshold.
#'
#' @param m Numeric matrix. The matrix to visualize (rows = sources, columns = targets).
#' @param value Numeric scalar (default = 0.40). Minimum value for a link to be included in the graph.
#'             Links with values less than or equal to this threshold are omitted.
#' @param ... Additional arguments passed to \code{networkD3::sankeyNetwork()}.
#'
#' @return A Sankey diagram as produced by \code{networkD3::sankeyNetwork()}.
#' @details Row and column names of the matrix are used as node names. Link values below the threshold are omitted.
#'
#' @importFrom networkD3 sankeyNetwork
#' @examples
#' # Example usage:
#' # sankey_from_matrix(m = your_matrix, value = 0.4)
#'
#' @export
sankey_from_matrix <- function(m, value = 0.40, ...) {
 # Require package
 if (!requireNamespace("networkD3", quietly = TRUE)) {
  stop("Package 'networkD3' is required but not installed.")
 }
 
 # Convert matrix to long-form data.frame
 df <- as.data.frame(as.table(m))
 colnames(df) <- c("source", "target", "value")
 
 # Filter out very small links if desired (optional)
 df <- df[df$value > value, ]
 
 # Prepare node list
 nodes <- data.frame(name = unique(c(as.character(df$source), as.character(df$target))))
 
 # Map source and target to node indices (zero-based for networkD3)
 df$source_id <- match(df$source, nodes$name) - 1
 df$target_id <- match(df$target, nodes$name) - 1
 
 # Draw Sankey diagram
 networkD3::sankeyNetwork(
  Links = df,
  Nodes = nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 30,
  ...
 )
}
