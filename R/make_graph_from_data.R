#' Make a graph object from data set containing weighted edgelist
#'
#' Upload a dataset where the first row is the destination firm and the second 
#' column is the origin firm, and the third column is the number of workers that
#' transitioned from the origin firm to the destination firm, and make a graph object.
#'
#' @param filepath Path to the input file
#' @return Graph representing the transition matrix
#' @export
make_graph_from_data = function(filepath) {
  df = read.csv(filepath,
                col.names = c("firmid", "lagfirm", "N_workers"))
  g = graph_from_data_frame(df, directed = TRUE)
  return(g)
}