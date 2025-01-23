#' Get revealed preference ranking of firms from data on firm transitions
#'
#' Given a data set containing transitions across firms, 
#'
#' @param file_path Location of the data set
#' @param tol Tolerance level for convergence of the power method
#' @param max_iter Maximum number of iterations for the power method
#' @return Data set with exponential value (expV) for each firm in the largest conneted component.
#' @export
rprank = function(file_path, tol = 1e-6, max_iter = 5e5) {
  g = make_graph_from_data(file_path)
  gc = get_strongly_connected_component(g)
  expV = power_method(gc, tol, max_iter)
  V(gc)$expV = as.vector(expV)
  df = igraph::as_data_frame(gc, what = "vertices") %>% 
    as_tibble() %>% 
    rename(firmid = name) %>% 
    select(firmid, expV)
  return(df)
}