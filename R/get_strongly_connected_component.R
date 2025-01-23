#' Get largest strongly connected component of a graph
#'
#' Given a weighted graph object, return the largest strongly connected component.
#'
#' @param g Graph object
#' @return Subgraph containing the largest strongly connected component
#' @export
get_strongly_connected_component = function(g) {
  clu = components(g, mode = "strong")
  sc = (clu$membership == which.max(clu$csize))
  V(g)$sc = sc
  gc = induced_subgraph(g, V(g)[sc])
  return(gc)
}