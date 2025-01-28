#' Apply 'power method' iterated fixed point algorithm to get the exp(V) vector
#' from Sorkin (2018).
#'
#' @param g Weighted graph object
#' @param tol Tolerance for fixed-point algorithm
#' @param max_iter Maximum number of iterations for fixed-point algorithm
#' @return exp(V) a vector of values (see paper for details)
#' @export
power_method = function(g, tol = 1e-6, max_iter = 5e5) {
  
  #prepare matrix
  M = as_adjacency_matrix(g, attr = "N_workers", sparse = TRUE)
  Sinv = solve(Diagonal(nrow(M), colSums(M)))
  SinvM = Sinv%*%M
  
  #initialize guess
  expV_init = rnorm(nrow(SinvM))
  expV_init = abs(expV_init/sum(expV_init))
  
  #initialize while loop parameters
  iter = 1
  gap =  200
  
  expV_guess = expV_init
  
  while (gap > tol & iter < max_iter) {
    expV_new = SinvM%*%expV_guess
    gap = sum(abs(expV_new - expV_guess))
    expV_guess = expV_new
    iter = iter + 1
  }
  
  cat("Fixed-point algorithm converged after", iter, "iterations.")
  
  return(expV_guess)
}
