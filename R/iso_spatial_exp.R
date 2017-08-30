#' Isotropic spatial simulation by exponential function
#'
#' This function load grid floor and generate the var-cov matrix via exponential function
#'
#' @param dist.matrix the a distance matrix generate from spatial_dist function
#' @param nugget the nugget value of the var-cov function
#' @param sill the sill(variance, sigma^2) of the var-cov function
#' @param ranges the range value of var-cov function
#' @return A matrix of var-cov matrix
#' @export
#' @examples
#' D = spatial_dist(expand.grid(1:5, 1:5))
#' iso_spatial_exp(D, 0, 3, 1)

iso_spatial_exp = function(dist.matrix, nugget, sill, ranges){
  cov.tmp = sill * exp(-dist.matrix/ranges)
  diag(cov.tmp) = sill + nugget
  return(cov.tmp)
}
