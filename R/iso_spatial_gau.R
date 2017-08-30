#' Isotropic spatial simulation by Gaussian function
#'
#' This function load grid floor and generate the var-cov matrix via Gaussian function
#'
#' @param dist.data the a distance matrix generate from spatial_dist function
#' @param nugget the nugget value of the var-cov function
#' @param sill the sill(variance, sigma^2) of the var-cov function
#' @param ranges the range value of var-cov function
#' @return A matrix of var-cov matrix
#' @export
#' @examples
#' D = spatial_dist(expand.grid(1:5, 1:5))
#' iso_spatial_gau(D, 0, 3, 1)

iso_spatial_gau = function(dist.data, nugget, sill, ranges){
  cov.tmp = sill * exp( -(dist.data/ranges)^2 )
  diag(cov.tmp) = sill + nugget
  return(cov.tmp)
}
