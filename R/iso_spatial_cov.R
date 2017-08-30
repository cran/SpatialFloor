#' Isotropic spatial var-cov wrapper
#'
#' This function load grid floor and generate the var-cov matrix
#'
#' @param D The distance matrix
#' @param cov.var the var-cov function of producing var-cov matrix. 'exp' is exponential, 'gau' is Gaussian
#' @param nugget the nugget value of the var-cov function
#' @param sill the sill(variance, sigma^2) of the var-cov function
#' @param ranges the range value of var-cov function
#' @return A matrix of var-cov matrix
#' @export
#' @examples
#' D = fields::rdist(expand.grid(1:5, 1:5))
#' iso_spatial_cov(D, sill=3, ranges=1)


iso_spatial_cov = function(D, cov.var='exp', nugget=0, sill, ranges){

  if(cov.var=='exp'){
    var.data = iso_spatial_exp(D, nugget=nugget, sill=sill, ranges=ranges)
  } else if(cov.var=='Gaussian'){
    var.data = iso_spatial_gau(D, nugget=nugget, sill=sill, ranges=ranges)
  }
}
