#' Isotropic spatial simulation wrapper
#'
#' This function load grid floor and generate the simulated floor
#'
#' @param D The distance matrix
#' @param cov.var the var-cov function of producing var-cov matrix. 'exp' is exponential, 'gau' is Gaussian
#' @param nugget the nugget value of the var-cov function
#' @param sill the sill(variance, sigma^2) of the var-cov function
#' @param ranges the range value of var-cov function
#' @param mu the mean value of the simulated spatial floor
#' @return A vector with the same length of grid floor
#' @export
#' @examples
#' D = spatial_dist(expand.grid(1:5, 1:5))
#' iso_spatial_floor(D, sill=50, ranges=2)



iso_spatial_floor = function(D, cov.var='exp', nugget=0, sill, ranges, mu=0){
    cov.tmp = iso_spatial_cov(D, cov.var=cov.var, nugget=nugget, sill=sill, ranges=ranges)
    floor = t(chol(cov.tmp)) %*% rnorm(nrow(cov.tmp), 0 ,1) + mu
    return(floor)
  }
