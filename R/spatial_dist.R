#' Distance matrix calculation
#'
#' This function load grid floor and generate the distance matrix
#'
#' @param M A data.frame contains grid coordinates
#' @param lon.lat If TRUE, the function will use longitude and latitude for calculating geographic distance 
#' @param mile If TRUE, the function will yeild distance in mileages. If FALSE, the function will yeild distance in kilometers
#' @return A matrix of distance matrix
#' @import fields
#' @export
#' @examples
#' spatial_dist(expand.grid(1:4, 1:4))

spatial_dist = function(M, lon.lat=F, mile=F){
  if(lon.lat){
    fields::rdist.earth(M, miles=mile)
  } else {
    base::as.matrix(stats::dist(M, method='manhattan'))
  }
}
