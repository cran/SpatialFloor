#' Generate a random spatial layout of check
#'
#' This function generate a random spatial layout of checks
#' @param n.row The row of the retangular grid
#' @param n.col The column of the retangular grid
#' @param by.density The density of checks
#' @export
#' @examples
#' nr=nc=5
#' tmp = random_check_by_density(nr, nc, .1)
#' fields::quilt.plot(tmp, nx=nr, ny=nc)

random_check_by_density = function(n.row, n.col, by.density){
  m = matrix(1, nrow=n.row, ncol=n.col)
  tmp.out = density_check(m, by.density)
  return(tmp.out)
}
