#' Generate a spatial layout of checks by density
#'
#' This function generate a check layout in a retangular grid, The check is evenly distributed by the given density
#'
#' @param n.row The row of a retangular grid
#' @param n.col The column of a retangular grid
#' @param by.density The density of the check
#' @return A dataframe will return
#' @export
#' @examples
#' # To produce a 4 by 5 field with check distributed by density=0.2
#' nr = 4; nc = 5
#' example.tmp = equal_space_check_by_density(nr, nc, .2)
#' fields::quilt.plot(example.tmp, nx=nr, ny=nc)

equal_space_check_by_density = function(n.row, n.col, by.density){
  tmp.row = tmp.col = matrix(0, nrow=n.row, ncol=n.col)
  share.n = 1 / by.density
  by.nr = sqrt(share.n*n.row/n.col)
  by.nc = sqrt(share.n*n.col/n.row)
  tmp.row[seq(from=1, to=nrow(tmp.row), by=ceiling(by.nr)), ] = 1
  tmp.col[, seq(from=1, to=ncol(tmp.col), by=ceiling(by.nc))] = 1

  id.cell = tmp.row * tmp.col
  tmp.out = density_check(id.cell, by.density)
  return(tmp.out)
}


