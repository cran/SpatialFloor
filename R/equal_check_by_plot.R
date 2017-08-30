#' Generate a spatial layout of checks by every # of plots
#'
#' This function generate a check layout in a retangular grid, The check is distributed by every # of plots
#'
#' @param n.row The row of a retangular grid
#' @param n.col The column of a retangular grid
#' @param by.plot The distance between checks (row-wise and col-wise) in the unit of plots
#' @return A dataframe will return
#' @import reshape2
#' @export
#' @examples
#' # To produce a 4 by 5 field with check distributed by every 2 plots
#' nr = 4; nc = 5
#' example.tmp = equal_check_by_plot(nr, nc, 2)
#' fields::quilt.plot(example.tmp, nx=nr, ny=nc)

equal_check_by_plot = function(n.row, n.col, by.plot){
  tmp.row = tmp.col = matrix(0, nrow=n.row, ncol=n.col)
  tmp.row[seq(from=1, to=nrow(tmp.row), by=by.plot), ] = 1
  tmp.col[, seq(from=1, to=ncol(tmp.col), by=by.plot)] = 1
  
  id.cell = melt(tmp.row * tmp.col)
  colnames(id.cell) = c('row.var', 'col.var', 'check')
  return(id.cell)
}


