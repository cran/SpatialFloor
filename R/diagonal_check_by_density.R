#' Generate a spatial layout of checks by diagonal design
#'
#' This funciton generates a layout grid with checks on diagonal
#' @param n.row The row of the retangular grid
#' @param n.col The column of the retangular grid
#' @param by.density The density of the check
#' @param spread.scale The spreading of the check across diagonal, related to the scale parameter in Cauchy distribution, default=50*max(n.row, n.col)
#' @export
#' @examples
#' #Creat a 30 by 30 layout with 0.2 density
#' tmp = diagonal_check_by_density(30, 30, 0.2)
#' fields::quilt.plot(tmp, nx=30, ny=30)

diagonal_check_by_density = function(n.row, n.col, by.density, spread.scale=50*max(n.row, n.col)){
	if(by.density*n.row*n.col<=min(n.row, n.col)){
		tmp.out.u=unsaturated.diag(n.row, n.col, by.density)
		return(tmp.out.u)
	} else {
		tmp.out.s=saturated.diag(n.row, n.col, by.density, spread.scale)
		return(tmp.out.s)
	}
	}