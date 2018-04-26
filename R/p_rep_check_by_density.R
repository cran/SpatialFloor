#' Generate a spatial layout of checks by density: p rep design with checks
#'
#' This function generate a check layout in a retangular grid. the check and partial replicated entries are spread by blocksdesign library
#'
#' @param n.row The row of a retangular grid
#' @param n.col The column of a retangular grid
#' @param check.density The density of the check
#' @param by.density The total density of check and p_rep, MUST be a even decimal number after substract the check.density
#' @return A dataframe will return: row.var=row, col.var=col, check=0, single rep; 1, check; else, partial replicated entries
#' @import blocksdesign reshape2
#' @export
#' @examples
#' # To produce a 20 by 20 field with check distributed by density=0.35
#' nr = 20; nc = 20
#' example.tmp = p_rep_check_by_density(nr, nc, by.density=.35)
#' fields::quilt.plot(example.tmp, nx=nr, ny=nc)

p_rep_check_by_density = function(n.row, n.col, check.density=0.05, by.density){
	tot.=n.row*n.col
	
	if((tot.*check.density)%%2 == 1){check.den=check.density+0.01} else {check.den=check.density}
	rep.density=by.density-check.density
	check.rep=round(check.den*tot.)
	sig.=tot.*(1-check.den-rep.density)
	
	t1=as.matrix(blocksdesign::blocks(treatments=c(1, round((tot.-check.rep)/2)), replicates=c(check.rep, 2), blocks=n.row)$Plan[,-c(1:2)])
	colnames(t1)=1:n.col
	t2=reshape2::melt(t1); t2$value=as.numeric(as.character(t2$value))
	colnames(t2)=c('row.var', 'col.var', 'check')
	
	rep.=(rep.density*tot.)/2
	t2$check[t2$check>=(2+rep.)]=0
	t3=as.matrix(reshape2::dcast(t2, row.var~col.var, value.var='check')[,-1])
	out=density_check(t3, check.density)
	return(out)
}