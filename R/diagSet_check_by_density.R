#' Generate a spatial layout of checks by diagonal design (settle for density = .1 - .35
#'
#' This function generates a layout grid with checks on diagonal (pre-designed)
#' @param n.row The row of the retangular grid
#' @param n.col The column of the retangular grid
#' @param by.density The density of the checks, range from 0.01 to 0.35
#' @export
#' @examples
#' nr=10; nc=20; den=0.2
#' diagSet_check_by_density(n.row=nr, n.col=nc, by.density=den)

diagSet_check_by_density=
function(n.row, n.col, by.density){
	floor.M=diag_extract()[[100*by.density]]
	row.time=n.row/nrow(floor.M)
	col.time=n.col/ncol(floor.M)
	
	col.layout=if(col.time<1){floor.M[, 1:n.col]
				} else {
					indc.1=floor(col.time); indc.2=col.time%%1
					if(indc.2==0){
						do.call(cbind, replicate(indc.1, floor.M, simplify=F))
					} else {
						cbind(do.call(cbind, replicate(indc.1, floor.M, simplify=F))
							, floor.M[, 1:(indc.2*10)])
					}
				}
	
	row.layout=if(row.time<1){col.layout[1:n.row, ]
				} else {
					indr.1=floor(row.time); indr.2=row.time%%1
					if(indr.2==0){
						do.call(rbind, replicate(indr.1, col.layout, simplify=F))
					} else {
						rbind(do.call(rbind, replicate(indr.1, col.layout, simplify=F))
							, col.layout[1:(indr.2*10), ])
					}
				}
	out=density_check(row.layout, by.density)
	return(out)
}