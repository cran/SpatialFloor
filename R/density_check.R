#' Internal function for density check of check layout
#'
#' This is an inernal function
#' @param layout.matrix The layout matrix of designed grid
#' @param by.density The check density in the layout matrix
#' @import reshape2
#' @export

density_check = function(layout.matrix, by.density){
  dta.tmp = reshape2::melt(layout.matrix)
  colnames(dta.tmp) = c('row.var', 'col.var', 'check')
  res = sum(dta.tmp$check)/nrow(dta.tmp) - by.density

  if(res > 0 & abs(res*nrow(dta.tmp)) >= 1){
    idc = sample(rownames(dta.tmp[dta.tmp$check==1, ]), abs(res)*nrow(dta.tmp))
    dta.tmp[idc, ]$check = 0
    return(dta.tmp)
  } else if(res < 0 & abs(res*nrow(dta.tmp)) >= 1){
    idc = sample(rownames(dta.tmp[dta.tmp$check==0, ]), abs(res)*nrow(dta.tmp))
    dta.tmp[idc, ]$check = 1
	return(dta.tmp)
  } else {
    return(dta.tmp)
  }
}
