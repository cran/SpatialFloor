#' This is a wrapper function of check layout generation
#'
#' The function takes grid input, and generates the corresponding check layout
#' @param n.row The row of the grid layout
#' @param n.col The column of the grid layout
#' @param by.density The density of the check in the layout. The parameter can be a vector
#' @param gen.module The pattern module of check layout, 'diagonal', 'diagSet', 'random', 'equal_space', 'p_rep'. The diagSet provides more homogeneized design than the diagonal at density from 0.01 to 0.35. The parameter can be a vector
#' @param p_rep_check The check density in the p_rep design, default 0.05
#' @export
#' @examples
#' nr = nc = 5; den = c(0.1, 0.2); gen.module=c('diagSet', 'random')
#' check_layout_generation(nr, nc, den, gen.module)

check_layout_generation = function(n.row, n.col, by.density, gen.module, p_rep_check=0.05) {
  tmp.1 = lapply(gen.module, function(x){
    tmp.2 = lapply(by.density, function(y){
	if(x=='p_rep'){
      match.fun(paste0(x, '_check_by_density')) (n.row, n.col, by.density=y, check.density=p_rep_check)
	} else {match.fun(paste0(x, '_check_by_density')) (n.row, n.col, by.density=y)}
})
    names(tmp.2) = by.density
    return(tmp.2)
})
  names(tmp.1) = gen.module
  return(tmp.1)
}
