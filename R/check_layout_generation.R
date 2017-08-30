#' This is a wrapper function of check layout generation
#'
#' The function takes grid input, and generates the corresponding check layout
#' @param n.row The row of the grid layout
#' @param n.col The column of the grid layout
#' @param by.density The density of the check in the layout. The parameter can be a vector
#' @param gen.module The pattern module of check layout, 'diagonal', 'random', 'equal_space'. The parameter can be a vector
#' @export
#' @examples
#' nr = nc = 5; den = c(0.1, 0.2); gen.module=c('diagonal', 'random')
#' check_layout_generation(nr, nc, den, gen.module)

check_layout_generation = function(n.row, n.col, by.density, gen.module) {
  tmp.1 = lapply(gen.module, function(x){
    tmp.2 = lapply(by.density, function(y){
      match.fun(
        paste0(x, '_check_by_density')
        ) (n.row, n.col, y)
})
    names(tmp.2) = by.density
    return(tmp.2)
})
  names(tmp.1) = gen.module
  return(tmp.1)
}
