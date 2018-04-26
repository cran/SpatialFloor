#' A simulation fucntion for isotrophic spatial data wrt/ p-rep data
#'
#' The function simulates spatial data by given parameters
#' @param n.row (Mandatory) The row of the field grid
#' @param n.col (Mandatory) The column of the fields grid
#' @param lon.lat (Optional) The distance calculation is based on earth distance (if TRUE), default is FALSE
#' @param mile (Optional) The distance calculation is based on milage (if TRUE), default is FALSE
#' @param density.choice (Mandatory) The density of the replicated entries (range from 0 to 1, if 'diagonal'; from 0 to 0.34, if 'diagSet'), EVEN number ONLY
#' @param density.layout (Optional) The layout pattern of density, default are: 'diagonal', 'diagSet', 'random', 'equal_space'. The diagSet is recommended over the diagonal at density from 0.01 to 0.35. 
#' @param h2 (Mandatory) The heritability of the simulated data, h2 = sigma_variety / (sigma_variety + sigma_env)
#' @param sigma_env (Mandatory) The variance of environment, (equilievent of sill)
#' @param sigma_variety (Optional) The variance of entry, default sigma_variety = (sigma_env * h2) / (1 - h2)
#' @param mu_variety (Mandatory) The mean of the variety
#' @param cov_fun (Optional) The var-cov function ('exp' or 'gau'), default is 'exp', exponential
#' @param ranges (Optional) The range parameter of spatial distribution, default sqrt(2)
#' @param simulation (Optional) The number of simulated data returned, default 3
#' @param nugget (Optional) The nugget parameter in spatial simulation, default 0
#' @param mu_floor (Optional) The mean of the spatial floor, default 0
#' @import stats
#' @export
#' @examples
#' temp.1 = iso_simulation_p_rep(10, 10, density.choice=c(0.04,0.2)
#'				, h2=0.5, sigma_env=100, mu_variety=300)
#' temp.2 = iso_simulation_p_rep(10, 10, density.choice=c(0.04,0.2)
#'				, h2=0.5, sigma_env=100, mu_variety=300)

iso_simulation_p_rep = function(n.row, n.col, lon.lat=FALSE, mile=FALSE, density.choice, density.layout=c('diagSet', 'random', 'equal_space'), h2, sigma_env, sigma_variety=(sigma_env*h2)/(1-h2), mu_variety, cov_fun='exp', ranges=sqrt(2), simulation=3, nugget=0, mu_floor=0) {
		density.choice = round(density.choice,2)
		tmp.1 = check_layout_generation(n.row, n.col, density.choice, density.layout)
		D.tmp.fix = spatial_dist(tmp.1[[1]][[1]][,1:2], lon.lat=lon.lat, mile=mile)
		env.error = iso_spatial_floor(D.tmp.fix, cov.var=cov_fun, nugget=nugget, sill=sigma_env, ranges=ranges, mu=mu_floor)
		true.all = rnorm(n.row*n.col, mu_variety, sqrt(sigma_variety))
		
		dta.out.fix = lapply(density.layout, function(x){
					tmp.den.fix = lapply(as.character(density.choice), function(y){
						tmp.sim.fix = lapply(1:simulation, function(z){
									y=as.numeric(y)
									entry.tot = round((y/2*n.row*n.col) + round((1-y)*n.row*n.col))
									true.pool = sample(true.all, entry.tot)
									dup.ind = sample(1:length(true.pool), round((y/2*n.row*n.col)))
									
									dup.true = data.frame(true.val=rep(true.pool[dup.ind], each=2), rep.=rep(1:2, length(dup.ind)), variety=rep(paste0('variety.d',seq_len(length(dup.ind))), each=2), p_rep='T')
									sig.true = data.frame(true.val=true.pool[-dup.ind], rep.=1, variety=paste0('variety.', seq_len(round((1-y)*n.row*n.col))), p_rep='F')
						
									tmp.2 = tmp.1[[x]][[as.character(y)]]
									tmp.2$env.error = env.error

									tmp.3 = rbind(
												cbind(tmp.2[tmp.2$check==1,], dup.true)
												, cbind(tmp.2[tmp.2$check==0,], sig.true)
												)
									tmp.3$response = with(tmp.3, env.error+true.val)
									return(tmp.3)
									})
						names(tmp.sim.fix) = 1:simulation; return(tmp.sim.fix)
					})
					names(tmp.den.fix) = density.choice; return(tmp.den.fix)
				})
		names(dta.out.fix) = density.layout; 
		return(dta.out.fix)
	}
  


