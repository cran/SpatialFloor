#' A simulation fucntion for isotrophic spatial data
#'
#' The function simulates spatial data by given parameters
#' @param n.row (Mandatory) The row of the field grid
#' @param n.col (Mandatory) The column of the fields grid
#' @param lon.lat (Optional) The distance calculation is based on earth distance (if TRUE), default is FALSE
#' @param mile (Optional) The distance calculation is based on milage (if TRUE), default is FALSE
#' @param density.choice (Mandatory) The density of the checks, a vector (range from 0 to 1)
#' @param density.layout (Optional) The layout pattern of density, default are: 'diagonal', 'random', 'equal_space'
#' @param h2 (Mandatory) The heritability of the simulated data, h2 = sigma_variety / (sigma_variety + sigma_env)
#' @param sigma_env (Mandatory) The variance of environment, (equilievent of sill)
#' @param sigma_variety (Optional) The variance of entry, default sigma_variety = (sigma_env * h2) / (1 - h2)
#' @param mu_variety (Mandatory) The mean of the variety
#' @param mu_check (Optional) The value of check, default mu_check = 1.68 * sqrt(sigma_variety)
#' @param cov_fun (Optional) The var-cov function ('exp' or 'gau'), default is 'exp', exponential
#' @param ranges (Optional) The range parameter of spatial distribution, default sqrt(2)
#' @param simulation (Optional) The number of simulated data returned, default 3
#' @param fixed.pattern (Optional) If TRUE, the check pattern layout is the same within each density. Default TRUE
#' @param nugget (Optional) The nugget parameter in spatial simulation, default 0
#' @param mu_floor (Optional) The mean of the spatial floor, default 0
#' @import stats
#' @export
#' @examples
#' temp.1 = iso_simulation_by_heriability(10, 10, density.choice=c(0.05,0.1)
#'				, h2=0.5, sigma_env=100, mu_variety=300, fixed.pattern=TRUE)
#' temp.2 = iso_simulation_by_heriability(10, 10, density.choice=c(0.05,0.1)
#'				, h2=0.5, sigma_env=100, mu_variety=300, fixed.pattern=FALSE)

iso_simulation_by_heriability = function(n.row, n.col, lon.lat=FALSE, mile=FALSE, density.choice, density.layout=c('diagonal', 'random', 'equal_space'), h2, sigma_env, sigma_variety=(sigma_env*h2)/(1-h2), mu_variety, mu_check=mu_variety+1.68*sqrt(sigma_variety), cov_fun='exp', ranges=sqrt(2), simulation=3, fixed.pattern=TRUE, nugget=0, mu_floor=0) {
  if(fixed.pattern==TRUE){
		tmp.1 = check_layout_generation(n.row, n.col, density.choice, density.layout)
		D.tmp.fix = spatial_dist(tmp.1[[1]][[1]][,1:2], lon.lat=lon.lat, mile=mile)
		dta.out.fix = lapply(density.layout, function(x){
					tmp.den.fix = lapply(as.character(density.choice), function(y){
						tmp.sim.fix = lapply(1:simulation, function(z){
									tmp.2 = tmp.1[[x]][[y]]
									tmp.2$env.error = iso_spatial_floor(D.tmp.fix, cov.var=cov_fun, nugget=nugget, sill=sigma_env, ranges=ranges, mu=mu_floor)
									tmp.2$variety = tmp.2$true.val = tmp.2$check
									idc = sum(tmp.2$check==0)
							
									tmp.2[tmp.2$check==1,]$true.val = mu_check
									tmp.2[tmp.2$check==0,]$true.val = rnorm(idc, mu_variety, sqrt(sigma_variety))
							
									tmp.2[tmp.2$check==1,]$variety='check'
									tmp.2[tmp.2$check==0,]$variety=paste0('variety.', seq_len(idc))
							
									tmp.2$response = tmp.2$env.error + tmp.2$true.val
									return(tmp.2)
									})
						names(tmp.sim.fix) = 1:simulation; return(tmp.sim.fix)
					})
					names(tmp.den.fix) = density.choice; return(tmp.den.fix)
				})
		names(dta.out.fix) = density.layout; return(dta.out.fix)
	} else if(fixed.pattern==FALSE){
		dta.out = lapply(density.layout, function(i){
					tmp.den = lapply(density.choice, function(j){
						tmp.sim = lapply(1:simulation, function(k){
									tmp.3 = check_layout_generation(n.row, n.col, j, i)[[i]][[as.character(j)]]
									D.tmp = spatial_dist(tmp.3[,1:2], lon.lat=lon.lat, mile=mile)
									tmp.3$env.error = iso_spatial_floor(D.tmp, cov.var=cov_fun, nugget=nugget, sill=sigma_env, ranges=ranges, mu=mu_floor)
									tmp.3$variety = tmp.3$true.val = tmp.3$check
									idc = sum(tmp.3$check==0)
							
									tmp.3[tmp.3$check==1,]$true.val = mu_check
									tmp.3[tmp.3$check==0,]$true.val = rnorm(idc, mu_variety, sqrt(sigma_variety))
							
									tmp.3[tmp.3$check==1,]$variety='check'
									tmp.3[tmp.3$check==0,]$variety=paste0('variety.', seq_len(idc))
							
									tmp.3$response = tmp.3$env.error + tmp.3$true.val
									return(tmp.3)
									})
						names(tmp.sim) = 1:simulation; return(tmp.sim)
					})
					names(tmp.den) = density.choice; return(tmp.den)
				})
		names(dta.out) = density.layout; return(dta.out)
	}
	}
  


