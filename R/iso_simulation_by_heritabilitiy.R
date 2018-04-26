#' A simulation fucntion for isotrophic spatial data
#'
#' The function simulates spatial data by given parameters
#' @param n.row (Mandatory) The row of the field grid
#' @param n.col (Mandatory) The column of the fields grid
#' @param lon.lat (Optional) The distance calculation is based on earth distance (if TRUE), default is FALSE
#' @param mile (Optional) The distance calculation is based on milage (if TRUE), default is FALSE
#' @param density.choice (Mandatory) The density of the checks, a vector (range from 0 to 1)
#' @param density.layout (Optional) The layout pattern of density, default are: 'diagonal', 'diagSet', 'random', 'equal_space', 'p_rep'. The diagSet is recommended over the diagonal at density from 0.01 to 0.35. 
#' @param h2 (Mandatory) The heritability of the simulated data, h2 = sigma_variety / (sigma_variety + sigma_env)
#' @param sigma_env (Mandatory) The variance of environment, (equilievent of sill)
#' @param sigma_variety (Optional) The variance of entry, default sigma_variety = (sigma_env * h2) / (1 - h2)
#' @param mu_variety (Mandatory) The mean of the variety
#' @param mu_check (Optional) The mean value of checks, default mu_check = 1.68 * sqrt(sigma_variety)
#' @param cov_fun (Optional) The var-cov function ('exp' or 'gau'), default is 'exp', exponential
#' @param ranges (Optional) The range parameter of spatial distribution, default sqrt(2)
#' @param simulation (Optional) The number of simulated data returned, default 3
#' @param nugget (Optional) The nugget parameter in spatial simulation, default 0
#' @param mu_floor (Optional) The mean of the spatial floor, default 0
#' @param p_rep_check The density of checks in the p rep design, default 0.05
#' @import stats taRifx
#' @export
#' @examples
#' temp.1 = iso_simulation_by_heriability(20, 20, density.choice=c(0.05)
#'				, density.layout=c('diagSet', 'random')
#'				, h2=0.5, sigma_env=100, mu_variety=300)
#' temp.2 = iso_simulation_by_heriability(20, 20, density.choice=c(0.05)
#'				, density.layout=c('diagSet', 'random')
#'				, h2=0.5, sigma_env=100, mu_variety=300)

iso_simulation_by_heriability = function(n.row, n.col, lon.lat=FALSE, mile=FALSE, density.choice, density.layout=c('diagSet', 'random', 'equal_space', 'p_rep'), h2, sigma_env, sigma_variety=(sigma_env*h2)/(1-h2), mu_variety, mu_check=mu_variety+1.68*sqrt(sigma_variety), cov_fun='exp', ranges=sqrt(2), simulation=3, nugget=0, mu_floor=0, p_rep_check=0.05) {
		density.choice = sort(round(density.choice,2))
		tmp.1 = check_layout_generation(n.row, n.col, density.choice, density.layout, p_rep_check=p_rep_check)
		D.tmp.fix = spatial_dist(tmp.1[[1]][[1]][,1:2], lon.lat=lon.lat, mile=mile)
		check=NULL
		dta.out.tmp = do.call(rbind,
						lapply(1:simulation, function(x){
							true.pool=rnorm(n.row*n.col, mu_variety, sqrt(sigma_variety))
							env.error=iso_spatial_floor(D.tmp.fix, cov.var=cov_fun, nugget=nugget, sill=sigma_env, ranges=ranges, mu=mu_floor)
							do.call(rbind,
								lapply(1:length(density.choice), function(y){
									den.=density.choice[y]
									general.design=sample(true.pool, round(n.row*n.col*(1-den.)))
									p_rep.design=sample(true.pool, round(n.row*n.col*(1-(den.-p_rep_check)/2-p_rep_check)))
									do.call(rbind,
										lapply(density.layout, function(z){
										print(paste(x,y,z))

										den.ind=as.character(den.)
										tmp.2=tmp.1[[z]][[den.ind]]
										if(z=='p_rep'){
											tmp.2$env.error=env.error; tmp.2$true.value=NA; tmp.2$entry='check'; tmp.2$p_rep=NA
										
											p_rep.dta=taRifx::sort.data.frame(droplevels(subset(tmp.2, !check %in% c(0, 1))), f=~check)
											p.true=sample(1:length(p_rep.design), round(n.row*n.col*(den.-p_rep_check)/2))
											p_rep.dta$true.value=rep(p_rep.design[p.true], each=2)
											if(length(p.true)!=0){
												p_rep.dta$entry=paste0('entry.d', p_rep.dta$check-1)
												p_rep.dta$p_rep=T
											}
										
											sig.dta=droplevels(subset(tmp.2, check %in% 0))
											if(length(p.true)==0){sig.dta$true.value=p_rep.design}else{sig.dta$true.value=p_rep.design[-p.true]} 
											sig.dta$entry=paste0('entry.', 1:nrow(sig.dta))
											sig.dta$p_rep=F
											
											check.dta=droplevels(subset(tmp.2, check %in% 1))
											check.dta$true.value=mu_check; check.dta$p_rep=F
											
											out=data.frame(simulation=x, layout.=z, tot.den=den., den.check=p_rep_check, den.p_rep=den.-p_rep_check, taRifx::sort.data.frame(rbind(p_rep.dta, sig.dta, check.dta), f=~row.var+col.var))
											out$response=with(out, true.value+env.error); return(out)
										
										} else {
											tmp.2$env.error=env.error
											check.dta=droplevels(subset(tmp.2, check %in% 1))
											check.dta$true.value=mu_check; check.dta$entry='check'; check.dta$p_rep=F
											
											sig.dta=droplevels(subset(tmp.2, check %in% 0))
											sig.dta$true.value=general.design
											sig.dta$entry=paste0('entry.', 1:nrow(sig.dta)); sig.dta$p_rep=F
										
											out=data.frame(simulation=x, layout.=z, tot.den=den., den.check=den., den.p_rep=0, taRifx::sort.data.frame(rbind(check.dta, sig.dta), f=~row.var+col.var))
											out$response=with(out, true.value+env.error); return(out)
										}
									})
								)
							})
						)
					})
				)
		out=data.frame(grid.=paste0(n.row,'*',n.col), h2=h2, ranges=ranges, dta.out.tmp)
	}
	
  


