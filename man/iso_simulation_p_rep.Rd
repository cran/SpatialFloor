% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iso_simulation_p_rep.R
\name{iso_simulation_p_rep}
\alias{iso_simulation_p_rep}
\title{A simulation fucntion for isotrophic spatial data wrt/ p-rep data}
\usage{
iso_simulation_p_rep(n.row, n.col, lon.lat = FALSE, mile = FALSE,
  density.choice, density.layout = c("diagSet", "random", "equal_space"), h2,
  sigma_env, sigma_variety = (sigma_env * h2)/(1 - h2), mu_variety,
  cov_fun = "exp", ranges = sqrt(2), simulation = 3, nugget = 0,
  mu_floor = 0)
}
\arguments{
\item{n.row}{(Mandatory) The row of the field grid}

\item{n.col}{(Mandatory) The column of the fields grid}

\item{lon.lat}{(Optional) The distance calculation is based on earth distance (if TRUE), default is FALSE}

\item{mile}{(Optional) The distance calculation is based on milage (if TRUE), default is FALSE}

\item{density.choice}{(Mandatory) The density of the replicated entries (range from 0 to 1, if 'diagonal'; from 0 to 0.34, if 'diagSet'), EVEN number ONLY}

\item{density.layout}{(Optional) The layout pattern of density, default are: 'diagonal', 'diagSet', 'random', 'equal_space'. The diagSet is recommended over the diagonal at density from 0.01 to 0.35.}

\item{h2}{(Mandatory) The heritability of the simulated data, h2 = sigma_variety / (sigma_variety + sigma_env)}

\item{sigma_env}{(Mandatory) The variance of environment, (equilievent of sill)}

\item{sigma_variety}{(Optional) The variance of entry, default sigma_variety = (sigma_env * h2) / (1 - h2)}

\item{mu_variety}{(Mandatory) The mean of the variety}

\item{cov_fun}{(Optional) The var-cov function ('exp' or 'gau'), default is 'exp', exponential}

\item{ranges}{(Optional) The range parameter of spatial distribution, default sqrt(2)}

\item{simulation}{(Optional) The number of simulated data returned, default 3}

\item{nugget}{(Optional) The nugget parameter in spatial simulation, default 0}

\item{mu_floor}{(Optional) The mean of the spatial floor, default 0}
}
\description{
The function simulates spatial data by given parameters
}
\examples{
temp.1 = iso_simulation_p_rep(10, 10, density.choice=c(0.04,0.2)
			, h2=0.5, sigma_env=100, mu_variety=300)
temp.2 = iso_simulation_p_rep(10, 10, density.choice=c(0.04,0.2)
			, h2=0.5, sigma_env=100, mu_variety=300)
}
