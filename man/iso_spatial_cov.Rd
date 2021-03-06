% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iso_spatial_cov.R
\name{iso_spatial_cov}
\alias{iso_spatial_cov}
\title{Isotropic spatial var-cov wrapper}
\usage{
iso_spatial_cov(D, cov.var = "exp", nugget = 0, sill, ranges)
}
\arguments{
\item{D}{The distance matrix}

\item{cov.var}{the var-cov function of producing var-cov matrix. 'exp' is exponential, 'gau' is Gaussian}

\item{nugget}{the nugget value of the var-cov function}

\item{sill}{the sill(variance, sigma^2) of the var-cov function}

\item{ranges}{the range value of var-cov function}
}
\value{
A matrix of var-cov matrix
}
\description{
This function load grid floor and generate the var-cov matrix
}
\examples{
D = fields::rdist(expand.grid(1:5, 1:5))
iso_spatial_cov(D, sill=3, ranges=1)
}
