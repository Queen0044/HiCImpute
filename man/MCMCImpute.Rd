% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCMCImpute.R
\name{MCMCImpute}
\alias{MCMCImpute}
\title{This is the flagship function of HiCImpute. It identifies structural zeros and imputes 
sampling zeros under a Bayesian framework. The outputs can be used to facilitate downstream 
analysis such as clustering, subtype discovery, or 3D structure construction.}
\usage{
MCMCImpute(
  scHiC,
  bulk = bulk,
  startval = c(100, 100, 10, 8, 10, 0.1, 900, 0.2, 0, replicate(dim(single)[2], 8)),
  n,
  epsilon1 = 0.5,
  epsilon2 = 5,
  mc.cores = 1,
  cutoff = 0.5,
  niter = 30000,
  burnin = 5000
)
}
\arguments{
\item{scHiC}{The single-cell Hi-C matrix. It can take three types of formats. The preferred format is a single-cell matrix with 
each column being a vector of the upper triangular matrix without including the diagonal entries 
of the 2D matrix of a single-cell. Another types of formats are a list with each element being a 
2D single-cell contact matrix, or a 3D (\eqn{n\times n\times k}) array that has k matrices of dimension 
\eqn{n\times n}. HiCImpute automatically transforms these two types of input into a matrix with each 
column being the vector of upper triangular matrix of a single-cell. For a single-cell matrix of 
size \eqn{n \times n}, the length of the vector should be \eqn{n\times(n-1)/2}. We only need the upper 
triangular matrix because the Hi-C matrix are symmetrical.}

\item{bulk}{The bulk data. It can take two types of formats. A 2D bulk matrix of dimension \eqn{n\times n} 
or a vector of the upper triangular entries of 2D bulk matrix. It can provide information for priors settings. 
If bulk data is not available, simply set it to be NULL, and MCMCImpute will sum up the single-cells to 
construct a bulk data.}

\item{startval}{The starting value for the vector of parameters 
\eqn{\Theta=(\alpha, \mu^\gamma, \beta, \mu, a, \delta, b, \pi, s, \mu_1, \cdots, \mu_K)}. 
See xie et al. for the details of these parameters. The default value is as set in the function.}

\item{n}{Integer. The dimension of single-cell matrix.}

\item{epsilon1}{The range size of  \eqn{\delta} that is used to monitor the prior mean of \eqn{\pi_{ij}}, 
the probability that the pair \eqn{(i,j)} do not interact. The default value of \eqn{\epsilon_1} is 0.5.}

\item{epsilon2}{The range size of \eqn{B} that is used to monitor the prior mean of \eqn{\mu_{ij}}, the intensity 
of interaction between pair \eqn{(i,j)}. The default value  of \eqn{\epsilon_2} is 5.}

\item{mc.cores}{The number of cores to be used in mclapply function that can parallelly impute the 
matrix. The default value is 1 (no parallelization), but the users is advised to a higher number to increase 
computational speed if their computer has parallel computing capability.}

\item{cutoff}{The threshold of \eqn{\pi_{ij}} that is used to define structural zeros. The default value is 0.5. 
That is, if the probability of being a SZ is greater than 0.5, then the pair \eqn{(i,j)} are labelled as not 
interacting due to underlying biological mechanism.}

\item{niter}{The number of iterations for the MCMC run. Default is 30000.}

\item{burnin}{The number of burn-in iteration. Default is 5000.}
}
\value{
A list of posterior mean of probability (SZ), the imputed data without defining SZ (Impute_All), 
and imputed data with SZ, using the threshold (Impute_SZ).
}
\description{
This is the flagship function of HiCImpute. It identifies structural zeros and imputes 
sampling zeros under a Bayesian framework. The outputs can be used to facilitate downstream 
analysis such as clustering, subtype discovery, or 3D structure construction.
}
\examples{
data("K562_T1_7k")
data("K562_bulk")
scHiC=K562_T1_7k
T1_7k_res=MCMCImpute(scHiC=K562_T1_7k,bulk=K562_bulk,
startval=c(100,100,10,8,10,0.1,900,0.2,0,replicate(dim(scHiC)[2],8)),n=61,mc.cores = 1,
cutoff=0.5, niter=100000,burnin=5000)
}
