# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

oneimpute <- function(niter, burnin, n_single, m, lambda, mu_sigma_vec, startval, n, epsilon1, epsilon2, ii) {
    .Call('_HiCImpute_oneimpute', PACKAGE = 'HiCImpute', niter, burnin, n_single, m, lambda, mu_sigma_vec, startval, n, epsilon1, epsilon2, ii)
}

