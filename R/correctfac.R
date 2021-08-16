
correctfac <- function(type_bulk,nei){
  n = dim(type_bulk)[1]
  mean_bulk = sum(type_bulk[upper.tri(type_bulk)])/sum(upper.tri(type_bulk))
  correct = matrix(rep(0,n*n),nrow=n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      m <- type_bulk[max(1,i-nei):min(n,i+nei), max(1,j-nei):min(n,j+nei)]
      c <- mean(m[!is.na(m)])
      correct[i,j] = c
    }
  }
  return(correct)
}