

mattovec <- function(mat){
  vec <- mat[upper.tri(mat, diag = FALSE)]
  return(vec)
}