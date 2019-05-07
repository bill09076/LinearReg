LinearReg <- function(x, y){
  cov = solve(t(x) %*% x)
  b <- cov %*% t(x) %*% y
  sigma_square = sum((y-x%*%b)^2)/(nrow(x)-ncol(x))
  cov = cov * sigma_square
  se_b = sqrt(diag(cov))
  t = b/se_b
  for(i in 1:ncol(x)){
    p[i,1] = 2*(1-pt(abs(t[i,1]),nrow(x)-ncol(x)))
  }
  index = 0:(ncol(x)-1)
  solution = data.frame(index, b, se_b, t, p)
  show(solution)
  return(solution)
}