generate_data = function(n, p) {
  m = matrix(vector (length = n * p), nrow = n, col = p)
  for (i in 1:n) {
    m[i, ] = rnorm(p, mean = 0, sd = 1)
  }
  responses = rnorm(n, mean = 0, sd = 1)
  result = list(m, responses)
  return(result)
}