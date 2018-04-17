generate_data = function(n, p) {
  m = matrix(vector (length = n * p), nrow = n, col = p)
  for (i in 1:n) {
    m[i, ] = rnorm(p, mean = 0, sd = 1)
  }
  responses = rnorm(n, mean = 0, sd = 1)
  result = list(m, responses)
  return(result)
}

model_select = function(covariates, responses, cutoff) {
  init_model = lm(responses ~ covariates) 
  reduced = summary(init_model)$coefficients[, 4] <= cutoff
  reduced_model = lm(responses ~ covariates[, reduced])
  if (nrow(reduced_cov) == 0) {
    return(v)
  }
  return(summary(reduced_model)$coefficients[, 4])
}

