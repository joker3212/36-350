generate_data = function(n, p) {
  m = matrix(vector (length = n * p), nrow = n, ncol = p)
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
  if (sum(reduced) == 0) {
    return(v)
  }
  return(summary(reduced_model)$coefficients[, 4])
}

run_simulation = function(n_trials, n, p, cutoff) {
  p_values = c()
  for (trial in 1:n_trials) {
    result_i = generate_data(n, p)
    p_values_i = model_select(result_i[[1]], result_i[[2]], cutoff)
    p_values = c(p_values, p_values_i)
  }
  hist(p_values, xlab = "P Values")
}

make_plot(datapath) {
  load(datapath)
  hist(p_values)
}



