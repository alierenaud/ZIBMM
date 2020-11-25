data {
  int N;
  int d; // nb fixed effect
  int k; // nb random effects
  matrix[N, d] x_woint;   // no intertcept predictor matrix (fixed effect)
  matrix[N, k] z;   // predictor matrix (random effect)
  vector<lower=0, upper=1>[N] y;
}
transformed data {
  int<lower=0, upper=1> is_nonzero[N];
  // create indicator for whether y is discrete 
  // and an integer value to pass to bernoulli_lpmf for discrete y
  for (i in 1:N) {
    if (y[i] == 0) {
      is_nonzero[i] = 0;
    } else {
      is_nonzero[i] = 1;
    } 
  }
}
parameters {
  vector[d] coef_a; // fixed effects parameters a
  vector[d] coef_m; // fixed effects parameters m
  vector[d] coef_p; // fixed effects parameters p
  
  real<lower=0> eta1; // random effects variance 1
  real m0; // random effects centering 1
  vector[k] coef_b1; // random effects parameters b1
  
  real<lower=0> eta2; // random effects variance 2
  real a0; // random effects centering 2
  vector[k] coef_b2; // random effects parameters b2
  
  real<lower=0> eta3; // random effects variance 3
  real p0; // random effects centering 3
  vector[k] coef_b3; // random effects parameters b3
}
transformed parameters {
  vector<lower=0, upper=1>[N] alpha;
  vector[N] mu;
  vector<lower=0>[N] phi;
  vector<lower=0>[N] p;
  vector<lower=0>[N] q;

  for (i in 1:N) {
    alpha[i] = inv_logit(x_woint[i,] * coef_a + z[i,] * coef_b2);
    mu[i] = inv_logit(x_woint[i,] * coef_m + z[i,] * coef_b1);
    phi[i] = exp(x_woint[i,] * coef_p + z[i,] * coef_b3);
    p[i] = mu[i] * phi[i];
    q[i] = phi[i] - mu[i] * phi[i];
  }
}
model {
  a0 ~ normal(0, 100);
  coef_a ~ normal(0, 100);
  m0 ~ normal(0, 100);
  coef_m ~ normal(0, 100);
  p0 ~ normal(0, 100);
  coef_p ~ normal(0, 100);
  
  coef_b1 ~ normal(m0, sqrt(eta1));
  eta1 ~ inv_gamma(0.01, 0.01);
  
  coef_b2 ~ normal(a0, sqrt(eta2));
  eta2 ~ inv_gamma(0.01, 0.01);
  
  coef_b3 ~ normal(p0, sqrt(eta3));
  eta3 ~ inv_gamma(0.01, 0.01);
  
  
  is_nonzero ~ bernoulli(alpha); 
  for (i in 1:N) {
    if (is_nonzero[i] == 1) {
      y[i] ~ beta(p[i], q[i]);
    } 
  }
    
  
}
