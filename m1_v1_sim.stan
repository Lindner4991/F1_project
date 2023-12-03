data {
  
  // number of constructors in time series
  int<lower=1> K;
  
  // number of drivers in time series
  int<lower=2> N;
  
  // number of qualifiers/races
  int<lower=1> T;
  
  // constructor race/qualifier NA indicators
  matrix[K,T] I_3;
  
  // constructor indicators
  matrix[N,K] I_2[T];
  
  // driver race/qualifier NA indicators
  matrix[N,T] I_1;
  
  // number of ranks per qualifier/race
  int<lower=2> J;
  
  // initial conditions for latent constructor ability state equations
  vector[K] c_0;
  
  // initial conditions for latent driver ability state equations
  vector[N] d_0;
  
  // SD for error for latent constructor ability state equations
  real<lower=0> sigma_C;
  
  // SD for error for latent driver ability state equations
  real<lower=0> sigma_D;
  
  // cut points
  ordered[J-1] gamma;
  
}


parameters {}


model {}
  

generated quantities {
  
  // simulated qualifier/race ranks
  array[N,T] int<lower=1,upper=J> R_sim;
  
  // latent constructor abilities ( mu )
  matrix[K,T] mu_C;
  
  // latent driver abilities ( mu )
  matrix[N,T] mu_D;
  
  // latent qualifier/race performance ( mu )
  matrix[N,T] mu_P;
  
  for (t in 1:T) {
    
    if (t == 1) {
      
      // latent constructor ability state equation ( mu )
      for (k in 1:K) { mu_C[k,t] = normal_rng(c_0[k], sigma_C * I_3[k,t]); }
    
      // latent driver ability state equation ( mu )
      for (n in 1:N) { mu_D[n,t] = normal_rng(d_0[n], sigma_D * I_1[n,t]); }
      
    }
    
    if (t > 1) {
      
      // latent constructor ability state equation ( mu )
      for (k in 1:K) { mu_C[k,t] = normal_rng(mu_C[k,t-1], sigma_C); }
    
      // latent driver ability state equation ( mu )
      for (n in 1:N) { mu_D[n,t] = normal_rng(mu_D[n,t-1], sigma_D); }
      
    }
    
    // SD for latent qualifier/race performance
    real sigma_P;
    sigma_P = sqrt(square(sigma_D) + square(sigma_C));
  
    // initialization for choice probabilities
    vector[J] theta;
    
    for (n in 1:N) {
    
      // latent qualifier/race performance equation ( mu )
      mu_P[n,t] = mu_D[n,t] + dot_product(I_2[t,n], col(mu_C,t));
    
      // choice probability equations
      theta[J] = Phi((gamma[1] - mu_P[n,t]) / sigma_P);
      
      for (j in 1:(J-2)) {
        theta[J-j] = Phi((gamma[j+1] - mu_P[n,t]) / sigma_P) - Phi((gamma[j] - mu_P[n,t]) / sigma_P);
      }
      
      theta[1] = 1 - Phi((gamma[J-1] - mu_P[n,t]) / sigma_P);
      
      // qualifier/race ranking equation
      R_sim[n,t] = categorical_rng(theta);
    
    }
    
  }
  
}


