data {
  
  // number of constructors in time series
  int<lower=1> K;
  
  // number of drivers in time series
  int<lower=2> N;
  
  // number of qualifiers/races
  int<lower=1> T;
  
  // driver qualifier/race NA indicators
  matrix[N,T] I_1;
  
  // constructor indicators
  matrix[N,K] I_2[T];
  
  // constructor qualifier/race NA indicators
  matrix[K,T] I_3;
  
  // number of ranks per qualifier/race
  int<lower=2> J;
  
  // fixed initial conditions for latent constructor ability state equation
  vector[K] mu_C_0;
  
  // fixed initial conditions for latent driver ability state equation
  vector[N] mu_D_0;
  
  // fixed cut points
  real gamma_lower;
  real gamma_upper;
  
  // simulated/actual qualifier/race ranks
  array[N,T] int<lower=1,upper=J> R;
  
}


parameters {
  
  // SD for error for latent constructor ability state equation
  real<lower=0> varsigma_C;
  
  // SD for error for latent driver ability state equation
  real<lower=0> varsigma_D;
  
  // latent constructor abilities ( mu )
  matrix[K,T] mu_C_temp;
  
  // latent driver abilities ( mu )
  matrix[N,T] mu_D_temp;
  
  // cut points
  simplex[J-3+1] gamma_temp;
  
}


transformed parameters {
  
  // latent constructor abilities ( mu )
  matrix[K,T] mu_C;
  
  for (t in 1:T) {
    
    if (t == 1) {
      
      for (k in 1:K) {
        
        if (I_3[k,t] == 1) { mu_C[k,t] = mu_C_temp[k,t]; }
        
        else { mu_C[k,t] = mu_C_0[k]; }
        
      }
      
    }
    
    if (t > 1) {
      
      for (k in 1:K) {
        
        if (I_3[k,t] == 1) { mu_C[k,t] = mu_C_temp[k,t]; }
        
        else { mu_C[k,t] = mu_C[k,t-1]; }
        
      }
      
    }
    
  }
  
  // latent driver abilities ( mu )
  matrix[N,T] mu_D;
  
  for (t in 1:T) {
    
    if (t == 1) {
      
      for (n in 1:N) {
        
        if (I_1[n,t] == 1) { mu_D[n,t] = mu_D_temp[n,t]; }
        
        else { mu_D[n,t] = mu_D_0[n]; }
        
      }
      
    }
    
    if (t > 1) {
      
      for (n in 1:N) {
        
        if (I_1[n,t] == 1) { mu_D[n,t] = mu_D_temp[n,t]; }
        
        else { mu_D[n,t] = mu_D[n,t-1]; }
        
      }
      
    }
    
  }
  
  // latent qualifier/race performance equation ( mu )
  matrix[N,T] mu_P;
  
  for (t in 1:T) {
    
    for (n in 1:N) {
      
      mu_P[n,t] = mu_D[n,t] + dot_product(I_2[t,n], col(mu_C,t));
    
    }
  
  }
  
  // cut points
  ordered[J-1] gamma;
  
  for (j in 2:(J-2)) {
    gamma[j] = head(cumulative_sum(gamma_temp), (J-3))[j-1] * gamma_upper;
  }
  
  gamma[1] = gamma_lower;
  gamma[J-1] = gamma_upper;
  
}


model {
  
  // prior equation for varsigma_C
  varsigma_C ~ normal(0,1);
  
  // prior equation for varsigma_D
  varsigma_D ~ normal(0,1);
  
  // SD for latent qualifier/race performance equation
  real sigma_P;
  sigma_P = sqrt(square(varsigma_D) + square(varsigma_C));
  
  // initialization for choice probabilities
  vector[J] theta;
  
  for (t in 1:T) {
    
    if (t == 1) {
      
      // latent constructor ability state equation ( mu )
      for (k in 1:K) {
        
        if (I_3[k,t] == 1) { mu_C[k,t] ~ normal(mu_C_0[k], varsigma_C); }
        
      }
    
      // latent driver ability state equation ( mu )
      for (n in 1:N) {
        
        if (I_1[n,t] == 1) { mu_D[n,t] ~ normal(mu_D_0[n], varsigma_D); }
        
      }
      
    }
    
    if (t > 1) {
      
      // latent constructor ability state equation ( mu )
      for (k in 1:K) {
        
        if (I_3[k,t] == 1) { mu_C[k,t] ~ normal(mu_C[k,t-1], varsigma_C); }
        
      }
    
      // latent driver ability state equation ( mu )
      for (n in 1:N) {
        
        if (I_1[n,t] == 1) { mu_D[n,t] ~ normal(mu_D[n,t-1], varsigma_D); }
        
      }
      
    }
    
    for (n in 1:N) {
      
      if(I_1[n,t] == 1) {
        
        // choice probability equations
        theta[J] = Phi((gamma[1] - mu_P[n,t]) / sigma_P);
      
        for (j in 1:(J-2)) {
          theta[J-j] = Phi((gamma[j+1] - mu_P[n,t]) / sigma_P) - Phi((gamma[j] - mu_P[n,t]) / sigma_P);
        }
      
        theta[1] = 1 - Phi((gamma[J-1] - mu_P[n,t]) / sigma_P);
      
        // qualifier/race ranking equation
        R[n,t] ~ categorical(theta);
        
      }
        
    }
    
  }
    
}


generated quantities {
  
  // predicted ranks
  array[N,T] int<lower=1,upper=J> R_pred;
  
  for (t in 1:T) {
    
    // SD for latent qualifier/race performance equation
    real sigma_P;
    sigma_P = sqrt(square(varsigma_D) + square(varsigma_C));
  
    // initialization for choice probabilities
    vector[J] theta;
    
    for (n in 1:N) {
      
      // choice probability equations
      theta[J] = Phi((gamma[1] - mu_P[n,t]) / sigma_P);
      
      for (j in 1:(J-2)) {
        theta[J-j] = Phi((gamma[j+1] - mu_P[n,t]) / sigma_P) - Phi((gamma[j] - mu_P[n,t]) / sigma_P);
      }
      
      theta[1] = 1 - Phi((gamma[J-1] - mu_P[n,t]) / sigma_P);
      
      // qualifier/race ranking equation
      R_pred[n,t] = categorical_rng(theta);
    
    }
    
  }
  
}


