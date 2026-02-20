#include <TMB.hpp>

/* ---------- Inverse-Gaussian log-pdf  --------------------
   IG(a, b):  log f(x;a,b) = (1/2)log(b) - (1/2)log(2π) - (3/2)log(x)
                            - (a·x)/2 - b/(2·x) + sqrt(a·b)        */
template <class Type> Type invgauss_lpdf(Type x, Type a, Type b) {
  return 0.5 * log(b) - 0.5 * log(Type(2.0 * M_PI)) - 1.5 * log(x) -
         (a * x) / 2.0 - b / (2.0 * x) + sqrt(a * b);
}

template <class Type> Type objective_function<Type>::operator()() {

  /* ---------- DATA ---------- */
  DATA_VECTOR(y);  // observation y_t
  DATA_INTEGER(n); // sample length

  /* ---------- PARAMETERS ---------- */
  PARAMETER(mu);            // μ   (≥0)   — NIG mean parameter
  PARAMETER(log_sigma);     // log σ     — NIG scale parameter
  PARAMETER(rho_un);        // ρ constrained to (-1,1) via tanh
  PARAMETER(log_sigma_eps); // observation noise σ_ε
  PARAMETER(log_nu);        // ν (>0)  — IG(ν,ν)   (λ=ν)

  /* Latent variables */
  PARAMETER_VECTOR(W);    // AR(1) state
  PARAMETER_VECTOR(logV); // log V_t,  V_t ~ IG(ν,ν)

  /* ---------- Transformations ---------- */
  Type rho = tanh(rho_un);             // ρ ∈ (-1,1)
  Type sigma = exp(log_sigma);         // σ >0
  Type sigma_eps = exp(log_sigma_eps); // σ_ε >0
  Type nu = exp(log_nu);               // ν  >0
  vector<Type> V = exp(logV);          // V_t  >0

  /* ---------- Construct KW vector ----------
     K as shown in picture, lower bi-diagonal is (-rho, 1), first element
     sqrt(1-rho^2) */
  vector<Type> KW(n);
  KW(0) = sqrt(1.0 - rho * rho) * W(0);
  for (int t = 1; t < n; ++t) {
    KW(t) = W(t) - rho * W(t - 1);
  }

  /* ---------- Negative log-posterior ---------- */
  Type nll = 0.0;

  /* 1) V_t ~ IG(ν, ν);  need to add log-Jacobian (−logV) */
  for (int t = 0; t < n; ++t) {
    nll -= invgauss_lpdf(V(t), nu, nu);
    nll -= logV(t); // Jacobian for logV → V
  }

  /* 2) KW_t | V_t  ~  N( -μ + μ V_t ,  σ² V_t ) */
  for (int t = 0; t < n; ++t) {
    Type mean_t = -mu + mu * V(t);
    Type sd_t = sigma * sqrt(V(t));
    nll -= dnorm(KW(t), mean_t, sd_t, /*give_log=*/true);
  }

  /* 3) observation  y_t | W_t  ~  N( W_t ,  σ_ε ) */
  for (int t = 0; t < n; ++t) {
    nll -= dnorm(y(t), W(t), sigma_eps, /*give_log=*/true);
  }

  /* (Optional) Add weakly informative priors to μ, σ, ρ, σ_ε, ν */
  nll -= dnorm(mu, Type(0), Type(10), true);
  nll -= dnorm(log_sigma, Type(0), Type(2), true);
  nll -= dnorm(rho_un, Type(0), Type(2), true);
  nll -= dnorm(log_sigma_eps, Type(0), Type(2), true);
  nll -= dnorm(log_nu, Type(0), Type(2), true);

  return nll;
}
