#include <TMB.hpp>

/* ---------- Inverse-Gaussian log-pdf  --------------------
   IG(a, b):  log f(x;a,b) = (1/2)log(b) - (1/2)log(2π) - (3/2)log(x) 
                            - (a·x)/2 - b/(2·x) + sqrt(a·b)        */
template<class Type>
Type invgauss_lpdf(Type x, Type a, Type b){
  return 0.5*log(b) 
       - 0.5*log(Type(2.0*M_PI)) 
       - 1.5*log(x)
       - (a * x) / 2.0
       - b / (2.0 * x)
       + sqrt(a * b);
}

template<class Type>
Type objective_function<Type>::operator() () {

  /* ---------- DATA ---------- */
  DATA_VECTOR(y);          // 观测 y_t
  DATA_INTEGER(n);         // 样本长度

  /* ---------- PARAMETERS ---------- */
  PARAMETER(mu);                     // μ   (≥0)   — NIG 均值参数
  PARAMETER(log_sigma);              // log σ     — NIG 尺度参数
  PARAMETER(rho_un);                 // ρ 经 tanh 约束到 (-1,1)
  PARAMETER(log_sigma_eps);          // 观测噪声 σ_ε
  PARAMETER(log_nu);                 // ν (>0)  — IG(ν,ν)   (λ=ν)

  /* 潜变量 */
  PARAMETER_VECTOR(W);               // AR(1) 状态
  PARAMETER_VECTOR(logV);            // log V_t,  V_t ~ IG(ν,ν)

  /* ---------- 变换 ---------- */
  Type rho        = tanh(rho_un);           // ρ ∈ (-1,1)
  Type sigma      = exp(log_sigma);         // σ >0
  Type sigma_eps  = exp(log_sigma_eps);     // σ_ε >0
  Type nu         = exp(log_nu);            // ν  >0
  vector<Type> V  = exp(logV);              // V_t  >0

  /* ---------- 构造 K W 向量 ----------
     K 如图片所示，下三对角为 (-ρ,1)，首元素 √(1-ρ²)              */
  vector<Type> KW(n);
  KW(0) = sqrt(1.0 - rho*rho) * W(0);
  for(int t=1; t<n; ++t){
    KW(t) = W(t) - rho * W(t-1);
  }

  /* ---------- 负对数后验 ---------- */
  Type nll = 0.0;

  /* 1) V_t ~ IG(ν, ν);  需要加上 log-Jacobian (−logV) */
  for(int t=0; t<n; ++t){
    nll -= invgauss_lpdf(V(t), nu, nu);
    nll -= logV(t);                       // Jacobian for logV → V
  }

  /* 2) KW_t | V_t  ~  N( -μ + μ V_t ,  σ² V_t ) */
  for(int t=0; t<n; ++t){
    Type mean_t = -mu + mu * V(t);
    Type sd_t   = sigma * sqrt(V(t));
    nll -= dnorm(KW(t), mean_t, sd_t, /*give_log=*/true);
  }

  /* 3) 观测  y_t | W_t  ~  N( W_t ,  σ_ε ) */
  for(int t=0; t<n; ++t){
    nll -= dnorm(y(t), W(t), sigma_eps, /*give_log=*/true);
  }

  /* （可选）给 μ, σ, ρ, σ_ε, ν 加弱信息先验 */
  nll -= dnorm(mu,        Type(0), Type(10), true);
  nll -= dnorm(log_sigma, Type(0), Type(2),  true);
  nll -= dnorm(rho_un,    Type(0), Type(2),  true);
  nll -= dnorm(log_sigma_eps, Type(0), Type(2), true);
  nll -= dnorm(log_nu,    Type(0), Type(2),  true);

  return nll;
}
