# RLT <img src="https://img.shields.io/badge/version-6.0.2-blue" alt="version"> <img src="https://img.shields.io/badge/license-GPL%20(%3E%3D%203)-green" alt="license">

**Reinforcement Learning Trees** — high-performance random forests for R with C++/Rcpp backend.

> [Ruoqing Zhu](https://teazrq.github.io/) · CRAN: [v3.2.6](https://cran.r-project.org/web/packages/RLT/index.html)

---

### Highlights

- **Three model types** — regression, classification, and survival (logrank, sup-logrank, Cox gradient splits)
- **Linear combination (LC) splits** — SIR, LM, PCA, LDA, CoxPH-based multivariate splits across all models
- **Reinforcement learning** — embedded trees with VI gating, variable muting, and tiered LC selection
- **Variance estimation** — matched, infinitesimal jackknife (IJ), and jackknife variance for predictions
- **Variable importance** — permutation importance with variance estimates (`$VarVI`); distribution-based importance
- **Survival analysis** — Kaplan–Meier curves with confidence bands, distribution-based variable importance
- **Parallel & reproducible** — bit-identical results across `ncores`
- **326+ tests** across all models

### Installation

```r
devtools::install_github("rqzhu-aide/RLT")
```

Requires: R ≥ 4.0, C++ compiler with OpenMP support.

### Quick Example

```r
library(RLT)

# Regression with variance
fit <- RLT(x, y, model = "regression", ntrees = 500,
           importance = TRUE, var.mode = "matched")
pred <- predict(fit, x_test, var.mode = "matched")
pred$Variance     # prediction variance
fit$VarVI         # variable importance variance

# Survival with LC splits
fit <- RLT(x, Y, C, model = "survival", ntrees = 500,
           linear.comb = 3, linear.comb.method = "coxph")

# Reinforcement learning
fit <- RLT(x, y, model = "regression", ntrees = 500,
           reinforcement = TRUE, linear.comb = 3,
           linear.comb.method = "sir")
```

### Supported Models at a Glance

| Model | Split Rules | LC Methods | Variance |
|-------|------------|------------|----------|
| **Regression** | variance | naive, LM, PCA, SIR | matched, IJ, jackknife |
| **Classification** | gini | LDA, naive, random, logistic | matched, IJ, jackknife |
| **Survival** | logrank, sup-logrank, Cox gradient | CoxPH, naive | matched, IJ, jackknife |

---

### References

- Zhu, R., Zeng, D., & Kosorok, M. R. (2015). Reinforcement Learning Trees. *JASA*, 110(512), 1770–1784. [DOI](https://doi.org/10.1080/01621459.2015.1036994)
- Xu, T., Zhu, R., & Shao, X. (2024). On variance estimation of random forests with infinite-order U-statistics. *EJS*, 18(1), 2135–2207.
- Formentini, S., Liang, K., & Zhu, R. (2022). Survival Function Confidence Band Estimation using Random Forests. [arXiv:2204.12038](https://arxiv.org/abs/2204.12038)
