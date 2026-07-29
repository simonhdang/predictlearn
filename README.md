# PredictLearn

Search a PLS-SEM model for structure your theory did not specify.

Give it raw indicator data. It estimates your measurement model, extracts
latent variable scores, then uses Bayesian network structure learning to
propose arcs you did not hypothesise, MARS and GAM to test whether those
relationships are linear or involve interactions, and out-of-sample error to
check whether any of it predicts better than the model you started with.

**It runs on your own computer. No data is uploaded anywhere.**

---

## Install

You need R, which is free from <https://cran.r-project.org>.
**RStudio is not required.**

Then download this repository (green **Code** button, **Download ZIP**), unzip
it, and double-click the launcher for your system:

| System | File |
|---|---|
| Windows | `PredictLearn (Windows).bat` |
| macOS | `PredictLearn (Mac).command` |

The first launch installs what it needs and takes a few minutes. See
[INSTALL.md](INSTALL.md) for troubleshooting, including the macOS security
prompt.

Already an R user? Skip all that:

```r
install.packages("remotes")
remotes::install_github("simonhdang/predictlearn")
predictlearn::run_app()
```

---

## What it does

1. **Measurement model.** Map indicators to constructs — read automatically
   from your item codes — and estimate. Reliability, AVE, HTMT, item-level
   loadings with "AVE if dropped", and full collinearity VIF are reported
   before anything else happens.
2. **Higher-order constructs.** Nominate a candidate and see the evidence:
   dimensionality, loadings on the common component, HTMT, and a predictive
   comparison of both specifications.
3. **Constraints.** Name the constructs nothing may cause, and forbid the
   reverse of paths your theory has already tested.
4. **Discovery.** Grow-Shrink and Hill-Climbing, compared on BIC and on
   out-of-sample error computed by one identical routine.
5. **Non-linearity.** MARS and GAM on the discovered equations, with a test of
   whether the curvature earns its keep, a tensor-product test for
   interactions, and simple slopes at ±1 SD.
6. **Export.** A plain R script reproducing the whole analysis without this
   package, so reviewers can audit what the interface did.

## What it will not do

Structure learning proposes; it does not confirm. Every arc is a hypothesis to
judge against theory, and the constraints exist so you can rule out in advance
what your theory has already settled. An unconstrained search returns a
well-fitting model that means nothing.

The same holds for higher-order constructs and for moderation. Whether a set of
dimensions belongs to one second-order construct is a question about your
theory (Sarstedt et al., 2019). An interaction term is symmetric, so which
variable moderates which is your claim, not the model's.

## Corrections to previously published code

The cross-validation routines modified from those in Appendix E of Dang, Quach
and Roberts (2025). Each marked in the source:

1. **Non-comparable evaluation.** Grow-Shrink was evaluated with
   `lm(target ~ .)`, a regression on every variable in the data, while
   Hill-Climbing used the fitted network's own parent set. Regressing a
   variable on its own descendants leaks information and understates the error,
   by an amount that grows with the number of descendants. Both algorithms now
   use one identical routine. See `cv_network_rmse()`.
2. **Mis-indexed target.** The Hill-Climbing loop scored predictions against itself
   
3. **In-sample "cross-validation".** The GAM fold loop refitted on the complete
   sample and predicted rows it had trained on. It now refits on training rows
   only. See `cv_gam()`.

## Method

Follows Richter, N. F., & Tudoran, A. A. (2024). Elevating theoretical insight
and predictive accuracy in business research: Combining PLS-SEM and selected
machine learning algorithms. *Journal of Business Research*, 173, 114453.

Scores are extracted from a saturated model under the factor weighting scheme,
so they do not already encode the structure being tested.

## Citing

> Dang, S. (2026). PredictLearn: Machine learning discovery of structure in
> PLS-SEM models (Version 0.1.0) [Computer software].
> https://doi.org/XXXXX (pending DOI)

## Licence

MIT
