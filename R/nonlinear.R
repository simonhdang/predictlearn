#' Non-linearity and interaction detection
#'
#' Once a structure has been selected, each of its regression equations is
#' probed for non-linearity using multivariate adaptive regression splines and
#' generalised additive models.

#' Fit MARS models for every equation implied by a DAG
#'
#' @param scores Latent variable scores.
#' @param equations Named list of node -> parents, from [dag_equations()].
#' @param max_degree Highest interaction degree to consider.
#' @param k Cross-validation folds.
#' @param seed Random seed.
#' @return A named list of caret train objects.
#' @export
fit_mars <- function(scores, equations, max_degree = 3, k = 10, seed = 123,
                     family = c("auto", "gaussian", "binomial")) {
  family <- match.arg(family)
  dat <- as.data.frame(scores)
  grid <- expand.grid(
    degree = seq_len(max_degree),
    nprune = floor(seq(2, 100, length.out = 10))
  )
  out <- lapply(names(equations), function(target) {
    preds <- equations[[target]]
    y     <- dat[[target]]

    use_binom <- family == "binomial" ||
      (family == "auto" && is_binary(y))
    if (use_binom) y <- as_binary01(y)

    set.seed(seed)
    fit <- if (use_binom) {
      # caret does not forward `glm` to earth, so passing family = binomial()
      # silently fitted a linear regression to a 0/1 outcome. Handing caret a
      # two-level factor puts it in classification mode instead, and earth
      # then fits the binomial GLM on top of the MARS basis itself.
      yf <- factor(y, levels = c(0, 1), labels = c("no", "yes"))
      caret::train(x = dat[, preds, drop = FALSE], y = yf,
                   method = "earth", metric = "Accuracy",
                   trControl = caret::trainControl(method = "cv", number = k),
                   tuneGrid = grid)
    } else {
      caret::train(x = dat[, preds, drop = FALSE], y = y,
                   method = "earth", metric = "RMSE",
                   trControl = caret::trainControl(method = "cv", number = k),
                   tuneGrid = grid)
    }
    attr(fit, "predictlearn_family") <- if (use_binom) "binomial" else "gaussian"
    fit
  })
  names(out) <- names(equations)
  out
}

#' Summarise MARS results
#' @param mars_fits Output of [fit_mars()].
#' @return A data frame of one row per equation.
#' @export
summarise_mars <- function(mars_fits) {
  # A binomial fit is scored on accuracy, a Gaussian one on RMSE, so the
  # results table has different columns depending on the family. Pull whatever
  # is there rather than assuming.
  pick <- function(df, col) {
    if (!is.null(df) && nrow(df) && col %in% names(df) &&
        is.numeric(df[[col]][1])) round(df[[col]][1], 3) else NA_real_
  }

  do.call(rbind, lapply(names(mars_fits), function(nm) {
    m <- mars_fits[[nm]]
    best <- tryCatch(
      m$results[m$results$nprune == m$bestTune$nprune &
                  m$results$degree == m$bestTune$degree, , drop = FALSE],
      error = function(e) NULL)
    terms <- tryCatch(length(stats::coef(m$finalModel)) - 1L,
                      error = function(e) NA_integer_)
    data.frame(
      equation     = nm,
      family       = attr(m, "predictlearn_family") %||% "gaussian",
      degree       = m$bestTune$degree,
      hinge_terms  = terms,
      RMSE         = pick(best, "RMSE"),
      Rsquared     = pick(best, "Rsquared"),
      MAE          = pick(best, "MAE"),
      Accuracy     = pick(best, "Accuracy"),
      Kappa        = pick(best, "Kappa"),
      # Number of RETAINED terms that multiply two or more predictors. The
      # tuned degree only records how far the search was allowed to look;
      # reporting it as though an interaction had been found overstates the
      # result badly.
      interactions = n_interaction_terms(m),
      stringsAsFactors = FALSE
    )
  }))
}

#' Interaction terms actually retained by a MARS model
#'
#' The tuned `degree` records how far the search was permitted to look, not
#' what survived pruning. A model tuned at degree 2 frequently keeps no product
#' terms at all. These functions read the retained basis functions instead.
#'
#' @param m A caret train object from [fit_mars()].
#' @return Integer count of retained terms involving two or more predictors.
#' @export
n_interaction_terms <- function(m) {
  tryCatch({
    fm   <- m$finalModel
    dirs <- fm$dirs[fm$selected.terms, , drop = FALSE]
    sum(rowSums(dirs != 0) > 1L)
  }, error = function(e) NA_integer_)
}

#' List the interaction terms a MARS model retained
#'
#' A MARS interaction is a product of hinge functions, so it acts only within
#' the range where both hinges are active. That is a local effect, not the
#' global multiplicative moderation a structural model tests. Treat anything
#' here as a lead to be confirmed in PLS-SEM, never as a moderation result.
#'
#' @param mars_fits Output of [fit_mars()].
#' @return A data frame of equation, term, and the variables involved, or NULL.
#' @export
mars_interactions <- function(mars_fits) {
  rows <- list()
  for (nm in names(mars_fits)) {
    fm <- tryCatch(mars_fits[[nm]]$finalModel, error = function(e) NULL)
    if (is.null(fm)) next
    dirs <- tryCatch(fm$dirs[fm$selected.terms, , drop = FALSE],
                     error = function(e) NULL)
    if (is.null(dirs) || !nrow(dirs)) next
    keep <- which(rowSums(dirs != 0) > 1L)
    for (i in keep) {
      vars <- colnames(dirs)[dirs[i, ] != 0]
      rows[[length(rows) + 1L]] <- data.frame(
        equation  = nm,
        term      = rownames(dirs)[i],
        variables = paste(vars, collapse = " x "),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

#' Fit GAM models for every equation implied by a DAG
#'
#' Continuous parents are given smooth terms; parents with few distinct values
#' are entered parametrically, because a smooth over a binary or ordinal
#' variable is not meaningful.
#'
#' @param scores Latent variable scores.
#' @param equations Named list of node -> parents.
#' @param min_unique Minimum distinct values before a term is smoothed.
#' @return A named list of mgcv gam objects.
#' @export
fit_gam <- function(scores, equations, min_unique = 10,
                    family = c("auto", "gaussian", "binomial")) {
  family <- match.arg(family)
  dat <- as.data.frame(scores)
  out <- lapply(names(equations), function(target) {
    preds <- equations[[target]]
    terms <- vapply(preds, function(p) {
      if (length(unique(stats::na.omit(dat[[p]]))) >= min_unique) {
        paste0("s(", p, ")")
      } else {
        p
      }
    }, character(1))
    f <- stats::as.formula(paste(target, "~", paste(terms, collapse = " + ")))

    use_binom <- family == "binomial" ||
      (family == "auto" && is_binary(dat[[target]]))

    d <- dat
    if (use_binom) d[[target]] <- as_binary01(dat[[target]])

    fit <- mgcv::gam(f, data = d,
                     family = if (use_binom) stats::binomial() else stats::gaussian())
    attr(fit, "predictlearn_family") <- if (use_binom) "binomial" else "gaussian"
    fit
  })
  names(out) <- names(equations)
  out
}

#' Summarise GAM results, including which terms are non-linear
#' @param gam_fits Output of [fit_gam()].
#' @return A data frame of one row per smooth or parametric term.
#' @export
summarise_gam <- function(gam_fits) {
  do.call(rbind, lapply(names(gam_fits), function(nm) {
    s <- summary(gam_fits[[nm]])
    if (is.null(s$s.table) || nrow(s$s.table) == 0L) {
      return(data.frame(equation = nm,
                        family = attr(gam_fits[[nm]], "predictlearn_family") %||% "gaussian",
                        term = NA_character_, edf = NA_real_,
                        F_value = NA_real_, p_value = NA_real_,
                        non_linear = NA, stringsAsFactors = FALSE))
    }
    tab <- as.data.frame(s$s.table)
    data.frame(
      equation   = nm,
      family     = attr(gam_fits[[nm]], "predictlearn_family") %||% "gaussian",
      term       = rownames(tab),
      edf        = round(tab[["edf"]], 3),
      F_value    = round(tab[["F"]], 3),
      p_value    = signif(tab[["p-value"]], 3),
      # An edf near 1 means the smooth has collapsed to a straight line.
      non_linear = tab[["edf"]] > 1.05,
      stringsAsFactors = FALSE
    )
  }))
}

#' Test for interactions between predictors in a GAM
#'
#' The additive models fitted by [fit_gam()] cannot represent an interaction:
#' the response is a sum of separate smooths. This adds a tensor product
#' interaction term `ti(a, b)` for each pair of continuous predictors in an
#' equation and tests whether it improves fit over the additive model.
#'
#' This is the proper test of the question MARS only gestures at. A significant
#' `ti()` term means the effect of one predictor on the outcome genuinely
#' varies with the level of the other, across their observed range, rather than
#' only where two hinge functions happen to overlap.
#'
#' It still is not a moderation result. Which variable moderates which is a
#' theoretical claim, and the interaction should be re-estimated in the
#' structural model before it is reported as one.
#'
#' @param scores Latent variable scores.
#' @param equations Named list of node -> parents, from [dag_equations()].
#' @param min_unique Predictors with fewer distinct values are skipped.
#' @param family "auto", "gaussian" or "binomial".
#' @return A data frame of pairwise tests, or NULL if none could be run.
#' @export
test_gam_interactions <- function(scores, equations, min_unique = 10,
                                  family = c("auto", "gaussian", "binomial")) {
  family <- match.arg(family)
  dat <- as.data.frame(scores)
  rows <- list()

  for (target in names(equations)) {
    preds <- equations[[target]]
    smooth_ok <- preds[vapply(preds, function(p)
      length(unique(stats::na.omit(dat[[p]]))) >= min_unique, logical(1))]
    if (length(smooth_ok) < 2L) next

    use_binom <- family == "binomial" ||
      (family == "auto" && is_binary(dat[[target]]))
    d <- dat
    if (use_binom) d[[target]] <- as_binary01(dat[[target]])
    fam <- if (use_binom) stats::binomial() else stats::gaussian()
    tst <- if (use_binom) "Chisq" else "F"

    # Non-smoothed predictors stay in both models as parametric terms.
    extra <- setdiff(preds, smooth_ok)

    pairs <- utils::combn(smooth_ok, 2L, simplify = FALSE)
    for (pr in pairs) {
      a <- pr[1]; b <- pr[2]
      rhs_base <- c(sprintf("s(%s)", smooth_ok), extra)
      f_base <- stats::as.formula(
        paste(target, "~", paste(rhs_base, collapse = " + ")))
      f_full <- stats::as.formula(
        paste(target, "~", paste(c(rhs_base, sprintf("ti(%s, %s)", a, b)),
                                 collapse = " + ")))

      res <- tryCatch({
        m0 <- mgcv::gam(f_base, data = d, family = fam)
        m1 <- mgcv::gam(f_full, data = d, family = fam)
        an <- stats::anova(m0, m1, test = tst)

        st <- summary(m1)$s.table
        ti_row <- grep("^ti\\(", rownames(st))
        pcol <- intersect(c("Pr(>F)", "Pr(>Chi)"), names(an))[1]
        p <- if (!is.na(pcol)) an[[pcol]][2] else NA_real_

        data.frame(
          equation   = target,
          pair       = paste(a, "x", b),
          ti_edf     = if (length(ti_row)) round(st[ti_row[1], "edf"], 3) else NA_real_,
          deviance   = round(an[["Deviance"]][2] %||% NA_real_, 3),
          p_value    = signif(p, 3),
          supported  = !is.na(p) && p < 0.05,
          stringsAsFactors = FALSE)
      }, error = function(e) {
        data.frame(equation = target, pair = paste(a, "x", b),
                   ti_edf = NA_real_, deviance = NA_real_,
                   p_value = NA_real_, supported = NA,
                   stringsAsFactors = FALSE)
      })
      rows[[length(rows) + 1L]] <- res
    }
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out[order(out$p_value), ]
}

# ---------------------------------------------------------------------------
# Probing an interaction
# ---------------------------------------------------------------------------

#' Fit, plot and probe one interaction
#'
#' Refits the equation with a tensor product term for the chosen pair, then
#' describes the interaction in the two ways a reader needs: predicted curves
#' for the outcome across the focal predictor at three levels of the moderator,
#' and the slope of the focal predictor at each of those levels.
#'
#' Which variable is focal and which is the moderator is your choice, not the
#' model's. The product term is symmetric, so `a x b` and `b x a` are the same
#' fit; swapping them changes only how the result is described. The point of
#' looking at the surface is to see whether the effect strengthens, weakens or
#' reverses, and therefore which framing your theory can support.
#'
#' @param scores Latent variable scores.
#' @param equations Named list of node -> parents.
#' @param target Outcome, one of `names(equations)`.
#' @param focal Predictor whose effect is being described.
#' @param moderator Predictor whose level the effect is conditioned on.
#' @param at Moderator levels, in standard deviations from its mean.
#' @param n Points along the focal range.
#' @param family "auto", "gaussian" or "binomial".
#' @return A list with `model`, `curves`, `slopes`, and labels.
#' @export
analyse_interaction <- function(scores, equations, target, focal, moderator,
                                at = c(-1, 0, 1), n = 60,
                                family = c("auto", "gaussian", "binomial")) {
  family <- match.arg(family)
  dat <- as.data.frame(scores)

  preds <- equations[[target]]
  if (is.null(preds)) stop("'", target, "' is not an outcome in the structure.")
  if (!all(c(focal, moderator) %in% preds)) {
    stop("Both ", focal, " and ", moderator, " must be predictors of ", target,
         ". Its predictors are: ", paste(preds, collapse = ", "), ".")
  }

  use_binom <- family == "binomial" ||
    (family == "auto" && is_binary(dat[[target]]))
  if (use_binom) dat[[target]] <- as_binary01(dat[[target]])
  fam <- if (use_binom) stats::binomial() else stats::gaussian()

  smooth_ok <- preds[vapply(preds, function(p)
    length(unique(stats::na.omit(dat[[p]]))) >= 10L, logical(1))]
  extra <- setdiff(preds, smooth_ok)

  rhs <- c(sprintf("s(%s)", smooth_ok), extra,
           sprintf("ti(%s, %s)", focal, moderator))
  f <- stats::as.formula(paste(target, "~", paste(rhs, collapse = " + ")))
  m <- mgcv::gam(f, data = dat, family = fam)

  # --- grid ------------------------------------------------------------------
  fx <- dat[[focal]]; mo <- dat[[moderator]]
  fseq <- seq(stats::quantile(fx, 0.02, na.rm = TRUE),
              stats::quantile(fx, 0.98, na.rm = TRUE), length.out = n)
  mlev <- mean(mo, na.rm = TRUE) + at * stats::sd(mo, na.rm = TRUE)
  labs <- paste0(ifelse(at > 0, "+", ""), at, " SD")

  base_row <- dat[1, , drop = FALSE]
  for (v in setdiff(preds, c(focal, moderator))) {
    base_row[[v]] <- if (is.numeric(dat[[v]])) mean(dat[[v]], na.rm = TRUE)
                     else dat[[v]][1]
  }

  grid <- do.call(rbind, lapply(seq_along(mlev), function(i) {
    g <- base_row[rep(1, n), , drop = FALSE]
    g[[focal]] <- fseq
    g[[moderator]] <- mlev[i]
    g$.level <- labs[i]
    g
  }))

  pr <- stats::predict(m, newdata = grid, se.fit = TRUE)
  curves <- data.frame(
    focal_value = grid[[focal]],
    level       = factor(grid$.level, levels = labs),
    fit         = as.numeric(pr$fit),
    lower       = as.numeric(pr$fit - 1.96 * pr$se.fit),
    upper       = as.numeric(pr$fit + 1.96 * pr$se.fit),
    stringsAsFactors = FALSE)

  # --- simple slopes ---------------------------------------------------------
  # The slope of a smooth is not constant, so this reports the average slope
  # across the central range: the change in the linear predictor between the
  # focal predictor at -1 SD and +1 SD, divided by that distance. Standard
  # errors come from the model's covariance matrix, so the test is exact for
  # that contrast.
  lo <- mean(fx, na.rm = TRUE) - stats::sd(fx, na.rm = TRUE)
  hi <- mean(fx, na.rm = TRUE) + stats::sd(fx, na.rm = TRUE)
  Vp <- m$Vp

  slopes <- do.call(rbind, lapply(seq_along(mlev), function(i) {
    g <- base_row[rep(1, 2), , drop = FALSE]
    g[[focal]] <- c(lo, hi)
    g[[moderator]] <- mlev[i]
    X <- stats::predict(m, newdata = g, type = "lpmatrix")
    cvec <- (X[2, ] - X[1, ]) / (hi - lo)
    est <- sum(cvec * stats::coef(m))
    se  <- sqrt(as.numeric(t(cvec) %*% Vp %*% cvec))
    z   <- est / se
    data.frame(
      moderator_at = labs[i],
      slope        = round(est, 4),
      std_error    = round(se, 4),
      z            = round(z, 3),
      p_value      = signif(2 * stats::pnorm(-abs(z)), 3),
      stringsAsFactors = FALSE)
  }))

  list(model = m, curves = curves, slopes = slopes,
       target = target, focal = focal, moderator = moderator,
       family = if (use_binom) "binomial" else "gaussian")
}

#' Plot the curves from [analyse_interaction()]
#' @param res Output of [analyse_interaction()].
#' @export
plot_interaction <- function(res) {
  cu   <- res$curves
  levs <- levels(cu$level)
  cols <- c("#A8742A", "#5A6360", "#2D5F5D")[seq_along(levs)]

  op <- graphics::par(mar = c(4.2, 4.2, 2.5, 1))
  on.exit(graphics::par(op), add = TRUE)

  ylab <- if (identical(res$family, "binomial")) {
    paste0(res$target, " (log odds)")
  } else res$target

  graphics::plot(range(cu$focal_value), range(c(cu$lower, cu$upper)),
                 type = "n", xlab = res$focal, ylab = ylab,
                 main = sprintf("%s by %s, at levels of %s",
                                res$target, res$focal, res$moderator),
                 bty = "n", cex.main = 1)

  for (i in seq_along(levs)) {
    d <- cu[cu$level == levs[i], ]
    graphics::polygon(c(d$focal_value, rev(d$focal_value)),
                      c(d$lower, rev(d$upper)),
                      col = grDevices::adjustcolor(cols[i], alpha.f = 0.12),
                      border = NA)
    graphics::lines(d$focal_value, d$fit, col = cols[i], lwd = 2)
  }
  graphics::legend("topleft", legend = paste(res$moderator, levs),
                   col = cols, lwd = 2, bty = "n", cex = 0.85)
  invisible(res)
}

#' Out-of-sample performance for a GAM
#'
#' CORRECTION 3 (Appendix E). The published loop refitted the model with
#' `data = mldata`, the complete sample, inside every fold and then predicted
#' rows that model had been trained on. The reported statistics were therefore
#' in-sample. Here the model is refitted on the training rows only.
#'
#' RMSE is pooled as sqrt(mean(MSE)) across folds rather than averaged from
#' per-fold RMSEs, which is the standard aggregation.
#'
#' @param scores Latent variable scores.
#' @param formula A model formula.
#' @param k Folds.
#' @param seed Random seed.
#' @return Named numeric vector of RMSE, MAE and cross-validated R squared.
#' @export
cv_gam <- function(scores, formula, k = 10, seed = 123,
                   family = c("auto", "gaussian", "binomial")) {
  family <- match.arg(family)
  dat  <- as.data.frame(scores)
  resp <- all.vars(formula)[1]

  use_binom <- family == "binomial" ||
    (family == "auto" && is_binary(dat[[resp]]))
  if (use_binom) dat[[resp]] <- as_binary01(dat[[resp]])
  fam <- if (use_binom) stats::binomial() else stats::gaussian()

  set.seed(seed)
  folds <- caret::createFolds(seq_len(nrow(dat)), k = k,
                              list = TRUE, returnTrain = TRUE)
  a <- b <- d <- numeric(k)

  for (j in seq_len(k)) {
    train <- dat[folds[[j]], , drop = FALSE]
    test  <- dat[-folds[[j]], , drop = FALSE]
    fit   <- mgcv::gam(formula, data = train, family = fam)  # training rows only
    y     <- test[[resp]]

    if (use_binom) {
      # RMSE is not meaningful for a 0/1 outcome. Brier score is the mean
      # squared error on the probability scale; log loss penalises confident
      # mistakes; accuracy is at a 0.5 cut.
      p <- stats::predict(fit, newdata = test, type = "response")
      p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
      a[j] <- mean((y - p)^2, na.rm = TRUE)
      b[j] <- -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
      d[j] <- mean((p > 0.5) == (y == 1), na.rm = TRUE)
    } else {
      pred <- stats::predict(fit, newdata = test)
      err  <- y - pred
      a[j] <- mean(err^2, na.rm = TRUE)
      b[j] <- mean(abs(err), na.rm = TRUE)
      d[j] <- 1 - a[j] / mean((y - mean(y))^2)
    }
  }

  out <- if (use_binom) {
    c(Brier = mean(a), LogLoss = mean(b), Accuracy = mean(d))
  } else {
    c(RMSE = sqrt(mean(a)), MAE = mean(b), cv_R2 = mean(d))
  }
  attr(out, "family") <- if (use_binom) "binomial" else "gaussian"
  out
}

#' Test whether a smooth term earns its flexibility
#'
#' Compares each fitted GAM against a version constrained to be linear. A
#' non-significant test means the non-linearity detected by MARS or by the
#' effective degrees of freedom does not improve fit enough to keep.
#'
#' @param scores Latent variable scores.
#' @param gam_fits Output of [fit_gam()].
#' @return A data frame of deviance tests.
#' @export
test_nonlinearity <- function(scores, gam_fits) {
  dat <- as.data.frame(scores)
  do.call(rbind, lapply(names(gam_fits), function(nm) {
    fit <- gam_fits[[nm]]
    f   <- stats::formula(fit)
    rhs <- attr(stats::terms(f), "term.labels")
    linear_rhs <- gsub("^s\\(([^,)]+).*\\)$", "\\1", rhs)
    f_lin <- stats::as.formula(
      paste(all.vars(f)[1], "~", paste(linear_rhs, collapse = " + ")))
    fam <- attr(fit, "predictlearn_family") %||% "gaussian"
    d <- dat
    if (identical(fam, "binomial")) d[[all.vars(f)[1]]] <- as_binary01(dat[[all.vars(f)[1]]])
    fit_lin <- mgcv::gam(f_lin, data = d,
                         family = if (identical(fam, "binomial"))
                           stats::binomial() else stats::gaussian())
    # An F test assumes an estimated scale parameter; the binomial family
    # fixes it, so the likelihood ratio is compared against chi-squared.
    tst <- if (identical(fam, "binomial")) "Chisq" else "F"
    a <- tryCatch(stats::anova(fit_lin, fit, test = tst),
                  error = function(e) NULL)
    if (is.null(a) || nrow(a) < 2L) {
      return(data.frame(equation = nm, family = fam, F_value = NA_real_,
                        p_value = NA_real_, keep_nonlinear = NA,
                        stringsAsFactors = FALSE))
    }
    pcol <- intersect(c("Pr(>F)", "Pr(>Chi)"), names(a))[1]
    scol <- intersect(c("F", "Deviance", "Chisq"), names(a))[1]
    p <- if (!is.na(pcol)) a[[pcol]][2] else NA_real_
    data.frame(
      equation       = nm,
      family         = fam,
      F_value        = if (!is.na(scol)) round(a[[scol]][2], 3) else NA_real_,
      p_value        = signif(p, 3),
      keep_nonlinear = !is.na(p) && p < 0.05,
      stringsAsFactors = FALSE
    )
  }))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
