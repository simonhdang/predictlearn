#' Higher-order construct diagnostics
#'
#' Whether a set of first-order constructs should be modelled as dimensions of
#' a higher-order construct is a theoretical decision, not a statistical one
#' (Sarstedt et al., 2019). These functions do not make that decision. They
#' report the evidence a researcher would weigh when making it, and the calling
#' code is expected to require explicit confirmation before any higher-order
#' construct is added to a model.

#' Diagnose a nominated higher-order construct
#'
#' @param scores Data frame of first-order latent variable scores.
#' @param dimensions Character vector naming the candidate dimensions.
#' @param htmt Optional HTMT matrix from [assess_measurement()].
#' @return An object of class `predictlearn_hoc_diagnostic`.
#' @export
diagnose_hoc <- function(scores, dimensions, htmt = NULL) {
  missing_dims <- setdiff(dimensions, names(scores))
  if (length(missing_dims)) {
    stop("These dimensions are not among the construct scores: ",
         paste(missing_dims, collapse = ", "))
  }
  if (length(dimensions) < 2L) {
    stop("A higher-order construct needs at least two dimensions.")
  }

  X <- as.matrix(scores[, dimensions, drop = FALSE])
  R <- stats::cor(X, use = "pairwise.complete.obs")

  # --- 1. Dimensionality -----------------------------------------------------
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  first_share  <- ev[1] / sum(ev)
  second_share <- if (length(ev) > 1L) ev[2] / sum(ev) else NA_real_

  # --- 2. Loadings of each dimension on the first principal component -------
  pc  <- stats::prcomp(X, center = TRUE, scale. = TRUE)
  pc1 <- pc$x[, 1]
  dim_loadings <- vapply(dimensions,
                         function(d) stats::cor(X[, d], pc1), numeric(1))
  # Sign is arbitrary in PCA; orient so the majority load positively.
  if (mean(dim_loadings) < 0) dim_loadings <- -dim_loadings

  # --- 3. Inter-dimension correlations --------------------------------------
  off <- R[upper.tri(R)]

  # --- 4. HTMT among the dimensions, if available ---------------------------
  htmt_sub <- NULL
  if (!is.null(htmt)) {
    hm <- as.matrix(htmt)
    keep <- intersect(dimensions, rownames(hm))
    if (length(keep) >= 2L) htmt_sub <- hm[keep, keep, drop = FALSE]
  }

  # --- Evidence summary ------------------------------------------------------
  evidence <- c(
    sprintf("First component explains %.1f%% of the variance among the %d dimensions.",
            100 * first_share, length(dimensions)),
    if (!is.na(second_share))
      sprintf("Second component explains %.1f%%.", 100 * second_share),
    sprintf("Dimension loadings on the first component range from %.2f to %.2f.",
            min(dim_loadings), max(dim_loadings)),
    sprintf("Inter-dimension correlations range from %.2f to %.2f.",
            min(off), max(off))
  )

  concerns <- character(0)
  if (first_share < 0.50) {
    concerns <- c(concerns, paste(
      "The first component explains less than half the variance, so these",
      "dimensions do not share a single dominant source. A reflective",
      "higher-order construct is hard to justify on this evidence."))
  }
  if (any(dim_loadings < 0.60)) {
    weak <- names(dim_loadings)[dim_loadings < 0.60]
    concerns <- c(concerns, paste0(
      "These dimensions load below 0.60 on the common component: ",
      paste(weak, collapse = ", "),
      ". They may not belong to the same higher-order construct."))
  }
  if (min(off) < 0) {
    concerns <- c(concerns, paste(
      "At least one pair of dimensions is negatively correlated, which is",
      "inconsistent with a reflective higher-order specification."))
  }

  structure(
    list(
      dimensions    = dimensions,
      correlations  = R,
      eigenvalues   = ev,
      first_share   = first_share,
      second_share  = second_share,
      dim_loadings  = dim_loadings,
      htmt          = htmt_sub,
      evidence      = evidence,
      concerns      = concerns
    ),
    class = "predictlearn_hoc_diagnostic"
  )
}

#' @export
print.predictlearn_hoc_diagnostic <- function(x, ...) {
  cat("Higher-order construct diagnostic\n")
  cat("Dimensions: ", paste(x$dimensions, collapse = ", "), "\n\n", sep = "")
  cat("Evidence\n")
  for (e in x$evidence) cat("  - ", e, "\n", sep = "")
  if (length(x$concerns)) {
    cat("\nConcerns\n")
    for (c_ in x$concerns) cat("  - ", c_, "\n", sep = "")
  }
  cat("\nThis is evidence, not a recommendation. Whether these dimensions\n")
  cat("belong to one higher-order construct is a question about your theory.\n")
  invisible(x)
}

#' Compare a higher-order specification against a first-order-only one
#'
#' Estimates both models and compares how well each predicts a nominated
#' downstream target out of sample. This is a predictive comparison only; it
#' cannot tell you whether the higher-order construct is conceptually correct.
#'
#' @param data Raw indicator data.
#' @param mapping The indicator-to-construct mapping.
#' @param dimensions Candidate dimensions of the higher-order construct.
#' @param hoc_name Name to give the higher-order construct.
#' @param target Name of the downstream construct to predict.
#' @param k Number of cross-validation folds.
#' @param seed Random seed.
#' @return A data frame comparing out-of-sample RMSE and MAE.
#' @export
compare_hoc_specification <- function(data, mapping, dimensions, hoc_name,
                                      target, k = 10, seed = 123) {
  if (!target %in% mapping$construct) {
    stop("Target '", target, "' is not a construct in the mapping.")
  }
  if (target %in% dimensions) {
    stop("The target cannot also be a dimension of the higher-order construct.")
  }

  mm_first <- build_measurement_model(mapping)
  mm_hoc   <- build_measurement_model(
    mapping,
    higher_order = stats::setNames(list(dimensions), hoc_name)
  )

  cv_predict <- function(mm, predictors_from) {
    sc <- extract_scores(data, mm)$scores
    preds <- intersect(predictors_from, names(sc))
    if (!length(preds)) stop("No usable predictors were found.")
    cv_lm_rmse(sc, target, preds, k = k, seed = seed)
  }

  first <- cv_predict(mm_first, dimensions)
  hoc   <- cv_predict(mm_hoc,   hoc_name)

  data.frame(
    specification = c("First-order dimensions as separate predictors",
                      paste0("Higher-order construct '", hoc_name, "'")),
    n_predictors  = c(length(dimensions), 1L),
    RMSE          = c(first[["RMSE"]], hoc[["RMSE"]]),
    MAE           = c(first[["MAE"]],  hoc[["MAE"]]),
    stringsAsFactors = FALSE
  )
}

#' Suggest candidate groupings of first-order constructs
#'
#' Exploratory only. Hierarchical clustering on correlation distance will
#' always return groups, whether or not any higher-order construct exists. Use
#' the output to prompt thinking, never as a result.
#'
#' @param scores Data frame of first-order construct scores.
#' @param k Number of groups to cut the dendrogram into.
#' @param exclude Constructs to leave out, such as demographics.
#' @return A data frame of construct and suggested group.
#' @export
suggest_hoc_groups <- function(scores, k = 3, exclude = character(0)) {
  keep <- setdiff(names(scores), exclude)
  if (length(keep) < 3L) stop("At least three constructs are needed.")
  X <- as.matrix(scores[, keep, drop = FALSE])
  d <- stats::as.dist(1 - abs(stats::cor(X, use = "pairwise.complete.obs")))
  cl <- stats::hclust(d, method = "average")
  data.frame(
    construct = keep,
    group     = stats::cutree(cl, k = min(k, length(keep) - 1L)),
    stringsAsFactors = FALSE
  )
}
