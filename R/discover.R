#' Structure discovery and non-linearity detection
#'
#' The cross-validation routines here differ deliberately from the code
#' published as Appendix E of Dang, Quach and Roberts (2025). Three defects in
#' that code are corrected, and each correction is marked below so that results
#' produced by this package can be reconciled against the published figures.

# ---------------------------------------------------------------------------
# Blacklist
# ---------------------------------------------------------------------------

#' Build an arc blacklist from a logical matrix
#'
#' The blacklist is where theory enters the search. Each forbidden arc removes
#' a hypothesis the search would otherwise be free to propose.
#'
#' @param blocked A logical matrix, rows = from, columns = to. TRUE forbids
#'   that arc.
#' @return A data frame with `from` and `to`, or NULL if nothing is blocked.
#' @export
blacklist_from_matrix <- function(blocked) {
  stopifnot(is.matrix(blocked), is.logical(blocked))
  idx <- which(blocked, arr.ind = TRUE)
  if (nrow(idx) == 0L) return(NULL)
  data.frame(
    from = rownames(blocked)[idx[, "row"]],
    to   = colnames(blocked)[idx[, "col"]],
    stringsAsFactors = FALSE
  )
}

#' A blacklist that forbids every arc already tested in the baseline model
#'
#' @param paths Data frame with `from` and `to`, the hypothesised paths.
#' @param exogenous Constructs nothing may point into, such as demographics or
#'   stable traits.
#' @param all_constructs All construct names in the model.
#' @return A blacklist data frame.
#' @export
default_blacklist <- function(paths, exogenous = character(0),
                              all_constructs) {
  bl <- data.frame(from = character(0), to = character(0),
                   stringsAsFactors = FALSE)

  # Arcs that reverse a hypothesised path
  if (nrow(paths)) {
    bl <- rbind(bl, data.frame(from = paths$to, to = paths$from,
                               stringsAsFactors = FALSE))
  }
  # Nothing may cause an exogenous variable
  for (ex in exogenous) {
    others <- setdiff(all_constructs, ex)
    if (length(others)) {
      bl <- rbind(bl, data.frame(from = others, to = ex,
                                 stringsAsFactors = FALSE))
    }
  }
  unique(bl)
}

# ---------------------------------------------------------------------------
# Variable types
# ---------------------------------------------------------------------------

#' Classify latent variable scores by measurement level
#'
#' @param scores Data frame of scores.
#' @param max_discrete Values at or below this count are treated as discrete.
#' @return A named character vector: constant, binary, few-valued, continuous.
#' @export
variable_types <- function(scores, max_discrete = 6L) {
  vapply(as.data.frame(scores), function(x) {
    u <- length(unique(stats::na.omit(x)))
    if (u <= 1L) "constant"
    else if (u == 2L) "binary"
    else if (u <= max_discrete) "few-valued"
    else "continuous"
  }, character(1))
}

#' Is a vector binary?
#' @param x A vector.
#' @return TRUE if it takes exactly two distinct non-missing values.
#' @export
is_binary <- function(x) length(unique(stats::na.omit(x))) == 2L

#' Recode a two-valued vector to 0 and 1
#' @param x A vector taking two distinct values.
#' @return An integer vector of 0 and 1.
#' @export
as_binary01 <- function(x) {
  u <- sort(unique(stats::na.omit(x)))
  if (length(u) != 2L) stop("Not a two-valued variable.")
  as.integer(x == u[2])
}

#' Warn about variables that violate the assumptions of structure learning
#'
#' Both search algorithms assume continuous, roughly Gaussian variables:
#' Grow-Shrink tests correlations, Hill-Climbing scores a Gaussian likelihood.
#' A binary variable does not break either of them, but as an outcome it turns
#' the network into a linear probability model, whose fitted values run outside
#' zero and one and whose error variance is not constant.
#'
#' The obvious remedy does not apply here. bnlearn supports mixed data through
#' conditional Gaussian scores, but that formulation forbids discrete nodes from
#' having continuous parents, which is precisely the structure a binary outcome
#' with latent antecedents requires.
#'
#' @param scores Data frame of latent variable scores.
#' @param targets Nodes to be evaluated for predictive error.
#' @param max_discrete Values at or below this count are treated as discrete.
#' @return A list with `errors` and `warnings`.
#' @export
check_discovery_variables <- function(scores, targets = character(0),
                                      max_discrete = 6L) {
  ty <- variable_types(scores, max_discrete)
  errors <- character(0); warnings <- character(0)

  const <- names(ty)[ty == "constant"]
  if (length(const)) {
    errors <- c(errors, paste0(
      "These constructs have no variance and cannot enter the search: ",
      paste(const, collapse = ", "), "."))
  }

  bin_targets <- intersect(targets, names(ty)[ty == "binary"])
  if (length(bin_targets)) {
    warnings <- c(warnings, paste0(
      "Binary outcome(s) selected: ", paste(bin_targets, collapse = ", "),
      ". Structure learning treats these as continuous, so the reported RMSE ",
      "is a linear probability approximation rather than a proper fit. ",
      "Consider running discovery on the continuous constructs only and ",
      "modelling the binary outcome separately \u2014 the non-linearity tab ",
      "fits it with a binomial GAM automatically."))
  }

  bin_other <- setdiff(names(ty)[ty == "binary"], bin_targets)
  if (length(bin_other)) {
    warnings <- c(warnings, paste0(
      "Binary variable(s) in the search: ", paste(bin_other, collapse = ", "),
      ". As predictors this is a reasonable approximation; as outcomes it is ",
      "not."))
  }

  # Near-perfectly correlated constructs make the correlation matrix singular,
  # and the conditional independence tests fail in ways that are hard to read.
  num <- as.data.frame(scores)[, ty != "constant", drop = FALSE]
  if (ncol(num) >= 2L) {
    R <- suppressWarnings(stats::cor(num, use = "pairwise.complete.obs"))
    R[lower.tri(R, diag = TRUE)] <- NA
    idx <- which(abs(R) > 0.99 & !is.na(R), arr.ind = TRUE)
    if (nrow(idx)) {
      pairs <- apply(idx, 1, function(r)
        paste(rownames(R)[r[1]], "/", colnames(R)[r[2]]))
      errors <- c(errors, paste0(
        "These constructs are almost perfectly correlated: ",
        paste(pairs, collapse = "; "),
        ". One of each pair has to go. A common cause is keeping a categorical ",
        "variable alongside the dummy variables coded from it."))
    }
  }

  few <- names(ty)[ty == "few-valued"]
  if (length(few)) {
    warnings <- c(warnings, paste0(
      "These take only a handful of distinct values: ",
      paste(few, collapse = ", "),
      ". Ordered categories such as age bands are usually acceptable. ",
      "Unordered categories are not, and should be dummy-coded before ",
      "import."))
  }

  list(errors = errors, warnings = warnings)
}

# ---------------------------------------------------------------------------
# Structure learning
# ---------------------------------------------------------------------------

#' Learn a network structure from latent variable scores
#'
#' @param scores Data frame of latent variable scores.
#' @param algorithm "gs" (constraint-based) or "hc" (score-based).
#' @param blacklist Data frame of forbidden arcs, or NULL.
#' @param alpha Significance level for the conditional independence tests (gs).
#' @param R Bootstrap replicates for hc.
#' @param threshold Arc strength threshold for the averaged hc network.
#' @param seed Random seed.
#' @return A list with `dag`, `algorithm`, `bic`, and, for hc, `strength`.
#' @export
learn_structure <- function(scores,
                            algorithm = c("gs", "hc"),
                            blacklist = NULL,
                            alpha = 0.05,
                            R = 1000,
                            threshold = NULL,
                            seed = 123) {
  algorithm <- match.arg(algorithm)
  dat <- as.data.frame(scores)
  strength <- NULL

  if (algorithm == "gs") {
    cpdag <- bnlearn::gs(dat, test = "cor", blacklist = blacklist,
                         alpha = alpha)
    # Grow-Shrink returns a partially directed graph. cextend() derives a
    # consistent fully-directed extension, which is required before the network
    # can be fitted or scored. Setting individual arcs by hand instead makes the
    # resulting structure depend on an undocumented analyst choice.
    dag <- tryCatch(bnlearn::cextend(cpdag), error = function(e) {
      stop("Grow-Shrink found a structure that cannot be oriented into a ",
           "directed graph without creating a cycle, so it cannot be fitted. ",
           "This usually means the search has too little data for the number ",
           "of constructs, or too few constraints. Try blocking more arcs, ",
           "removing constructs that are not part of your theoretical model, ",
           "or using Hill-Climbing instead. (Underlying message: ",
           conditionMessage(e), ")", call. = FALSE)
    })
  } else {
    set.seed(seed)
    strength <- bnlearn::boot.strength(
      dat, R = R, algorithm = "hc",
      algorithm.args = list(score = "bic-g", blacklist = blacklist)
    )
    dag <- if (is.null(threshold)) {
      bnlearn::averaged.network(strength)
    } else {
      bnlearn::averaged.network(strength, threshold = threshold)
    }
    if (length(bnlearn::undirected.arcs(dag))) dag <- bnlearn::cextend(dag)
  }

  list(
    dag       = dag,
    algorithm = algorithm,
    # bnlearn's bic-g is a penalised log-likelihood: HIGHER is better.
    bic       = bnlearn::score(dag, dat, type = "bic-g"),
    strength  = strength
  )
}

# ---------------------------------------------------------------------------
# Cross-validated predictive error
# ---------------------------------------------------------------------------

#' Out-of-sample RMSE for a learned network
#'
#' CORRECTION 1 (Appendix E). The published code evaluated the Grow-Shrink
#' model with `lm(target ~ .)`, an ordinary regression on every variable in the
#' data, while evaluating Hill-Climbing with the fitted network's own parent
#' set. Regressing a variable on its own descendants leaks information about it
#' and understates the error, and the size of the leak grows with the number of
#' descendants a target has. This function applies one identical procedure to
#' both algorithms: relearn the structure on the training folds, fit it, and
#' predict each target from its parents in that network.
#'
#' CORRECTION 2 (Appendix E). The published Hill-Climbing loop compared
#' predictions of `reasonsf` against the observed values of `attitude`. Each
#' target is now scored against itself.
#'
#' The structure is relearned inside every fold rather than learned once on all
#' the data, so the reported error includes the cost of structure selection.
#'
#' @param scores Data frame of latent variable scores.
#' @param targets Character vector of nodes to predict.
#' @param algorithm "gs" or "hc".
#' @param blacklist Forbidden arcs, or NULL.
#' @param k Number of folds.
#' @param seed Random seed.
#' @param relearn Relearn the structure within each fold. Default TRUE.
#' @param dag A fitted DAG to reuse when `relearn = FALSE`.
#' @return A named numeric vector of RMSE values.
#' @export
cv_network_rmse <- function(scores, targets,
                            algorithm = c("gs", "hc"),
                            blacklist = NULL,
                            k = 10, seed = 123,
                            relearn = TRUE, dag = NULL) {
  algorithm <- match.arg(algorithm)
  dat <- as.data.frame(scores)
  if (!relearn && is.null(dag)) {
    stop("Supply `dag` when `relearn = FALSE`.")
  }

  set.seed(seed)
  folds <- caret::createFolds(seq_len(nrow(dat)), k = k,
                              list = TRUE, returnTrain = TRUE)

  out <- matrix(NA_real_, nrow = k, ncol = length(targets),
                dimnames = list(NULL, targets))

  failed <- 0L
  reasons <- character(0)

  for (i in seq_along(folds)) {
    train <- dat[folds[[i]], , drop = FALSE]
    test  <- dat[-folds[[i]], , drop = FALSE]

    # A fold can fail without the analysis being wrong. Grow-Shrink returns a
    # partially directed graph, and on a subsample that graph sometimes cannot
    # be oriented without creating a cycle, so cextend() refuses. Skip that
    # fold, count it, and average over the rest.
    stru <- tryCatch({
      if (!relearn) dag
      else if (algorithm == "gs")
        bnlearn::cextend(bnlearn::gs(train, test = "cor",
                                     blacklist = blacklist, alpha = 0.05))
      else
        bnlearn::hc(train, score = "bic-g", blacklist = blacklist)
    }, error = function(e) {
      reasons <<- c(reasons, conditionMessage(e))
      NULL
    })
    if (is.null(stru)) { failed <- failed + 1L; next }

    fit <- tryCatch(bnlearn::bn.fit(stru, train), error = function(e) {
      reasons <<- c(reasons, conditionMessage(e)); NULL
    })
    if (is.null(fit)) { failed <- failed + 1L; next }

    for (tg in targets) {
      pred <- tryCatch(stats::predict(fit, node = tg, data = test),
                       error = function(e) NULL)
      # Each target scored against itself.
      if (!is.null(pred)) {
        out[i, tg] <- sqrt(mean((test[[tg]] - pred)^2, na.rm = TRUE))
      }
    }
  }

  if (failed == k) {
    stop(sprintf(
      paste("%s could not be fitted on any of the %d cross-validation folds,",
            "even though it produced a structure on the full sample. The",
            "structure is therefore not stable under resampling, and no",
            "honest out-of-sample error can be reported for it. This is",
            "usually a sign that there are too many constructs for the sample",
            "size. Use Hill-Climbing, which optimises a score rather than",
            "chaining independence tests, or reduce the number of constructs.",
            "(First message: %s)"),
      if (algorithm == "gs") "Grow-Shrink" else "Hill-Climbing", k,
      if (length(reasons)) reasons[1] else "none reported"), call. = FALSE)
  }

  res <- colMeans(out, na.rm = TRUE)
  attr(res, "folds_used")   <- k - failed
  attr(res, "folds_failed") <- failed

  if (failed > 0L) {
    warning(sprintf(
      "%d of %d folds could not be fitted and were skipped. Error is averaged over %d folds.",
      failed, k, k - failed), call. = FALSE)
  }
  res
}

#' Out-of-sample RMSE and MAE for a linear model on named predictors
#' @param scores Data frame.
#' @param target Outcome name.
#' @param predictors Character vector of predictor names.
#' @param k Folds.
#' @param seed Random seed.
#' @return Named numeric vector of RMSE, MAE and cross-validated R squared.
#' @export
cv_lm_rmse <- function(scores, target, predictors, k = 10, seed = 123) {
  dat <- as.data.frame(scores)
  set.seed(seed)
  folds <- caret::createFolds(seq_len(nrow(dat)), k = k,
                              list = TRUE, returnTrain = TRUE)
  mse <- mae <- r2 <- numeric(k)
  for (i in seq_len(k)) {
    train <- dat[folds[[i]], , drop = FALSE]
    test  <- dat[-folds[[i]], , drop = FALSE]
    fit  <- stats::lm(stats::reformulate(predictors, target), data = train)
    pred <- stats::predict(fit, newdata = test)
    err  <- test[[target]] - pred
    mse[i] <- mean(err^2, na.rm = TRUE)
    mae[i] <- mean(abs(err), na.rm = TRUE)
    r2[i]  <- 1 - mse[i] / mean((test[[target]] - mean(test[[target]]))^2)
  }
  c(RMSE = sqrt(mean(mse)), MAE = mean(mae), cv_R2 = mean(r2))
}

#' Compare two learned structures on the same footing
#' @param scores Latent variable scores.
#' @param targets Nodes to predict.
#' @param blacklist Forbidden arcs.
#' @param k Folds.
#' @param seed Random seed.
#' @param hc_threshold Arc strength threshold for hc.
#' @param R Bootstrap replicates for hc.
#' @return A list with `table`, `gs`, and `hc`.
#' @export
compare_algorithms <- function(scores, targets, blacklist = NULL,
                               k = 10, seed = 123,
                               hc_threshold = NULL, R = 1000) {
  gs <- learn_structure(scores, "gs", blacklist = blacklist, seed = seed)
  hc <- learn_structure(scores, "hc", blacklist = blacklist,
                        R = R, threshold = hc_threshold, seed = seed)

  rmse_gs <- cv_network_rmse(scores, targets, "gs", blacklist, k, seed)
  rmse_hc <- cv_network_rmse(scores, targets, "hc", blacklist, k, seed)

  tab <- data.frame(
    target  = targets,
    GS_RMSE = as.numeric(rmse_gs[targets]),
    HC_RMSE = as.numeric(rmse_hc[targets]),
    stringsAsFactors = FALSE
  )
  attr(tab, "bic") <- c(GS = gs$bic, HC = hc$bic)

  list(table = tab, gs = gs, hc = hc)
}

# ---------------------------------------------------------------------------
# Reading a DAG
# ---------------------------------------------------------------------------

#' The regression equations implied by a learned DAG
#' @param dag A bnlearn DAG.
#' @return A named list mapping each node with parents to its parent set.
#' @export
dag_equations <- function(dag) {
  nodes <- bnlearn::nodes(dag)
  eqs <- lapply(nodes, function(n) bnlearn::parents(dag, n))
  names(eqs) <- nodes
  eqs[vapply(eqs, length, integer(1)) > 0L]
}

#' Arcs present in a learned DAG but absent from the baseline model
#' @param dag A bnlearn DAG.
#' @param baseline_paths Data frame with `from` and `to`.
#' @return A data frame of newly discovered arcs.
#' @export
new_arcs <- function(dag, baseline_paths) {
  arcs <- as.data.frame(bnlearn::arcs(dag), stringsAsFactors = FALSE)
  if (!nrow(arcs)) return(arcs)
  known <- paste(baseline_paths$from, baseline_paths$to, sep = "->")
  arcs[!paste(arcs$from, arcs$to, sep = "->") %in% known, , drop = FALSE]
}

#' Plot a DAG with one layer per causal depth
#'
#' The layered layouts in igraph place every node of a layer at the same
#' height, but leave the horizontal spacing to the algorithm, which packs
#' nodes on top of one another once a layer holds more than a few. Positions
#' are therefore computed directly here: nodes are assigned to a layer by their
#' longest path from a root, then spread evenly across the width of that layer.
#'
#' @param dag A bnlearn DAG.
#' @param main Plot title.
#' @param show_isolated Draw constructs that have no arcs. They are placed in
#'   their own rows beneath the structure.
#' @param use_graphviz Use Rgraphviz if it is installed.
#' @export
plot_dag <- function(dag, main = "", show_isolated = TRUE,
                     use_graphviz = FALSE) {
  if (use_graphviz && requireNamespace("Rgraphviz", quietly = TRUE)) {
    return(bnlearn::graphviz.plot(dag, main = main, layout = "dot"))
  }

  g     <- bnlearn::as.igraph(dag)
  nodes <- igraph::V(g)$name
  iso   <- nodes[igraph::degree(g) == 0]
  con   <- setdiff(nodes, iso)

  if (!show_isolated) {
    if (!length(con)) {
      graphics::plot.new()
      graphics::title(main = main)
      graphics::text(0.5, 0.5, "No arcs were found.", col = "#6A7169")
      return(invisible(NULL))
    }
    g     <- igraph::induced_subgraph(g, con)
    nodes <- con
    iso   <- character(0)
  }

  # --- depth of each connected node ----------------------------------------
  layer <- stats::setNames(integer(length(nodes)), nodes)
  if (length(con)) {
    sub <- igraph::induced_subgraph(g, con)
    ord <- tryCatch(igraph::V(sub)$name[igraph::topo_sort(sub, mode = "out")],
                    error = function(e) igraph::V(sub)$name)
    for (v in ord) {
      pa <- tryCatch(igraph::neighbors(sub, v, mode = "in")$name,
                     error = function(e) character(0))
      if (length(pa)) layer[v] <- max(layer[pa]) + 1L
    }
  }

  # --- isolated constructs go in rows of their own, wrapped ----------------
  if (length(iso)) {
    base <- if (length(con)) max(layer[con]) + 1L else 0L
    per_row <- 6L
    layer[iso] <- base + ((seq_along(iso) - 1L) %/% per_row)
  }

  # --- spread each row evenly ----------------------------------------------
  rows  <- split(names(layer), layer)
  n_row <- length(rows)
  wide  <- max(vapply(rows, length, integer(1)))

  # One spacing for the whole plot. Spreading every row across the full width
  # regardless of how many nodes it holds pushes a row of two to opposite
  # edges, which is what made the circles grow to fill the gap.
  step_x <- min(0.55, 1.7 / max(1L, wide  - 1L))
  step_y <- min(0.55, 1.7 / max(1L, n_row - 1L))

  coords <- matrix(0, nrow = length(nodes), ncol = 2,
                   dimnames = list(nodes, NULL))
  for (i in seq_along(rows)) {
    members <- rows[[i]]
    m    <- length(members)
    half <- min(0.85, step_x * (m - 1L) / 2)
    xs   <- if (m == 1L) 0 else seq(-half, half, length.out = m)
    ys   <- if (n_row == 1L) 0 else 0.85 - step_y * (i - 1L)
    coords[members, 1] <- xs
    coords[members, 2] <- ys
  }
  coords <- coords[igraph::V(g)$name, , drop = FALSE]

  # A circle of vertex.size s has a diameter of roughly s/100 in these
  # coordinates, so keep it below half the tighter of the two spacings.
  vsize <- max(4, min(13, 45 * min(step_x, step_y)))
  lcex  <- max(0.5, min(0.85, 0.85 * vsize / 13))

  op <- graphics::par(mar = c(0.5, 0.5, if (nzchar(main)) 2 else 0.5, 0.5))
  on.exit(graphics::par(op), add = TRUE)

  igraph::plot.igraph(
    g, main = main, layout = coords, rescale = FALSE, asp = 1,
    xlim = c(-1, 1), ylim = c(-1, 1),
    vertex.shape = "circle", vertex.color = "#FFFFFF",
    vertex.frame.color = "#5A6360", vertex.size = vsize,
    vertex.label.color = "#16191D", vertex.label.cex = lcex,
    vertex.label.family = "sans",
    edge.arrow.size = 0.3, edge.color = "#2D5F5D",
    edge.width = 1, edge.curved = 0.1
  )

  if (length(iso)) {
    graphics::mtext("Constructs with no arcs are shown in the lower rows.",
                    side = 1, line = -0.5, cex = 0.7, col = "#6A7169")
  }
  invisible(coords)
}

