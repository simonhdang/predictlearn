# =============================================================================
# PredictLearn self-test
#
# Runs every stage of the pipeline on generated data with a KNOWN structure,
# so you can tell whether the engine works before pointing it at real data.
#
# HOW TO RUN
#   1. Open the predictlearn folder in RStudio
#   2. devtools::load_all(".")
#   3. source("tests/smoke_test.R")
#
# Each stage prints PASS or FAIL. Stop at the first FAIL and send me the
# message printed underneath it.
# =============================================================================

cat("\n================ PredictLearn self-test ================\n\n")

pass <- 0L; fail <- 0L
check <- function(label, expr) {
  res <- tryCatch(expr, error = function(e) e)
  if (inherits(res, "error")) {
    cat(sprintf("  FAIL  %s\n        %s\n", label, conditionMessage(res)))
    fail <<- fail + 1L
    return(invisible(NULL))
  }
  cat(sprintf("  PASS  %s\n", label))
  pass <<- pass + 1L
  invisible(res)
}

# ---------------------------------------------------------------------------
# 0. Dependencies
# ---------------------------------------------------------------------------
cat("[0] Dependencies\n")
for (p in c("seminr", "bnlearn", "earth", "mgcv", "caret", "igraph",
            "shiny", "DT", "readxl")) {
  check(paste0("package '", p, "' is installed"),
        if (!requireNamespace(p, quietly = TRUE))
          stop("not installed -- run install.packages('", p, "')") else TRUE)
}

# ---------------------------------------------------------------------------
# 1. Generate data with a known structure
#
#    TRUE MODEL:   PU --> ATT --> INT
#                  PE --> ATT
#                  EXP -------> INT
# ---------------------------------------------------------------------------
cat("\n[1] Generate test data\n")

set.seed(42)
n <- 400

make_items <- function(latent, n_items, loading = 0.80, prefix) {
  m <- sapply(seq_len(n_items), function(i) {
    x <- loading * latent + rnorm(length(latent), sd = sqrt(1 - loading^2))
    round(pmin(pmax(3 + x, 1), 5))          # 1-5 Likert
  })
  colnames(m) <- paste0(prefix, seq_len(n_items))
  m
}

PU  <- rnorm(n)
PE  <- 0.30 * PU + rnorm(n, sd = 0.95)
EXP <- rbinom(n, 1, 0.6)
ATT <- 0.55 * PU + 0.30 * PE + rnorm(n, sd = 0.70)
INT <- 0.60 * ATT + 0.20 * EXP + rnorm(n, sd = 0.70)

raw <- data.frame(
  make_items(PU,  4, prefix = "PU"),
  make_items(PE,  4, prefix = "PE"),
  make_items(ATT, 3, prefix = "AT"),
  make_items(INT, 3, prefix = "IN"),
  EXP = EXP
)

check("data frame built", {
  stopifnot(nrow(raw) == n, ncol(raw) == 15); TRUE
})

tmp_csv <- file.path(tempdir(), "predictlearn_test.csv")
write.csv(raw, tmp_csv, row.names = FALSE)
check("read_indicator_data() reads a CSV", {
  d <- read_indicator_data(tmp_csv)
  stopifnot(nrow(d) == n); TRUE
})

# ---------------------------------------------------------------------------
# 2. Measurement model
# ---------------------------------------------------------------------------
cat("\n[2] Measurement model\n")

mapping <- data.frame(
  construct = c(rep("PU", 4), rep("PE", 4), rep("ATT", 3),
                rep("INT", 3), "EXP"),
  indicator = c(paste0("PU", 1:4), paste0("PE", 1:4), paste0("AT", 1:3),
                paste0("IN", 1:3), "EXP"),
  mode      = "A",
  stringsAsFactors = FALSE
)

check("validate_mapping() accepts a clean mapping", validate_mapping(mapping))

check("validate_mapping() rejects a duplicated indicator", {
  bad <- rbind(mapping, mapping[1, ])
  r <- tryCatch(validate_mapping(bad), error = function(e) "caught")
  if (!identical(r, "caught")) stop("duplicate indicator was not rejected")
  TRUE
})

mm <- check("build_measurement_model() builds a model",
            build_measurement_model(mapping))

sm <- check("saturated_structural_model() builds a saturated model",
            saturated_structural_model(c("PU", "PE", "ATT", "INT", "EXP")))

fit <- check("extract_scores() estimates and returns scores", {
  f <- extract_scores(raw, mm)
  stopifnot(nrow(f$scores) == n, ncol(f$scores) >= 5)
  f
})

if (!is.null(fit)) {
  scores <- fit$scores
  cat("        constructs recovered: ", paste(names(scores), collapse = ", "), "\n")

  check("scores are standardised", {
    sds <- sapply(scores, sd)
    if (any(abs(sds - 1) > 0.01)) stop("not unit variance")
    TRUE
  })

  check("scores recover the known correlations", {
    r <- cor(scores$ATT, scores$INT)
    if (abs(r) < 0.30) stop(sprintf("ATT-INT correlation is only %.2f", r))
    cat(sprintf("        cor(ATT, INT) = %.2f\n", r))
    TRUE
  })

  a <- check("assess_measurement() returns reliability tables", {
    x <- assess_measurement(fit$model)
    if (is.null(x$reliability)) stop("reliability table came back NULL")
    print(round(x$reliability, 3))
    if (length(x$flags)) { cat("        flags:\n");
      for (f in x$flags) cat("         -", f, "\n") }
    x
  })
}

# ---------------------------------------------------------------------------
# 3. Higher-order diagnostics
# ---------------------------------------------------------------------------
cat("\n[3] Higher-order diagnostics\n")

if (exists("scores")) {
  check("diagnose_hoc() runs on a candidate pair", {
    d <- diagnose_hoc(scores, c("PU", "PE"), htmt = if (exists("a")) a$htmt else NULL)
    print(d); TRUE
  })
  check("suggest_hoc_groups() returns groupings", {
    g <- suggest_hoc_groups(scores, k = 2, exclude = "EXP")
    print(g); TRUE
  })
  check("build_measurement_model() accepts a higher-order construct", {
    build_measurement_model(mapping, higher_order = list(RF = c("PU", "PE")))
  })
}

# ---------------------------------------------------------------------------
# 4. Structure discovery
# ---------------------------------------------------------------------------
cat("\n[4] Structure discovery\n")

if (exists("scores")) {
  blocked <- matrix(FALSE, ncol(scores), ncol(scores),
                    dimnames = list(names(scores), names(scores)))
  blocked[setdiff(names(scores), "EXP"), "EXP"] <- TRUE   # nothing causes EXP
  bl <- check("blacklist_from_matrix() converts the grid",
              blacklist_from_matrix(blocked))

  gs <- check("learn_structure() runs Grow-Shrink", {
    s <- learn_structure(scores, "gs", blacklist = bl)
    cat("        arcs found:\n"); print(bnlearn::arcs(s$dag))
    cat(sprintf("        BIC = %.2f\n", s$bic))
    s
  })

  hc <- check("learn_structure() runs Hill-Climbing", {
    s <- learn_structure(scores, "hc", blacklist = bl, R = 100, threshold = 0.80)
    cat(sprintf("        BIC = %.2f\n", s$bic))
    s
  })

  check("cv_network_rmse() returns errors on the right scale", {
    r <- cv_network_rmse(scores, c("ATT", "INT"), "gs", bl, k = 5)
    print(round(r, 3))
    # Constructs are standardised, so predicting with the mean gives RMSE = 1.
    # Any value at or above 1 means the network predicts no better than nothing;
    # any value near 0 would signal a leak of the kind corrected in this package.
    if (any(r > 1.05)) stop("RMSE above 1 -- worse than the mean")
    if (any(r < 0.20)) stop("RMSE implausibly low -- check for leakage")
    TRUE
  })

  if (!is.null(gs)) {
    check("plot_dag() draws without Rgraphviz", {
      pdf(NULL); plot_dag(gs$dag, main = "test"); dev.off(); TRUE
    })
    eqs <- check("dag_equations() extracts the regression equations", {
      e <- dag_equations(gs$dag)
      for (nm in names(e)) cat("        ", nm, " <- ",
                               paste(e[[nm]], collapse = " + "), "\n", sep = "")
      e
    })
  }
}

# ---------------------------------------------------------------------------
# 5. Non-linearity
# ---------------------------------------------------------------------------
cat("\n[5] Non-linearity\n")

if (exists("eqs") && length(eqs)) {
  mars <- check("fit_mars() fits MARS models",
                fit_mars(scores, eqs, max_degree = 2, k = 5))
  if (!is.null(mars)) check("summarise_mars() tabulates", print(summarise_mars(mars)))

  gam <- check("fit_gam() fits GAM models", fit_gam(scores, eqs))
  if (!is.null(gam)) {
    check("summarise_gam() tabulates", print(summarise_gam(gam)))
    check("test_nonlinearity() compares against a linear fit",
          print(test_nonlinearity(scores, gam)))
  }

  check("cv_gam() returns out-of-sample error", {
    r <- cv_gam(scores, ATT ~ s(PU) + s(PE), k = 5)
    print(round(r, 3))
    # The data were generated linearly, so a GAM should not beat RMSE ~ 0.5.
    if (r[["RMSE"]] < 0.20) stop("RMSE implausibly low -- check the fold split")
    TRUE
  })
}

# ---------------------------------------------------------------------------
# 6. Export
# ---------------------------------------------------------------------------
cat("\n[6] Export\n")

if (exists("bl")) {
  check("export_script() writes a runnable script", {
    p <- file.path(tempdir(), "exported.R")
    export_script(p, tmp_csv, mapping, blacklist = bl,
                  algorithm = "gs", targets = c("ATT", "INT"))
    parse(p)               # will error if the generated script is not valid R
    cat("        written to ", p, "\n", sep = "")
    TRUE
  })
}

# ---------------------------------------------------------------------------
cat("\n================================================\n")
cat(sprintf("  %d passed, %d failed\n", pass, fail))
if (fail == 0L) {
  cat("\n  Engine looks sound. Try run_app() next.\n")
} else {
  cat("\n  Send me the FAIL messages above and I'll fix them.\n")
}
cat("================================================\n\n")
