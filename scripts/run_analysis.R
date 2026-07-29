# =============================================================================
# PredictLearn - run the full analysis from the console
#
# Use this when the interface gets in the way. It does everything the app does,
# using the same tested functions, and prints a real traceback when something
# fails instead of swallowing the error.
#
# HOW TO RUN
#   1. In the app: Export tab -> Download latent variable scores
#   2. Edit the SETTINGS block below
#   3. devtools::load_all(".")
#   4. source("run_analysis.R")
# =============================================================================

## ---- SETTINGS ---------------------------------------------------------------

scores_file <- "~/Downloads/latent_variable_scores.csv"

# Nothing may cause these: demographics, stable traits, randomised conditions.
exogenous <- c("SEC_exp", "SEC_cred", "AGE", "GEN", "EDU", "INC",
               "HS", "FAMI", "FRE", "AIU")

# Constructs to report predictive error for.
targets <- c("DV", "PU", "PD", "CE")

# Paths your theory already tested, as from -> to. The reverse of each is
# forbidden, so the search cannot simply re-propose your own model backwards.
# Leave empty to skip.
hypothesised <- data.frame(
  from = character(0),
  to   = character(0),
  stringsAsFactors = FALSE
)
# Example:
# hypothesised <- data.frame(
#   from = c("PD", "AT"),
#   to   = c("AT", "DV"),
#   stringsAsFactors = FALSE)

seed <- 123

## ---- LOAD -------------------------------------------------------------------

scores <- read.csv(scores_file)
cat("\nLoaded", nrow(scores), "rows and", ncol(scores), "constructs:\n")
cat(" ", paste(names(scores), collapse = ", "), "\n\n")

missing_exo <- setdiff(exogenous, names(scores))
if (length(missing_exo)) {
  cat("NOTE: not found in the data, ignoring: ",
      paste(missing_exo, collapse = ", "), "\n")
  exogenous <- intersect(exogenous, names(scores))
}
targets <- intersect(targets, names(scores))

## ---- 1. IS THE DATA FIT FOR A SEARCH? --------------------------------------

cat("========== 1. Collinearity ==========\n\n")
col <- collinearity_diagnostics(scores)
print(col$vif)
if (!is.null(col$pairs)) { cat("\nHighly correlated pairs:\n"); print(col$pairs) }
if (length(col$flags)) {
  cat("\n")
  for (f in col$flags) cat("! ", f, "\n\n")
}

cat("\n========== 2. Variable types ==========\n\n")
print(variable_types(scores))
chk <- check_discovery_variables(scores, targets)
for (e in chk$errors)   cat("\nERROR:   ", e, "\n")
for (w in chk$warnings) cat("\nWARNING: ", w, "\n")

if (length(chk$errors)) {
  stop("\nFix the errors above before searching. Usually this means dropping ",
       "a redundant construct from the mapping and re-estimating.")
}

## ---- 2. CONSTRAINTS ---------------------------------------------------------

cat("\n\n========== 3. Constraints ==========\n\n")

blacklist <- default_blacklist(
  paths          = hypothesised,
  exogenous      = exogenous,
  all_constructs = names(scores)
)

cat("Blocked", nrow(blacklist), "of",
    ncol(scores) * (ncol(scores) - 1), "possible arcs.\n")
cat("Nothing may cause: ", paste(exogenous, collapse = ", "), "\n")

## ---- 3. SEARCH --------------------------------------------------------------

cat("\n\n========== 4. Grow-Shrink ==========\n\n")

gs <- tryCatch(
  learn_structure(scores, "gs", blacklist = blacklist, seed = seed),
  error = function(e) {
    cat("Grow-Shrink failed.\n")
    cat("Message: '", conditionMessage(e), "'\n", sep = "")
    cat("\nThis is common when the number of constructs is large relative to\n")
    cat("the sample. Hill-Climbing below optimises a score instead of chaining\n")
    cat("independence tests, and usually survives where Grow-Shrink does not.\n")
    NULL
  })

if (!is.null(gs)) {
  cat("Arcs found:\n"); print(bnlearn::arcs(gs$dag))
  cat("\nBIC (higher is better):", round(gs$bic, 3), "\n")
}

cat("\n\n========== 5. Hill-Climbing ==========\n\n")

hc <- tryCatch(
  learn_structure(scores, "hc", blacklist = blacklist,
                  R = 300, threshold = 0.8, seed = seed),
  error = function(e) {
    cat("Hill-Climbing failed.\n")
    cat("Message: '", conditionMessage(e), "'\n", sep = "")
    NULL
  })

if (!is.null(hc)) {
  cat("Arcs found:\n"); print(bnlearn::arcs(hc$dag))
  cat("\nBIC (higher is better):", round(hc$bic, 3), "\n")
}

## ---- 4. PREDICTIVE COMPARISON ----------------------------------------------

chosen <- if (!is.null(gs)) gs else hc
algo   <- if (!is.null(gs)) "gs" else "hc"

if (is.null(chosen)) {
  stop("\nBoth algorithms failed. Reduce the number of constructs: clear the ",
       "ones that are not part of your theoretical model from the mapping and ",
       "estimate again. 276 respondents will not support a search over 17 ",
       "constructs.")
}

if (length(targets)) {
  cat("\n\n========== 6. Out-of-sample error ==========\n\n")
  cat("Both algorithms evaluated by one identical routine: the structure is\n")
  cat("relearned on each training fold, fitted, and each target predicted from\n")
  cat("its parents in that network.\n\n")

  rmse <- tryCatch({
    tab <- data.frame(target = targets, stringsAsFactors = FALSE)
    if (!is.null(gs)) tab$GS_RMSE <- round(as.numeric(
      cv_network_rmse(scores, targets, "gs", blacklist, seed = seed)[targets]), 3)
    if (!is.null(hc)) tab$HC_RMSE <- round(as.numeric(
      cv_network_rmse(scores, targets, "hc", blacklist, seed = seed)[targets]), 3)
    tab
  }, error = function(e) { cat("Failed: ", conditionMessage(e), "\n"); NULL })

  if (!is.null(rmse)) {
    print(rmse)
    cat("\nScores are standardised, so predicting with the mean gives RMSE = 1.\n")
    cat("Anything at or above 1 predicts no better than nothing. Anything below\n")
    cat("about 0.2 would indicate leakage of the kind this package corrects.\n")
  }
}

## ---- 5. NON-LINEARITY -------------------------------------------------------

eqs <- dag_equations(chosen$dag)

if (length(eqs)) {
  cat("\n\n========== 7. Non-linearity ==========\n\n")
  cat("Equations implied by the selected structure:\n")
  for (nm in names(eqs)) {
    cat("  ", nm, " <- ", paste(eqs[[nm]], collapse = " + "), "\n", sep = "")
  }

  cat("\nMARS:\n")
  mars <- tryCatch(fit_mars(scores, eqs, seed = seed),
                   error = function(e) { cat("Failed: ", conditionMessage(e), "\n"); NULL })
  if (!is.null(mars)) print(summarise_mars(mars))

  cat("\nGAM:\n")
  gam <- tryCatch(fit_gam(scores, eqs),
                  error = function(e) { cat("Failed: ", conditionMessage(e), "\n"); NULL })
  if (!is.null(gam)) {
    print(summarise_gam(gam))
    cat("\nDoes the curvature earn its keep?\n")
    print(test_nonlinearity(scores, gam))

    cat("\nOut-of-sample performance:\n")
    for (nm in names(gam)) {
      m <- tryCatch(cv_gam(scores, stats::formula(gam[[nm]]), seed = seed),
                    error = function(e) NULL)
      if (!is.null(m)) {
        cat("  ", nm, " (", attr(m, "family"), "): ",
            paste(sprintf("%s = %.3f", names(m), m), collapse = ", "),
            "\n", sep = "")
      }
    }
  }
} else {
  cat("\n\nThe selected structure has no equations to probe: no construct ",
      "ended up with parents.\n")
}

cat("\n\n========== Done ==========\n")
cat("Objects left in your workspace: scores, blacklist, gs, hc, eqs, mars, gam\n")
