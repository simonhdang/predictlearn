#' Reproducible script export and app launcher

#' Write a standalone R script reproducing an analysis
#'
#' A graphical tool that cannot show its working is not usable in a manuscript.
#' This writes out a plain script that reproduces the same results without
#' PredictLearn, so that the analysis can be audited, archived, and rerun.
#'
#' @param path Where to write the script.
#' @param data_file Path the script should read data from.
#' @param mapping The indicator-to-construct mapping.
#' @param higher_order Named list of higher-order constructs, or NULL.
#' @param blacklist Data frame of forbidden arcs, or NULL.
#' @param algorithm "gs" or "hc".
#' @param targets Nodes evaluated for predictive error.
#' @param seed Random seed used.
#' @return The path, invisibly.
#' @export
export_script <- function(path, data_file, mapping, higher_order = NULL,
                          blacklist = NULL, algorithm = "gs",
                          targets = character(0), seed = 123) {

  df_lit <- function(df) {
    if (is.null(df) || !nrow(df)) return("NULL")
    paste0(
      "data.frame(\n  from = c(",
      paste(sprintf('"%s"', df$from), collapse = ", "),
      "),\n  to   = c(",
      paste(sprintf('"%s"', df$to), collapse = ", "),
      "),\n  stringsAsFactors = FALSE\n)"
    )
  }

  map_lit <- paste0(
    "data.frame(\n  construct = c(",
    paste(sprintf('"%s"', mapping$construct), collapse = ", "),
    "),\n  indicator = c(",
    paste(sprintf('"%s"', mapping$indicator), collapse = ", "),
    "),\n  mode      = c(",
    paste(sprintf('"%s"', mapping$mode), collapse = ", "),
    "),\n  stringsAsFactors = FALSE\n)"
  )

  hoc_lit <- if (is.null(higher_order) || !length(higher_order)) {
    "NULL"
  } else {
    paste0("list(\n",
           paste(vapply(names(higher_order), function(n) {
             paste0("  ", n, " = c(",
                    paste(sprintf('"%s"', higher_order[[n]]), collapse = ", "),
                    ")")
           }, character(1)), collapse = ",\n"),
           "\n)")
  }

  lines <- c(
    "# Reproducible analysis exported from PredictLearn",
    paste0("# Generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "#",
    "# This script reproduces the analysis without the graphical interface.",
    "# Package versions are recorded by sessionInfo() at the end.",
    "",
    "library(predictlearn)",
    "",
    paste0('data_raw <- read_indicator_data("', data_file, '")'),
    "",
    "mapping <- ", map_lit,
    "",
    "higher_order <- ", hoc_lit,
    "",
    "mm <- build_measurement_model(mapping, higher_order = higher_order)",
    "",
    "# Scores are extracted from a saturated model under the factor weighting",
    "# scheme, so they do not encode the structure being tested.",
    "fit    <- extract_scores(data_raw, mm)",
    "scores <- fit$scores",
    "",
    "# Measurement quality. Scores from an unassessed measurement model are",
    "# not interpretable.",
    "assessment <- assess_measurement(fit$model)",
    "print(assessment$reliability)",
    "print(assessment$flags)",
    "",
    "# Theory enters here: arcs the search is forbidden to propose.",
    "blacklist <- ", df_lit(blacklist),
    "",
    paste0('structure_fit <- learn_structure(scores, algorithm = "',
           algorithm, '", blacklist = blacklist, seed = ', seed, ")"),
    "plot_dag(structure_fit$dag)",
    'cat("BIC (higher is better):", structure_fit$bic, "\\n")',
    "",
    if (length(targets)) c(
      paste0("targets <- c(",
             paste(sprintf('"%s"', targets), collapse = ", "), ")"),
      paste0('cv_network_rmse(scores, targets, algorithm = "', algorithm,
             '", blacklist = blacklist, seed = ', seed, ")"),
      ""
    ),
    "equations <- dag_equations(structure_fit$dag)",
    "mars_fits <- fit_mars(scores, equations, seed = ", seed, ")",
    "gam_fits  <- fit_gam(scores, equations)",
    "",
    "summarise_mars(mars_fits)",
    "summarise_gam(gam_fits)",
    "test_nonlinearity(scores, gam_fits)",
    "",
    "sessionInfo()"
  )

  writeLines(unlist(lines), path)
  invisible(path)
}

#' Launch the PredictLearn interface
#'
#' Runs locally in your browser. No data leaves your computer.
#'
#' @param launch.browser Open a browser window automatically.
#' @param ... Passed to [shiny::runApp()].
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "predictlearn")
  if (app_dir == "") {
    stop("Could not find the app directory. Try reinstalling predictlearn.")
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
