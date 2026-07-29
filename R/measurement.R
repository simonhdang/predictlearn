#' Measurement model: specification, estimation, and score extraction
#'
#' These functions turn a user-supplied mapping of indicators to constructs
#' into a fitted PLS-SEM model and a matrix of latent variable scores suitable
#' for structure discovery.

# ---------------------------------------------------------------------------
# Reading data
# ---------------------------------------------------------------------------

#' Read raw indicator data from CSV or Excel
#'
#' @param path Path to a .csv, .xls or .xlsx file.
#' @param sheet Sheet name or index, for Excel files only.
#' @return A data frame.
#' @export
read_indicator_data <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(path))
  dat <- switch(
    ext,
    csv  = utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE),
    xlsx = as.data.frame(readxl::read_excel(path, sheet = sheet)),
    xls  = as.data.frame(readxl::read_excel(path, sheet = sheet)),
    stop("Unsupported file type '", ext, "'. Use .csv, .xls or .xlsx.")
  )
  if (nrow(dat) == 0L) stop("The file contains no rows.")
  tidy_columns(dat)
}

#' Reduce a data frame to column types the rest of the package can handle
#'
#' Spreadsheet readers return more than numbers and text. A column whose cells
#' have mixed types can arrive as a list; dates arrive as Date or POSIXct;
#' factors arrive from some CSV settings. None of these survive the numeric
#' checks downstream, and an unhandled one takes the whole session down, so
#' each is flattened to character here and classified properly afterwards.
#'
#' @param data A data frame.
#' @return A data frame of atomic columns only.
#' @export
tidy_columns <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  for (nm in names(data)) {
    x <- data[[nm]]

    if (is.list(x)) {
      data[[nm]] <- vapply(x, function(v) {
        if (length(v) == 0L || all(is.na(v))) NA_character_
        else as.character(v[[1]])
      }, character(1))
      next
    }
    if (is.factor(x)) { data[[nm]] <- as.character(x); next }
    if (inherits(x, c("Date", "POSIXct", "POSIXt", "difftime", "hms"))) {
      data[[nm]] <- as.character(x); next
    }
    if (!is.atomic(x)) data[[nm]] <- as.character(x)
  }

  # Blank columns from trailing spreadsheet cells carry no information and
  # break variance checks.
  empty <- vapply(data, function(x) all(is.na(x)), logical(1))
  blank_names <- !nzchar(trimws(names(data))) | grepl("^\\.\\.\\.[0-9]+$", names(data))
  drop <- empty & blank_names
  if (any(drop)) data <- data[, !drop, drop = FALSE]

  data
}

# ---------------------------------------------------------------------------
# Column types
# ---------------------------------------------------------------------------

#' Missing-value codes treated as blank when reading text columns
#' @export
default_na_strings <- c("", "NA", "N/A", "n/a", "na", "NaN", ".", "-", "--",
                        "missing", "Missing", "MISSING", "NULL", "null",
                        "#N/A", "#NULL!", "#DIV/0!")

#' Classify every column by whether it can serve as an indicator
#'
#' Spreadsheets routinely store numbers as text, usually because one cell in
#' the column holds a missing-value code or a stray note. Such a column reaches
#' R as character and fails deep inside the PLS algorithm. This separates
#' columns that hold numbers written as text, which can be converted safely,
#' from columns that hold words, which cannot.
#'
#' @param data A data frame.
#' @param na_strings Values treated as missing rather than as text.
#' @return A data frame with one row per column: `column`, `type`,
#'   `n_distinct`, `missing_codes`, and `text_values`.
#' @export
classify_columns <- function(data, na_strings = default_na_strings) {
  rows <- lapply(names(data), function(nm) {
    x <- data[[nm]]
    if (!is.atomic(x)) x <- as.character(x)

    if (is.numeric(x) || is.logical(x)) {
      return(data.frame(
        column = nm,
        type = if (is.logical(x)) "logical" else "numeric",
        n_distinct = length(unique(stats::na.omit(x))),
        missing_codes = "", text_values = "",
        stringsAsFactors = FALSE))
    }

    ch <- trimws(as.character(x))
    codes <- unique(ch[ch %in% na_strings])
    ch[ch %in% na_strings] <- NA_character_
    present <- ch[!is.na(ch)]
    suppressWarnings(num <- as.numeric(present))
    unparsed <- unique(present[is.na(num)])

    type <- if (!length(present)) "empty" else
      if (!length(unparsed)) "text-numeric" else "text-categorical"

    data.frame(
      column = nm, type = type,
      n_distinct = length(unique(present)),
      missing_codes = paste(codes, collapse = ", "),
      text_values = paste(utils::head(unparsed, 5), collapse = ", "),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Convert text columns that hold numbers
#'
#' Only columns classified as `text-numeric` are converted. Columns holding
#' words are left alone: coercing them would replace every value with a missing
#' one, which is worse than the error it avoids.
#'
#' @param data A data frame.
#' @param columns Columns to convert. Defaults to every convertible column.
#' @param na_strings Values treated as missing.
#' @return A list with `data` and `report`, a data frame of what changed.
#' @export
convert_to_numeric <- function(data, columns = NULL,
                               na_strings = default_na_strings) {
  cls <- classify_columns(data, na_strings)
  convertible <- cls$column[cls$type == "text-numeric"]
  columns <- if (is.null(columns)) convertible else intersect(columns, convertible)

  report <- data.frame(column = character(0), values_set_missing = integer(0),
                       stringsAsFactors = FALSE)

  for (nm in columns) {
    ch <- trimws(as.character(data[[nm]]))
    was_missing <- is.na(data[[nm]])
    ch[ch %in% na_strings] <- NA_character_
    suppressWarnings(converted <- as.numeric(ch))
    report <- rbind(report, data.frame(
      column = nm,
      values_set_missing = sum(is.na(converted) & !was_missing),
      stringsAsFactors = FALSE))
    data[[nm]] <- converted
  }

  list(data = data, report = report)
}

# ---------------------------------------------------------------------------
# Guessing constructs from item codes
# ---------------------------------------------------------------------------

#' Guess construct names from indicator names
#'
#' Survey item codes almost always carry the construct in the stem and the item
#' number as a trailing digit, so AT1 to AT12 belong to AT. This strips a
#' trailing run of digits, together with any separator before it and an
#' optional reverse-coding suffix.
#'
#' Columns that are not numeric are left unassigned: respondent IDs,
#' timestamps and free text cannot be indicators, and guessing a construct for
#' them only creates work.
#'
#' This is a starting point, not an answer. Item codes that share a stem by
#' coincidence will be grouped wrongly, and the mapping is editable for exactly
#' that reason.
#'
#' @param data A data frame of raw indicator data.
#' @return A character vector, one construct name per column, empty where no
#'   guess was made.
#' @export
guess_constructs <- function(data) {
  nm <- names(data)

  # AT1 -> AT, CE_6 -> CE, PU.4 -> PU, PRC2R -> PRC
  stem <- sub("[._ -]*[0-9]+[Rr]?$", "", nm)

  # A column named entirely of digits leaves an empty stem; keep the original.
  empty <- !nzchar(stem)
  stem[empty] <- nm[empty]

  # Only numeric columns can be indicators.
  usable <- vapply(data, function(x)
    is.atomic(x) && (is.numeric(x) || is.logical(x)) && !all(is.na(x)),
    logical(1))
  stem[!usable] <- ""

  stem
}

#' Warn when a guess has collapsed too much of the instrument into one construct
#'
#' Generic item codes such as Q1 to Q60 all share a stem, so the guess would put
#' every item in one construct. That is almost never what was intended.
#'
#' @param guess Output of [guess_constructs()].
#' @return A character vector of warnings, empty if none apply.
#' @export
check_guess <- function(guess) {
  assigned <- guess[nzchar(guess)]
  if (length(assigned) < 6L) return(character(0))
  tb <- table(assigned)
  biggest <- max(tb) / length(assigned)
  if (biggest > 0.6) {
    return(sprintf(
      paste("One construct ('%s') has taken %.0f%% of your indicators.",
            "Item codes that share a stem by coincidence group wrongly.",
            "Check the mapping before estimating."),
      names(tb)[which.max(tb)], 100 * biggest))
  }
  character(0)
}

# ---------------------------------------------------------------------------
# Pre-flight checks
# ---------------------------------------------------------------------------

#' Check indicator data before estimation
#'
#' PLS estimation fails deep inside matrix algebra when an indicator is not
#' numeric or has no variance, and the resulting message names neither the
#' column nor the construct. This checks the data first and reports problems
#' in terms of the columns they came from.
#'
#' @param data Raw indicator data.
#' @param mapping The indicator-to-construct mapping.
#' @param max_missing Proportion of missing values to warn at.
#' @return A list with `errors` and `warnings`, both character vectors.
#' @export
check_indicator_data <- function(data, mapping, max_missing = 0.2) {
  errors <- character(0)
  warnings <- character(0)

  map <- mapping[nzchar(trimws(mapping$construct)), , drop = FALSE]
  if (!nrow(map)) {
    return(list(errors = "No indicators are assigned to a construct.",
                warnings = character(0)))
  }

  absent <- setdiff(map$indicator, names(data))
  if (length(absent)) {
    errors <- c(errors, paste0(
      "These indicators are not columns in the data: ",
      paste(absent, collapse = ", ")))
    map <- map[map$indicator %in% names(data), , drop = FALSE]
  }

  cols <- data[, map$indicator, drop = FALSE]

  # Logical columns pass is.numeric() checks in some places and fail in others,
  # so they are converted rather than rejected.
  logi <- names(cols)[vapply(cols, is.logical, logical(1))]
  if (length(logi)) {
    warnings <- c(warnings, paste0(
      "Converted TRUE/FALSE to 1/0 for: ", paste(logi, collapse = ", "), "."))
  }

  bad_type <- names(cols)[vapply(cols, function(x)
    !(is.numeric(x) || is.logical(x)), logical(1))]
  if (length(bad_type)) {
    examples <- vapply(bad_type[seq_len(min(3L, length(bad_type)))],
                       function(n) {
                         v <- utils::head(stats::na.omit(as.character(cols[[n]])), 2)
                         paste0(n, " (", paste(v, collapse = ", "), ")")
                       }, character(1))
    errors <- c(errors, paste0(
      "These indicators are text, not numbers: ",
      paste(bad_type, collapse = ", "),
      ". Examples: ", paste(examples, collapse = "; "),
      ". Recode them, or clear their construct to leave them out. A common ",
      "cause is a missing-value code such as 'NA' or '-99' entered as text."))
  }

  numeric_cols <- cols[, vapply(cols, function(x)
    is.numeric(x) || is.logical(x), logical(1)), drop = FALSE]

  if (ncol(numeric_cols)) {
    all_na <- names(numeric_cols)[vapply(numeric_cols,
      function(x) all(is.na(x)), logical(1))]
    if (length(all_na)) {
      errors <- c(errors, paste0(
        "These indicators are entirely missing: ",
        paste(all_na, collapse = ", "), "."))
    }

    sds <- vapply(numeric_cols, function(x) {
      s <- stats::sd(as.numeric(x), na.rm = TRUE)
      if (is.na(s)) 0 else s
    }, numeric(1))
    constant <- names(sds)[sds < 1e-8 & !(names(sds) %in% all_na)]
    if (length(constant)) {
      errors <- c(errors, paste0(
        "These indicators have the same value for every respondent, so they ",
        "carry no information: ", paste(constant, collapse = ", "), "."))
    }

    miss <- vapply(numeric_cols, function(x) mean(is.na(x)), numeric(1))
    high <- names(miss)[miss > max_missing & miss < 1]
    if (length(high)) {
      warnings <- c(warnings, paste0(
        "More than ", round(100 * max_missing), "% missing for: ",
        paste(sprintf("%s (%.0f%%)", high, 100 * miss[high]),
              collapse = ", "),
        ". Missing values are mean-replaced."))
    }
  }

  n_con <- length(unique(map$construct))
  if (n_con < 2L) {
    errors <- c(errors, paste0(
      "Only ", n_con, " construct is assigned. Discovery needs at least two."))
  }

  list(errors = errors, warnings = warnings)
}

#' Coerce logical indicator columns to numeric
#' @param data Raw indicator data.
#' @return The data frame with logical columns converted to 0/1.
#' @export
coerce_indicators <- function(data) {
  logi <- vapply(data, is.logical, logical(1))
  if (any(logi)) data[logi] <- lapply(data[logi], as.integer)
  data
}

# ---------------------------------------------------------------------------
# Building a seminr measurement model from a mapping table
# ---------------------------------------------------------------------------

#' Build a seminr measurement model
#'
#' @param mapping A data frame with columns `construct`, `indicator`, and
#'   `mode`. `mode` is one of "A" (correlation weights, reflective/composite)
#'   or "B" (regression weights, formative). One row per indicator.
#' @param higher_order Optional list. Each element is named for the
#'   higher-order construct and contains a character vector of its first-order
#'   dimensions, e.g. `list(reasonsf = c("PU", "PE", "PI", "UB"))`.
#' @param hoc_method Either "two_stage" or "extended_repeated_indicators".
#' @param hoc_weights "A" or "B".
#' @return A seminr measurement model object.
#' @export
build_measurement_model <- function(mapping,
                                    higher_order = NULL,
                                    hoc_method = c("two_stage",
                                                   "extended_repeated_indicators"),
                                    hoc_weights = "A") {
  hoc_method <- match.arg(hoc_method)
  validate_mapping(mapping)

  constructs_list <- list()

  for (cn in unique(mapping$construct)) {
    rows  <- mapping[mapping$construct == cn, , drop = FALSE]
    items <- rows$indicator
    mode  <- unique(rows$mode)
    if (length(mode) != 1L) {
      stop("Construct '", cn, "' has indicators with conflicting modes.")
    }
    wt <- if (identical(mode, "B")) seminr::mode_B else seminr::mode_A

    constructs_list[[length(constructs_list) + 1L]] <-
      if (length(items) == 1L) {
        seminr::composite(cn, seminr::single_item(items), weights = wt)
      } else {
        seminr::composite(cn, items, weights = wt)
      }
  }

  if (!is.null(higher_order) && length(higher_order) > 0L) {
    hw <- if (identical(hoc_weights, "B")) seminr::mode_B else seminr::mode_A
    hoc_fun <- if (hoc_method == "two_stage") {
      seminr::two_stage
    } else {
      seminr::extended_repeated_indicators
    }
    for (hn in names(higher_order)) {
      dims <- higher_order[[hn]]
      missing_dims <- setdiff(dims, unique(mapping$construct))
      if (length(missing_dims)) {
        stop("Higher-order construct '", hn, "' names dimensions that are not ",
             "in the mapping: ", paste(missing_dims, collapse = ", "))
      }
      constructs_list[[length(constructs_list) + 1L]] <-
        seminr::higher_composite(hn, dimensions = dims,
                                 method = hoc_fun, weights = hw)
    }
  }

  mm <- do.call(seminr::constructs, constructs_list)

  # Record what we were asked to build. seminr does not return a named list, so
  # re-deriving these later means guessing at its internal representation.
  attr(mm, "predictlearn_constructs") <-
    unique(c(unique(mapping$construct), names(higher_order)))
  attr(mm, "predictlearn_hoc") <- higher_order
  mm
}

#' Check a mapping table for the problems that break estimation downstream
#' @param mapping See [build_measurement_model()].
#' @return Invisibly TRUE; stops with a message on failure.
#' @export
validate_mapping <- function(mapping) {
  required <- c("construct", "indicator", "mode")
  missing_cols <- setdiff(required, names(mapping))
  if (length(missing_cols)) {
    stop("The mapping is missing these columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (anyDuplicated(mapping$indicator)) {
    dup <- unique(mapping$indicator[duplicated(mapping$indicator)])
    stop("Each indicator can belong to only one construct. Assigned twice: ",
         paste(dup, collapse = ", "))
  }
  bad_mode <- setdiff(unique(mapping$mode), c("A", "B"))
  if (length(bad_mode)) {
    stop("Mode must be 'A' or 'B'. Found: ", paste(bad_mode, collapse = ", "))
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Saturated structural model for score extraction
# ---------------------------------------------------------------------------

#' Build a saturated (all-pairs) recursive structural model
#'
#' Richter and Tudoran (2024) extract latent variable scores from a model in
#' which every construct is connected, so that the scores do not already
#' encode the theoretical structure that discovery is meant to test.
#'
#' PLS requires a recursive inner model, so an ordering has to be imposed. This
#' is not a substantive choice: under the factor weighting scheme the inner
#' weights are the signs of the construct correlations, which makes the
#' resulting scores effectively invariant to the assumed directions.
#'
#' @param construct_names Character vector of construct names.
#' @return A seminr structural model.
#' @export
saturated_structural_model <- function(construct_names) {
  n <- length(construct_names)
  if (n < 2L) stop("A structural model needs at least two constructs.")
  path_list <- vector("list", n - 1L)
  for (i in seq_len(n - 1L)) {
    path_list[[i]] <- seminr::paths(from = construct_names[i],
                                    to   = construct_names[(i + 1L):n])
  }
  do.call(seminr::relationships, path_list)
}

# ---------------------------------------------------------------------------
# Estimation and score extraction
# ---------------------------------------------------------------------------

#' Estimate a PLS model and extract latent variable scores
#'
#' @param data Raw indicator data.
#' @param measurement_model From [build_measurement_model()].
#' @param structural_model Optional. Defaults to the saturated model.
#' @param inner_weights "factorial" (default, for score extraction) or "path".
#' @param standardize Standardise the returned scores. Default TRUE.
#' @param construct_names Optional character vector, if the construct names
#'   cannot be recovered from the measurement model automatically.
#' @return A list with `model`, `scores`, and `constructs`.
#' @export
extract_scores <- function(data,
                           measurement_model,
                           structural_model = NULL,
                           inner_weights = c("factorial", "path"),
                           standardize = TRUE,
                           construct_names = NULL) {
  inner_weights <- match.arg(inner_weights)
  iw <- if (inner_weights == "factorial") {
    seminr::path_factorial
  } else {
    seminr::path_weighting
  }

  if (is.null(structural_model)) {
    cn <- construct_names %||% structural_construct_names(measurement_model)
    if (length(cn) < 2L) {
      stop("Only ", length(cn), " construct(s) were found (",
           paste(cn, collapse = ", "), "). Discovery needs at least two. ",
           "Check that every indicator in the mapping has a construct name.")
    }
    structural_model <- saturated_structural_model(cn)
  }

  model <- seminr::estimate_pls(
    data              = data,
    measurement_model = measurement_model,
    structural_model  = structural_model,
    inner_weights     = iw
  )

  scores <- as.data.frame(model$construct_scores)
  if (standardize) {
    scores[] <- lapply(scores, function(x) as.numeric(scale(x)))
  }

  list(model = model, scores = scores, constructs = names(scores))
}

#' Extract the construct names from a measurement model
#'
#' Models built by [build_measurement_model()] carry their construct names as
#' an attribute. For models built directly in seminr this falls back to
#' introspection, which has to cope with more than one internal representation.
#'
#' @param measurement_model A seminr measurement model.
#' @return Character vector of construct names.
#' @export
construct_names_of <- function(measurement_model) {

  # 1. Recorded at build time -- the reliable path.
  recorded <- attr(measurement_model, "predictlearn_constructs")
  if (length(recorded)) return(unique(as.character(recorded)))

  # 2. A named list.
  nm <- names(measurement_model)
  if (!is.null(nm)) {
    nm <- nm[nzchar(nm)]
    if (length(nm)) return(unique(nm))
  }

  # 3. seminr stores each construct as a matrix whose first column repeats the
  #    construct name once per indicator.
  out <- character(0)
  for (el in measurement_model) {
    cand <- NULL
    if (is.matrix(el) || is.data.frame(el)) {
      cand <- as.character(el[, 1])
    } else if (!is.null(attr(el, "name"))) {
      cand <- as.character(attr(el, "name"))
    } else if (is.list(el)) {
      for (f in c("construct", "name")) {
        if (!is.null(el[[f]])) { cand <- as.character(el[[f]]); break }
      }
    } else if (is.character(el) && length(el)) {
      cand <- el[1]
    }
    if (!is.null(cand)) out <- c(out, cand)
  }
  out <- unique(out[nzchar(out) & !is.na(out)])

  if (!length(out)) {
    stop("Could not determine the construct names from this measurement ",
         "model. Pass `construct_names` to extract_scores() explicitly.")
  }
  out
}

#' Construct names to use in the saturated structural model
#'
#' Where a higher-order construct is defined, its dimensions are dropped: a
#' higher-order construct and the dimensions it is built from should not both
#' appear in the same structural model.
#'
#' @param measurement_model A seminr measurement model.
#' @return Character vector.
#' @export
structural_construct_names <- function(measurement_model) {
  cn  <- construct_names_of(measurement_model)
  hoc <- attr(measurement_model, "predictlearn_hoc")
  if (length(hoc)) cn <- setdiff(cn, unlist(hoc, use.names = FALSE))
  cn
}

# ---------------------------------------------------------------------------
# Reliability and validity
# ---------------------------------------------------------------------------

#' Reliability and validity tables for a fitted PLS model
#'
#' Scores extracted from a measurement model that has not been assessed are
#' not interpretable, so this is reported before discovery is allowed to run.
#'
#' @param model A fitted seminr model.
#' @return A list with `reliability`, `loadings`, `htmt`, and `flags`.
#' @export
assess_measurement <- function(model) {
  smry <- summary(model)

  reliability <- tryCatch(as.data.frame(smry$reliability),
                          error = function(e) NULL)
  loadings    <- tryCatch(as.data.frame(smry$loadings),
                          error = function(e) NULL)
  htmt        <- tryCatch(as.data.frame(smry$validity$htmt),
                          error = function(e) NULL)

  flags <- character(0)

  if (!is.null(reliability)) {
    ave_col <- grep("^AVE$", names(reliability), ignore.case = TRUE, value = TRUE)
    if (length(ave_col)) {
      low <- rownames(reliability)[reliability[[ave_col[1]]] < 0.50]
      low <- low[!is.na(low)]
      if (length(low)) {
        flags <- c(flags, paste0(
          "AVE below 0.50 for: ", paste(low, collapse = ", "),
          ". Convergent validity is not established for these constructs."))
      }
    }
    for (nm in c("rhoC", "CR", "Composite Reliability")) {
      cr_col <- grep(paste0("^", nm, "$"), names(reliability),
                     ignore.case = TRUE, value = TRUE)
      if (length(cr_col)) {
        low <- rownames(reliability)[reliability[[cr_col[1]]] < 0.70]
        low <- low[!is.na(low)]
        if (length(low)) {
          flags <- c(flags, paste0(
            "Composite reliability below 0.70 for: ",
            paste(low, collapse = ", "), "."))
        }
        break
      }
    }
  }

  if (!is.null(htmt)) {
    m <- as.matrix(htmt)
    idx <- which(m > 0.85 & !is.na(m), arr.ind = TRUE)
    if (nrow(idx)) {
      pairs <- unique(apply(idx, 1, function(r) {
        paste(sort(c(rownames(m)[r[1]], colnames(m)[r[2]])), collapse = " / ")
      }))
      flags <- c(flags, paste0(
        "HTMT above 0.85 for: ", paste(pairs, collapse = "; "),
        ". These constructs may not be discriminant. If they are conceptually ",
        "related, check the higher-order diagnostics before proceeding."))
    }
  }

  list(reliability = reliability, loadings = loadings,
       htmt = htmt, flags = flags)
}

#' Item-level diagnostics for constructs that fail convergent validity
#'
#' Average variance extracted is the mean squared loading of a construct's
#' items, so a construct falls below 0.50 because particular items load weakly.
#' This reports each item's loading and what the construct's AVE would become
#' without it.
#'
#' The recomputed AVE is an approximation. Removing an item changes the
#' weights, and therefore every remaining loading, so the true value is only
#' known after re-estimating. It is reliable for ranking which item to drop
#' first, not for predicting the exact result.
#'
#' @param model A fitted seminr model.
#' @param min_loading Loadings below this are marked for removal.
#' @param weak_loading Loadings below this are marked for consideration.
#' @return A data frame, one row per item of every multi-item construct.
#' @export
item_diagnostics <- function(model, min_loading = 0.40, weak_loading = 0.708) {
  smry <- summary(model)
  L <- tryCatch(as.matrix(smry$loadings), error = function(e) NULL)
  if (is.null(L)) return(NULL)

  rows <- list()
  for (cn in colnames(L)) {
    l <- L[, cn]
    items <- rownames(L)[!is.na(l) & abs(l) > 1e-8]
    if (length(items) < 2L) next          # single-item constructs cannot be diagnosed
    lv  <- l[items]
    ave <- mean(lv^2)

    for (it in items) {
      rest   <- setdiff(items, it)
      ave_wo <- if (length(rest)) mean(l[rest]^2) else NA_real_
      loading <- as.numeric(lv[it])
      rows[[length(rows) + 1L]] <- data.frame(
        construct      = cn,
        item           = it,
        loading        = round(loading, 3),
        variance_share = round(loading^2, 3),
        AVE            = round(ave, 3),
        AVE_if_dropped = round(ave_wo, 3),
        change         = round(ave_wo - ave, 3),
        verdict        = if (abs(loading) < min_loading) "remove"
                         else if (abs(loading) < weak_loading) "consider"
                         else "keep",
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out[order(out$construct, out$loading), ]
}

#' Collinearity among construct scores
#'
#' Reports the full collinearity variance inflation factor of Kock (2015):
#' each construct is regressed on every other construct, and the VIF follows
#' from the resulting R squared. Unlike the usual inner-model VIF this needs no
#' structural model, so it can be run as soon as scores exist.
#'
#' It serves two purposes. Values above 5 mean constructs are redundant enough
#' to destabilise any structural estimate, and structure learning will fail
#' outright once the correlation matrix becomes singular. Values above 3.3 are
#' also the threshold Kock proposes as evidence of common method bias.
#'
#' @param scores Data frame of latent variable scores.
#' @param vif_warn VIF above which common method bias is indicated.
#' @param vif_error VIF above which collinearity is a problem in its own right.
#' @param cor_warn Absolute correlation above which a pair is reported.
#' @return A list with `vif`, `pairs`, and `flags`.
#' @export
collinearity_diagnostics <- function(scores, vif_warn = 3.3, vif_error = 5,
                                     cor_warn = 0.90) {
  d <- as.data.frame(scores)
  varies <- vapply(d, function(x)
    length(unique(stats::na.omit(x))) > 1L, logical(1))
  d <- d[, varies, drop = FALSE]
  nm <- names(d)

  if (length(nm) < 2L) {
    return(list(vif = NULL, pairs = NULL,
                flags = "At least two varying constructs are needed."))
  }

  vif <- vapply(nm, function(j) {
    others <- setdiff(nm, j)
    fit <- tryCatch(stats::lm(stats::reformulate(others, j), data = d),
                    error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    r2 <- suppressWarnings(summary(fit)$r.squared)
    if (is.na(r2)) return(NA_real_)
    if (r2 >= 1 - 1e-10) return(Inf)
    1 / (1 - r2)
  }, numeric(1))

  vif_tab <- data.frame(
    construct = nm,
    R2 = round(1 - 1 / vif, 3),
    VIF = round(vif, 3),
    verdict = ifelse(is.na(vif), "not estimable",
              ifelse(vif >= vif_error, "collinear",
              ifelse(vif >= vif_warn, "above 3.3", "ok"))),
    stringsAsFactors = FALSE)
  vif_tab <- vif_tab[order(-vif_tab$VIF), ]

  R <- suppressWarnings(stats::cor(d, use = "pairwise.complete.obs"))
  R[lower.tri(R, diag = TRUE)] <- NA
  idx <- which(abs(R) >= cor_warn & !is.na(R), arr.ind = TRUE)
  pairs <- if (nrow(idx)) {
    data.frame(
      construct_1 = rownames(R)[idx[, 1]],
      construct_2 = colnames(R)[idx[, 2]],
      r = round(R[idx], 3),
      stringsAsFactors = FALSE)
  } else NULL

  flags <- character(0)
  bad <- vif_tab$construct[!is.na(vif_tab$VIF) & vif_tab$VIF >= vif_error]
  if (length(bad)) {
    flags <- c(flags, paste0(
      "Full collinearity VIF at or above ", vif_error, " for: ",
      paste(bad, collapse = ", "),
      ". These constructs are largely explained by the others. Structure ",
      "learning will be unstable, and may fail outright."))
  }
  warn <- vif_tab$construct[!is.na(vif_tab$VIF) &
                            vif_tab$VIF >= vif_warn & vif_tab$VIF < vif_error]
  if (length(warn)) {
    flags <- c(flags, paste0(
      "Full collinearity VIF above ", vif_warn, " for: ",
      paste(warn, collapse = ", "),
      ". Below the collinearity threshold, but Kock (2015) treats this as an ",
      "indication of common method bias."))
  }
  inf <- vif_tab$construct[is.infinite(vif_tab$VIF)]
  if (length(inf)) {
    flags <- c(flags, paste0(
      "These constructs are perfectly explained by the others: ",
      paste(inf, collapse = ", "),
      ". One of each redundant set has to be removed before discovery can ",
      "run. Keeping a categorical variable alongside dummies coded from it ",
      "is the usual cause."))
  }
  if (!is.null(pairs)) {
    flags <- c(flags, paste0(
      "Correlations at or above ", cor_warn, ": ",
      paste(sprintf("%s / %s (%.2f)", pairs$construct_1, pairs$construct_2,
                    pairs$r), collapse = "; "), "."))
  }

  list(vif = vif_tab, pairs = pairs, flags = flags)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
