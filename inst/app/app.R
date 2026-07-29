library(shiny)
library(predictlearn)
library(DT)

# ---------------------------------------------------------------------------
# Design tokens
#
# This is a measurement instrument, so every number is set in a tabular
# monospace and every label is quiet. The one place the interface raises its
# voice is the constraint matrix, which is where the researcher's theory
# actually enters the analysis.
# ---------------------------------------------------------------------------
CSS <- "
:root{
  --ink:#16191D; --paper:#F7F8F7; --panel:#FFFFFF; --rule:#D8DBD9;
  --muted:#6A7169; --accent:#2D5F5D; --block:#A8742A; --wash:#EDF0EF;
}
body{background:var(--paper); color:var(--ink);
  font-family:'Helvetica Neue',Arial,sans-serif; font-size:14px;}
.masthead{border-bottom:1px solid var(--ink); padding:22px 0 14px; margin-bottom:26px;}
.masthead h1{font-size:20px; font-weight:700; letter-spacing:.14em;
  text-transform:uppercase; margin:0;}
.masthead p{color:var(--muted); margin:6px 0 0; max-width:62ch; font-size:13px;}
.eyebrow{font-size:10px; letter-spacing:.18em; text-transform:uppercase;
  color:var(--muted); margin:0 0 8px; font-weight:600;}
.panel{background:var(--panel); border:1px solid var(--rule); padding:18px; margin-bottom:18px;}
.stat, table.dataTable, pre{font-family:ui-monospace,'SF Mono',Menlo,Consolas,monospace;
  font-variant-numeric:tabular-nums;}
pre{background:var(--wash); border:1px solid var(--rule); font-size:12px; padding:12px;}
.note{color:var(--muted); font-size:12.5px; line-height:1.55; max-width:70ch;}
.flag{border-left:3px solid var(--block); background:#FBF6EF;
  padding:10px 14px; margin:8px 0; font-size:13px;}
.ok{border-left:3px solid var(--accent); background:#EFF4F3;
  padding:10px 14px; margin:8px 0; font-size:13px;}
.btn-primary{background:var(--accent); border-color:var(--accent); border-radius:0;}
.btn-default{border-radius:0; border-color:var(--rule);}
.nav-tabs>li>a{border-radius:0; font-size:11px; letter-spacing:.12em;
  text-transform:uppercase; color:var(--muted);}
.nav-tabs>li.active>a{color:var(--ink); border-bottom-color:transparent;}
.form-control,.selectize-input{border-radius:0; border-color:var(--rule);}

/* Constraint matrix: the signature element */
.blmatrix{border-collapse:collapse; font-family:ui-monospace,Menlo,monospace; font-size:11px;}
.blmatrix th{font-weight:600; color:var(--muted); padding:4px 6px; text-align:left;}
.blmatrix th.col{writing-mode:vertical-rl; transform:rotate(180deg); height:96px;}
.blmatrix td{border:1px solid var(--rule); width:26px; height:26px; text-align:center; padding:0;}
.blmatrix td.diag{background:repeating-linear-gradient(45deg,var(--wash),
  var(--wash) 3px,#FFF 3px,#FFF 6px);}
.blmatrix input{margin:0; cursor:pointer;}
"

# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(CSS))),
  div(class = "masthead",
      h1("PredictLearn"),
      p("Estimate a measurement model from your raw data, then search the ",
        "resulting construct scores for structural relationships your theory ",
        "did not specify. Everything runs on this computer; no data is uploaded.")
  ),

  tabsetPanel(
    id = "tabs",

    # -- 1. Data ------------------------------------------------------------
    tabPanel(
      "Data",
      div(class = "panel",
          p(class = "eyebrow", "Load raw indicator data"),
          fileInput("file", NULL, accept = c(".csv", ".xls", ".xlsx"),
                    buttonLabel = "Choose file", placeholder = "CSV or Excel"),
          p(class = "note",
            "One row per respondent, one column per indicator. Keep the item ",
            "codes as column names; you will map them to constructs next.")
      ),
      uiOutput("column_types"),
      div(class = "panel",
          p(class = "eyebrow", "Preview"),
          DTOutput("preview"))
    ),

    # -- 2. Measurement model ------------------------------------------------
    tabPanel(
      "Measurement model",
      div(class = "panel",
          p(class = "eyebrow", "Assign indicators to constructs"),
          p(class = "note",
            "Constructs are read from your item codes when the file loads: ",
            "AT1 to AT12 become AT. Correct anything it got wrong by ",
            "double-clicking a cell and pressing Enter. Mode A uses ",
            "correlation weights, for reflective and composite constructs; ",
            "mode B uses regression weights, for formative ones. Clear a ",
            "construct to leave that indicator out of the model."),
          br(),
          uiOutput("mapping_summary"),
          div(style = "margin:12px 0",
              actionButton("autogroup", "Re-detect from item codes",
                           class = "btn-default"),
              actionButton("clearmap", "Clear all", class = "btn-default")),
          DTOutput("mapping_editor"),
          br(),
          actionButton("estimate", "Estimate measurement model",
                       class = "btn-primary")
      ),
      div(class = "panel",
          p(class = "eyebrow", "Reliability and validity"),
          uiOutput("assessment_flags"),
          DTOutput("reliability")),
      div(class = "panel",
          p(class = "eyebrow", "Item loadings"),
          p(class = "note",
            "AVE is the mean squared loading, so a construct falls below 0.50 ",
            "because particular items load weakly. 'AVE if dropped' shows what ",
            "the construct would reach without each item \u2014 use it to rank ",
            "which to drop first, then remove the item from the mapping and ",
            "estimate again. It is an approximation: removing an item changes ",
            "every remaining loading, so the true figure is only known after ",
            "re-estimating."),
          DTOutput("loadings_table")),
      div(class = "panel",
          p(class = "eyebrow", "Collinearity between constructs"),
          p(class = "note",
            "Full collinearity VIF (Kock, 2015): each construct regressed on ",
            "all the others. Above 5 the constructs are redundant enough to ",
            "destabilise any structural estimate, and structure learning may ",
            "fail. Above 3.3 is also Kock's threshold for common method bias. ",
            "An infinite value means one construct is perfectly determined by ",
            "the rest."),
          uiOutput("collinearity_flags"),
          DTOutput("vif_table"),
          br(),
          DTOutput("high_pairs")),
      div(class = "panel",
          p(class = "eyebrow", "Discriminant validity (HTMT)"),
          p(class = "note",
            "Values above 0.85 mean two constructs are not clearly distinct. ",
            "If they are conceptually related, the higher-order tab is the ",
            "place to decide whether they are dimensions of one construct."),
          DTOutput("htmt_table"))
    ),

    # -- 3. Higher-order constructs -----------------------------------------
    tabPanel(
      "Higher-order",
      div(class = "panel",
          p(class = "eyebrow", "Diagnose a candidate"),
          p(class = "note",
            "Whether a set of constructs belongs to one higher-order construct ",
            "is a question about your theory, not about your data. This panel ",
            "reports the evidence you would weigh in answering it. Nothing is ",
            "added to your model unless you add it."),
          br(),
          fluidRow(
            column(5, textInput("hoc_name", "Higher-order construct name", "")),
            column(7, selectizeInput("hoc_dims", "Dimensions", choices = NULL,
                                     multiple = TRUE))
          ),
          actionButton("diagnose", "Run diagnostic", class = "btn-default"),
          br(), br(),
          verbatimTextOutput("hoc_report")
      ),
      div(class = "panel",
          p(class = "eyebrow", "Exploratory groupings"),
          p(class = "note",
            "Clustering on correlation distance will always return groups, ",
            "whether or not a higher-order construct exists. Read this as a ",
            "prompt for thinking, never as a result."),
          numericInput("hoc_k", "Number of groups", 3, min = 2, max = 8, width = "160px"),
          DTOutput("hoc_groups")
      ),
      div(class = "panel",
          p(class = "eyebrow", "Confirm"),
          p(class = "note",
            "Adding a higher-order construct re-estimates the measurement ",
            "model and replaces its dimensions in the scores used for discovery."),
          actionButton("add_hoc", "Add this higher-order construct",
                       class = "btn-primary"),
          uiOutput("hoc_current"))
    ),

    # -- 4. Constraints ------------------------------------------------------
    tabPanel(
      "Constraints",
      div(class = "panel",
          p(class = "eyebrow", "Constructs nothing may cause"),
          p(class = "note",
            "Search is only as meaningful as the constraints you impose. ",
            "Nothing causes a respondent's age, and nothing causes a randomly ",
            "assigned experimental condition, so every arc pointing into those ",
            "can be ruled out before the search begins. Leaving them open only ",
            "gives the algorithm room to find noise."),
          br(),
          selectizeInput("exogenous", NULL, choices = NULL,
                         multiple = TRUE, width = "100%",
                         options = list(placeholder =
                           "Select demographics, stable traits, and experimental conditions")),
          uiOutput("constraint_summary")
      ),
      div(class = "panel",
          p(class = "eyebrow", "Forbid a specific arc"),
          p(class = "note",
            "Optional. Use this to rule out the reverse of a path your theory ",
            "has already tested: if your model says diagnosticity causes ",
            "attitude, forbid attitude causing diagnosticity. The point of ",
            "discovery is to find arcs you did not propose."),
          fluidRow(
            column(4, selectizeInput("arc_from", "From", choices = NULL)),
            column(4, selectizeInput("arc_to", "To", choices = NULL)),
            column(4, br(),
                   actionButton("add_arc", "Forbid this arc",
                                class = "btn-default"))
          ),
          uiOutput("extra_arc_list"),
          actionButton("clear_arcs", "Clear these", class = "btn-default")
      ),
      div(class = "panel",
          p(class = "eyebrow", "Every arc the search may not propose"),
          DTOutput("blacklist_table"))
    ),

    # -- 5. Discovery --------------------------------------------------------
    tabPanel(
      "Discovery",
      div(class = "panel",
          p(class = "eyebrow", "Search"),
          fluidRow(
            column(3, selectInput("algorithm", "Algorithm",
                                  c("Grow-Shrink" = "gs", "Hill-Climbing" = "hc"))),
            column(3, numericInput("boot_R", "Bootstrap replicates (HC)", 500,
                                   min = 100, max = 5000, step = 100)),
            column(3, numericInput("threshold", "Arc strength threshold", 0.8,
                                   min = 0.5, max = 1, step = 0.01)),
            column(3, numericInput("seed", "Seed", 123))
          ),
          selectizeInput("targets", "Evaluate predictive error for",
                         choices = NULL, multiple = TRUE, width = "100%"),
          uiOutput("varcheck"),
          actionButton("run", "Run discovery", class = "btn-primary"),
          p(class = "note", style = "margin-top:10px",
            "Hill-Climbing with bootstrapping takes a few minutes on a typical ",
            "sample.")
      ),
      div(class = "panel",
          p(class = "eyebrow", "Learned structure"),
          fluidRow(
            column(5, checkboxInput("show_isolated",
                                    "Show constructs with no arcs",
                                    value = FALSE)),
            column(7, sliderInput("dag_height", "Plot size",
                                  min = 300, max = 900, value = 440,
                                  step = 20, ticks = FALSE, width = "100%"))
          ),
          uiOutput("dag_plot_ui")),
      div(class = "panel",
          p(class = "eyebrow", "Model comparison"),
          uiOutput("bic_line"),
          DTOutput("rmse_table"),
          p(class = "note",
            "Both algorithms are evaluated by one identical routine: the ",
            "structure is relearned on each training fold, fitted, and each ",
            "target predicted from its parents in that network.")),
      div(class = "panel",
          p(class = "eyebrow", "Arcs not in your baseline"),
          DTOutput("new_arcs"))
    ),

    # -- 6. Non-linearity ----------------------------------------------------
    tabPanel(
      "Non-linearity",
      div(class = "panel",
          p(class = "eyebrow", "Probe the discovered equations"),
          actionButton("run_nl", "Fit MARS and GAM", class = "btn-primary")),
      div(class = "panel", p(class = "eyebrow", "MARS"), DTOutput("mars_table"),
          p(class = "note",
            "'degree' is how far the search was allowed to look; ",
            "'interactions' counts the product terms that actually survived ",
            "pruning. A model tuned at degree 2 often keeps none. Binary ",
            "outcomes are fitted with a binomial link automatically, so ",
            "predictions stay on the probability scale.")),
      div(class = "panel",
          p(class = "eyebrow", "Retained interaction terms"),
          p(class = "note",
            "A MARS interaction is a product of hinge functions, so it acts ",
            "only in the range where both hinges are active. That is a local ",
            "effect, not the global multiplicative moderation a structural ",
            "model tests. Treat anything listed here as a lead to confirm in ",
            "PLS-SEM, with a theoretical rationale, never as a moderation ",
            "result in itself."),
          DTOutput("mars_int_table")),
      div(class = "panel", p(class = "eyebrow", "GAM"), DTOutput("gam_table"),
          p(class = "note",
            "'equation' is the outcome; 'term' is one of its predictors. ",
            "Effective degrees of freedom near 1 means the smooth has ",
            "collapsed to a straight line. These are additive models, so every ",
            "row is a main effect: the fit is a sum of separate curves and ",
            "contains no interaction by construction.")),
      div(class = "panel",
          p(class = "eyebrow", "Interactions between predictors"),
          p(class = "note",
            "For each pair of continuous predictors in an equation, a tensor ",
            "product term is added to the additive model and tested against ",
            "it. A significant result means the effect of one predictor really ",
            "does vary with the level of the other across their observed ",
            "range. Which one moderates which is a theoretical claim, and the ",
            "interaction should be re-estimated in your structural model ",
            "before being reported as moderation."),
          DTOutput("gam_int_table")),
      div(class = "panel",
          p(class = "eyebrow", "Probe an interaction"),
          p(class = "note",
            "Pick a pair from the table above. The product term is symmetric, ",
            "so which variable you call focal and which you call the moderator ",
            "changes only how the result is described \u2014 not the fit. Look ",
            "at whether the effect strengthens, weakens or reverses, and let ",
            "that guide which framing your theory can support."),
          fluidRow(
            column(4, selectInput("probe_target", "Outcome", choices = NULL)),
            column(4, selectInput("probe_focal", "Focal predictor", choices = NULL)),
            column(4, selectInput("probe_mod", "Moderator", choices = NULL))
          ),
          actionButton("run_probe", "Probe", class = "btn-primary"),
          br(), br(),
          plotOutput("probe_plot", height = "380px"),
          br(),
          p(class = "eyebrow", "Simple slopes"),
          DTOutput("probe_slopes"),
          p(class = "note",
            "The slope of a smooth is not constant, so each row is the average ",
            "slope across the central range of the focal predictor: the change ",
            "in the outcome from -1 SD to +1 SD, divided by that distance. ",
            "Standard errors come from the model's covariance matrix. A ",
            "moderation claim needs these slopes to differ meaningfully, not ",
            "merely for one of them to be significant.")),
      div(class = "panel",
          p(class = "eyebrow", "Out-of-sample performance"),
          DTOutput("cv_table"),
          p(class = "note",
            "Continuous outcomes report RMSE, MAE and cross-validated R\u00b2. ",
            "Binary outcomes report the Brier score, log loss and accuracy at a ",
            "0.5 cut, because RMSE on a 0/1 outcome is not interpretable.")),
      div(class = "panel",
          p(class = "eyebrow", "Does the flexibility earn its keep?"),
          DTOutput("nl_test"),
          p(class = "note",
            "Each fitted model is compared against one constrained to be ",
            "linear. A non-significant test means the curvature is not worth ",
            "carrying into your structural model.")),
      div(class = "panel",
          p(class = "eyebrow", "Partial dependence"),
          plotOutput("pdp_plot", height = "380px"))
    ),

    # -- 7. Export -----------------------------------------------------------
    tabPanel(
      "Export",
      div(class = "panel",
          p(class = "eyebrow", "Reproducible script"),
          p(class = "note",
            "Writes a plain R script that reproduces this analysis without ",
            "PredictLearn, so reviewers can audit it and you can archive it ",
            "alongside your manuscript."),
          downloadButton("dl_script", "Download analysis script",
                         class = "btn-primary")),
      div(class = "panel",
          p(class = "eyebrow", "Construct scores"),
          downloadButton("dl_scores", "Download latent variable scores",
                         class = "btn-default"))
    )
  )
)

# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    raw = NULL, mapping = NULL, fit = NULL, scores = NULL,
    hoc = list(), blocked = NULL, result = NULL,
    mars = NULL, gam = NULL, data_path = NULL,
    map_version = 0L, precheck = NULL, coltypes = NULL,
    extra_arcs = data.frame(from = character(0), to = character(0),
                            stringsAsFactors = FALSE)
  )

  # -- Data ---------------------------------------------------------------
  observeEvent(input$file, {
    # Everything from here to the end of the handler runs inside tryCatch. A
    # malformed spreadsheet should produce a message, never a dead session.
    loaded <- tryCatch(read_indicator_data(input$file$datapath),
                       error = function(e) e)
    if (inherits(loaded, "error")) {
      message("\n--- PredictLearn: import failed ---")
      message("Message: ", conditionMessage(loaded))
      cl <- conditionCall(loaded)
      if (!is.null(cl)) message("Call:    ", paste(deparse(cl), collapse = " "))
      showNotification(paste0("Could not read the file: ",
                              conditionMessage(loaded),
                              "  --  see the R console for details"),
                       type = "error", duration = NULL)
      return()
    }
    rv$raw <- loaded
    rv$data_path <- input$file$name
    # Constructs are guessed from the item codes on load, so the common case
    # needs no typing at all. Everything stays editable.
    guess <- tryCatch(guess_constructs(rv$raw),
                      error = function(e) rep("", ncol(rv$raw)))
    rv$mapping <- data.frame(
      construct = guess,
      indicator = names(rv$raw),
      mode      = "A",
      stringsAsFactors = FALSE
    )
    rv$map_version <- rv$map_version + 1L

    rv$coltypes <- tryCatch(classify_columns(rv$raw), error = function(e) {
      message("--- PredictLearn: column classification failed: ",
              conditionMessage(e))
      showNotification(paste0("Could not classify the columns: ",
                              conditionMessage(e)),
                       type = "error", duration = NULL)
      NULL
    })

    n_con <- length(unique(guess[nzchar(guess)]))
    showNotification(
      sprintf("Detected %d construct%s from the item codes. Check the summary.",
              n_con, if (n_con == 1L) "" else "s"),
      type = "message", duration = 8)
    for (w in check_guess(guess)) {
      showNotification(w, type = "warning", duration = 15)
    }
  })

  output$preview <- renderDT({
    req(rv$raw)
    datatable(utils::head(rv$raw, 25), options = list(scrollX = TRUE, dom = "tp"),
              rownames = FALSE)
  })

  # The table is redrawn only when the mapping changes wholesale -- a new file,
  # or one of the bulk-fill buttons. Redrawing on every keystroke destroys the
  # DataTable and orphans the one the browser is still paging against, which
  # surfaces as an Ajax error on page 2. Individual edits are read back through
  # the cell_edit event instead; the browser already shows the typed value.
  # -- Column types, checked at import rather than at estimation -----------
  output$column_types <- renderUI({
    cls <- rv$coltypes
    if (is.null(cls)) return(NULL)

    n_num <- sum(cls$type %in% c("numeric", "logical"))
    fixable <- cls[cls$type == "text-numeric", , drop = FALSE]
    words   <- cls[cls$type == "text-categorical", , drop = FALSE]

    blocks <- list(p(class = "eyebrow", "Column types"))

    blocks <- c(blocks, list(div(
      class = if (nrow(fixable) || nrow(words)) "flag" else "ok",
      sprintf("%d of %d columns are numbers.%s", n_num, nrow(cls),
              if (nrow(fixable) || nrow(words))
                " The rest are text and cannot be used as indicators yet." else
                " Nothing needs fixing."))))

    if (nrow(fixable)) {
      blocks <- c(blocks, list(
        p(class = "note",
          strong("Numbers stored as text: "),
          paste(fixable$column, collapse = ", "), ". ",
          if (any(nzchar(fixable$missing_codes)))
            paste0("Values treated as missing: ",
                   paste(unique(unlist(strsplit(
                     fixable$missing_codes[nzchar(fixable$missing_codes)], ", "))),
                     collapse = ", "), ". ") else "",
          "These can be converted safely."),
        actionButton("convert_numeric", "Convert these to numbers",
                     class = "btn-primary")))
    }

    if (nrow(words)) {
      blocks <- c(blocks, list(div(
        class = "flag",
        strong("These columns hold words, not numbers: "),
        paste(sprintf("%s (%s)", words$column, words$text_values),
              collapse = "; "),
        ". They cannot be converted, because doing so would make every value ",
        "missing. Recode them as numbers in your file and load it again, or ",
        "leave them out of the model \u2014 they are already unassigned.")))
    }

    blocks <- c(blocks, list(br(), DTOutput("coltype_table")))
    div(class = "panel", blocks)
  })

  output$coltype_table <- renderDT({
    req(rv$coltypes)
    datatable(rv$coltypes, rownames = FALSE,
              options = list(dom = "t", paging = FALSE,
                             scrollY = "220px", scrollCollapse = TRUE))
  }, server = FALSE)

  observeEvent(input$convert_numeric, {
    req(rv$raw)
    res <- convert_to_numeric(rv$raw)
    rv$raw <- res$data
    rv$coltypes <- classify_columns(rv$raw)

    guess <- guess_constructs(rv$raw)
    rv$mapping <- data.frame(construct = guess, indicator = names(rv$raw),
                             mode = "A", stringsAsFactors = FALSE)
    rv$map_version <- rv$map_version + 1L

    lost <- res$report[res$report$values_set_missing > 0, , drop = FALSE]
    showNotification(
      sprintf("Converted %d column%s. Constructs re-detected.",
              nrow(res$report), if (nrow(res$report) == 1L) "" else "s"),
      type = "message", duration = 8)
    if (nrow(lost)) {
      showNotification(paste0(
        "Values that could not be read became missing: ",
        paste(sprintf("%s (%d)", lost$column, lost$values_set_missing),
              collapse = ", "), "."), type = "warning", duration = 15)
    }
  })

  output$mapping_editor <- renderDT({
    rv$map_version                       # redraw trigger
    m <- isolate(rv$mapping)
    req(m)
    datatable(
      m,
      editable = list(target = "cell", disable = list(columns = 1)),
      rownames = FALSE,
      # Client-side paging: no Ajax round trip, and the whole mapping stays on
      # one scrolling page instead of three.
      options  = list(dom = "t", paging = FALSE,
                      scrollY = "460px", scrollCollapse = TRUE)
    )
  }, server = FALSE)

  observeEvent(input$mapping_editor_cell_edit, {
    info <- input$mapping_editor_cell_edit
    rv$mapping[info$row, info$col + 1L] <- info$value
  })

  observeEvent(input$autogroup, {
    req(rv$mapping)
    req(rv$raw)
    rv$mapping$construct <- guess_constructs(rv$raw)
    rv$map_version <- rv$map_version + 1L
    showNotification("Re-detected from item codes.", type = "message")
  })

  observeEvent(input$clearmap, {
    req(rv$mapping)
    rv$mapping$construct <- ""
    rv$map_version <- rv$map_version + 1L
  })

  # A live tally, so the grouping can be checked without scrolling the table.
  # This redraws on every edit; the table deliberately does not.
  output$mapping_summary <- renderUI({
    req(rv$mapping)
    m <- rv$mapping[nzchar(trimws(rv$mapping$construct)), , drop = FALSE]
    if (!nrow(m)) {
      return(div(class = "flag", "No indicators are assigned to a construct."))
    }
    tb <- sort(table(m$construct), decreasing = TRUE)
    singles <- names(tb)[tb == 1L]
    div(class = "ok",
        HTML(paste0(
          "<strong>", length(tb), " constructs</strong> from ", nrow(m),
          " indicators &mdash; ",
          paste(sprintf("%s (%d)", names(tb), as.integer(tb)),
                collapse = " &middot; "),
          if (length(singles))
            paste0("<br><span style='color:#6A7169'>Single-item: ",
                   paste(singles, collapse = ", "), "</span>") else "")))
  })

  # -- Estimate ------------------------------------------------------------
  observeEvent(input$estimate, {
    req(rv$raw, rv$mapping)
    map <- rv$mapping[nzchar(trimws(rv$mapping$construct)), , drop = FALSE]
    if (!nrow(map)) {
      showNotification("Assign at least one indicator to a construct.",
                       type = "error"); return()
    }
    # Check the data before estimating. A failure inside the PLS algorithm
    # names neither the column nor the construct that caused it.
    pre <- check_indicator_data(rv$raw, map)
    for (w in pre$warnings) showNotification(w, type = "warning", duration = 12)
    if (length(pre$errors)) {
      for (e in pre$errors) showNotification(e, type = "error", duration = NULL)
      rv$precheck <- pre
      return()
    }
    rv$precheck <- pre

    dat <- coerce_indicators(rv$raw)

    withProgress(message = "Estimating measurement model", value = 0.4, {
      res <- tryCatch({
        mm  <- build_measurement_model(map, higher_order = rv$hoc)
        fit <- extract_scores(dat, mm)
        list(fit = fit, map = map)
      }, error = function(e) {
        # Surface the full context in the console; a notification is too small
        # to debug from.
        message("\n--- PredictLearn: estimation failed ---")
        message("Message: ", conditionMessage(e))
        cl <- conditionCall(e)
        if (!is.null(cl)) message("Call:    ", paste(deparse(cl), collapse = " "))
        message("Run traceback() for the full call stack.\n")
        showNotification(
          paste0(conditionMessage(e), "  --  see the R console for details"),
          type = "error", duration = NULL)
        NULL
      })
    })
    req(res)
    rv$fit    <- res$fit
    rv$scores <- res$fit$scores

    cn <- names(rv$scores)
    updateSelectizeInput(session, "hoc_dims",  choices = cn, server = TRUE)
    updateSelectizeInput(session, "targets",   choices = cn, server = TRUE)
    updateSelectizeInput(session, "exogenous", choices = cn, server = TRUE)
    updateSelectizeInput(session, "arc_from", choices = cn, server = TRUE)
    updateSelectizeInput(session, "arc_to",   choices = cn, server = TRUE)
    rv$extra_arcs <- data.frame(from = character(0), to = character(0),
                                stringsAsFactors = FALSE)
    showNotification("Measurement model estimated.", type = "message")
  })

  assessment <- reactive({ req(rv$fit); assess_measurement(rv$fit$model) })

  output$reliability <- renderDT({
    a <- assessment(); req(!is.null(a$reliability))
    datatable(round(a$reliability, 3), options = list(dom = "t"))
  })

  output$assessment_flags <- renderUI({
    pre <- rv$precheck
    if (!is.null(pre) && length(pre$errors)) {
      return(tagList(lapply(pre$errors, function(e) div(class = "flag", e))))
    }
    a <- assessment()
    if (!length(a$flags)) {
      return(div(class = "ok", "No reliability or validity thresholds breached."))
    }
    lapply(a$flags, function(f) div(class = "flag", f))
  })

  output$loadings_table <- renderDT({
    req(rv$fit)
    d <- item_diagnostics(rv$fit$model)
    req(!is.null(d))
    datatable(d, rownames = FALSE,
              options = list(dom = "tp", pageLength = 25, order = list())) |>
      formatStyle("verdict",
                  target = "row",
                  backgroundColor = styleEqual(
                    c("remove", "consider"), c("#FBF0E8", "#FCF8F1")))
  })

  collinearity <- reactive({
    req(rv$scores)
    collinearity_diagnostics(rv$scores)
  })

  output$collinearity_flags <- renderUI({
    f <- collinearity()$flags
    if (!length(f)) {
      return(div(class = "ok",
                 "No construct is redundant with the others."))
    }
    lapply(f, function(x) div(class = "flag", x))
  })

  output$vif_table <- renderDT({
    v <- collinearity()$vif
    req(!is.null(v))
    datatable(v, rownames = FALSE,
              options = list(dom = "tp", pageLength = 25, order = list())) |>
      formatStyle("verdict", target = "row",
                  backgroundColor = styleEqual(
                    c("collinear", "above 3.3", "not estimable"),
                    c("#FBF0E8", "#FCF8F1", "#F2F2F0")))
  })

  output$high_pairs <- renderDT({
    p <- collinearity()$pairs
    req(!is.null(p))
    datatable(p, rownames = FALSE, options = list(dom = "t"))
  })

  output$htmt_table <- renderDT({
    a <- assessment()
    req(!is.null(a$htmt))
    m <- round(as.matrix(a$htmt), 3)
    datatable(as.data.frame(m),
              options = list(dom = "tp", pageLength = 25, scrollX = TRUE)) |>
      formatStyle(colnames(m),
                  backgroundColor = styleInterval(0.85, c("white", "#FBF0E8")))
  })

  # -- Higher-order --------------------------------------------------------
  observeEvent(input$diagnose, {
    req(rv$scores, input$hoc_dims)
    output$hoc_report <- renderPrint({
      d <- tryCatch(diagnose_hoc(rv$scores, input$hoc_dims,
                                 htmt = assessment()$htmt),
                    error = function(e) conditionMessage(e))
      print(d)
    })
  })

  output$hoc_groups <- renderDT({
    req(rv$scores)
    g <- tryCatch(suggest_hoc_groups(rv$scores, k = input$hoc_k),
                  error = function(e) NULL)
    req(g)
    datatable(g[order(g$group), ], options = list(dom = "t", pageLength = 30),
              rownames = FALSE)
  })

  observeEvent(input$add_hoc, {
    req(nzchar(input$hoc_name), length(input$hoc_dims) >= 2)
    rv$hoc[[input$hoc_name]] <- input$hoc_dims
    showNotification(paste0("Added '", input$hoc_name,
                            "'. Re-estimate the measurement model to apply it."),
                     type = "message", duration = 8)
  })

  output$hoc_current <- renderUI({
    if (!length(rv$hoc)) return(NULL)
    div(class = "ok",
        HTML(paste(vapply(names(rv$hoc), function(n)
          paste0("<strong>", n, "</strong> &larr; ",
                 paste(rv$hoc[[n]], collapse = ", ")),
          character(1)), collapse = "<br>")))
  })

  # -- Constraints ---------------------------------------------------------
  # The blacklist is derived from the selections rather than stored, so there
  # is no state to keep in sync and nothing to redraw.

  blacklist <- reactive({
    req(rv$scores)
    cn <- names(rv$scores)
    bl <- data.frame(from = character(0), to = character(0),
                     stringsAsFactors = FALSE)

    for (ex in intersect(input$exogenous, cn)) {
      others <- setdiff(cn, ex)
      if (length(others)) {
        bl <- rbind(bl, data.frame(from = others, to = ex,
                                   stringsAsFactors = FALSE))
      }
    }
    if (nrow(rv$extra_arcs)) bl <- rbind(bl, rv$extra_arcs)

    bl <- unique(bl)
    if (!nrow(bl)) NULL else bl
  })

  output$constraint_summary <- renderUI({
    req(rv$scores)
    cn <- names(rv$scores)
    exo <- intersect(input$exogenous, cn)
    bl <- blacklist()
    n <- if (is.null(bl)) 0L else nrow(bl)
    total <- length(cn) * (length(cn) - 1L)

    if (!length(exo) && !nrow(rv$extra_arcs)) {
      return(div(class = "flag",
        "No constraints set. An unconstrained search over ", length(cn),
        " constructs will return a well-fitting model that means nothing."))
    }
    div(class = "ok",
        HTML(paste0(
          "<strong>", n, " of ", total, " possible arcs forbidden.</strong>",
          if (length(exo)) paste0(
            "<br>Nothing may cause: ", paste(exo, collapse = ", "), ".") else "",
          if (nrow(rv$extra_arcs)) paste0(
            "<br>", nrow(rv$extra_arcs), " individual arc",
            if (nrow(rv$extra_arcs) == 1L) "" else "s", " forbidden.") else "")))
  })

  observeEvent(input$add_arc, {
    req(input$arc_from, input$arc_to)
    if (identical(input$arc_from, input$arc_to)) {
      showNotification("A construct cannot cause itself.", type = "warning")
      return()
    }
    rv$extra_arcs <- unique(rbind(
      rv$extra_arcs,
      data.frame(from = input$arc_from, to = input$arc_to,
                 stringsAsFactors = FALSE)))
    showNotification(paste0("Forbidden: ", input$arc_from, " to ",
                            input$arc_to, "."), type = "message")
  })

  observeEvent(input$clear_arcs, {
    rv$extra_arcs <- data.frame(from = character(0), to = character(0),
                                stringsAsFactors = FALSE)
    showNotification("Individual arcs cleared.", type = "message")
  })

  output$extra_arc_list <- renderUI({
    if (!nrow(rv$extra_arcs)) return(NULL)
    div(class = "ok", style = "font-family:ui-monospace,Menlo,monospace",
        HTML(paste(sprintf("%s &rarr; %s", rv$extra_arcs$from,
                           rv$extra_arcs$to), collapse = "<br>")))
  })

  output$blacklist_table <- renderDT({
    bl <- blacklist()
    if (is.null(bl)) {
      return(datatable(data.frame(note = "No arcs are forbidden yet."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    datatable(bl[order(bl$to, bl$from), ], rownames = FALSE,
              options = list(dom = "tp", pageLength = 15))
  })

  # Both search algorithms assume continuous variables, so anything binary or
  # near-discrete is flagged before the search rather than after it.
  output$varcheck <- renderUI({
    req(rv$scores)
    chk <- check_discovery_variables(rv$scores, input$targets)
    if (!length(chk$errors) && !length(chk$warnings)) return(NULL)
    tagList(
      lapply(chk$errors,   function(e) div(class = "flag", e)),
      lapply(chk$warnings, function(w) div(class = "flag", w))
    )
  })

  # -- Discovery -----------------------------------------------------------
  observeEvent(input$run, {
    req(rv$scores)
    chk <- check_discovery_variables(rv$scores, input$targets)
    if (length(chk$errors)) {
      for (e in chk$errors) showNotification(e, type = "error", duration = NULL)
      return()
    }
    withProgress(message = "Learning structure", value = 0.2, {
      res <- tryCatch({
        s <- learn_structure(rv$scores, algorithm = input$algorithm,
                             blacklist = blacklist(), R = input$boot_R,
                             threshold = input$threshold, seed = input$seed)
        incProgress(0.5, detail = "Cross-validating")
        rmse <- if (length(input$targets)) {
          cv_network_rmse(rv$scores, input$targets, input$algorithm,
                          blacklist(), seed = input$seed)
        } else NULL
        list(structure = s, rmse = rmse)
      }, error = function(e) {
        # Some failures inside bnlearn carry no message at all, which surfaces
        # as an empty notification. Always show something actionable, and put
        # the full context in the console.
        msg <- conditionMessage(e)
        if (!nzchar(trimws(msg))) {
          msg <- paste("The search failed without returning a message.",
                       "This usually means the correlation matrix is singular",
                       "or the sample is too small for the number of",
                       "constructs.")
        }
        message("\n--- PredictLearn: discovery failed ---")
        message("Message: ", conditionMessage(e))
        cl <- conditionCall(e)
        if (!is.null(cl)) message("Call:    ", paste(deparse(cl), collapse = " "))
        message("Constructs: ", ncol(rv$scores), " | rows: ", nrow(rv$scores))
        message("Blacklisted arcs: ",
                if (is.null(blacklist())) 0 else nrow(blacklist()))
        message("Run traceback() for the full call stack.\n")
        showNotification(msg, type = "error", duration = NULL)
        NULL
      })
    })
    req(res)
    rv$result <- res
  })

  output$dag_plot_ui <- renderUI({
    plotOutput("dag_plot",
               height = paste0(input$dag_height %||% 440, "px"))
  })

  output$dag_plot <- renderPlot({
    req(rv$result)
    plot_dag(rv$result$structure$dag,
             main = if (input$algorithm == "gs") "Grow-Shrink" else "Hill-Climbing",
             show_isolated = isTRUE(input$show_isolated))
  })

  output$bic_line <- renderUI({
    req(rv$result)
    div(class = "stat", style = "margin-bottom:10px",
        sprintf("BIC  %.3f   (higher is better under the bnlearn convention)",
                rv$result$structure$bic))
  })

  output$rmse_table <- renderDT({
    req(rv$result$rmse)
    r <- rv$result$rmse
    df <- data.frame(target = names(r), RMSE = round(as.numeric(r), 3))
    used   <- attr(r, "folds_used")
    failed <- attr(r, "folds_failed")
    cap <- if (!is.null(failed) && failed > 0L) {
      sprintf(paste("Averaged over %d of %d folds. %d fold(s) produced a",
                    "structure that could not be oriented and were skipped,",
                    "which itself indicates the structure is unstable under",
                    "resampling."), used, used + failed, failed)
    } else NULL
    datatable(df, options = list(dom = "t"), rownames = FALSE, caption = cap)
  })

  output$new_arcs <- renderDT({
    req(rv$result)
    a <- as.data.frame(bnlearn::arcs(rv$result$structure$dag))
    datatable(a, options = list(dom = "tp", pageLength = 20), rownames = FALSE)
  })

  # -- Non-linearity -------------------------------------------------------
  observeEvent(input$run_nl, {
    req(rv$result, rv$scores)
    eqs <- dag_equations(rv$result$structure$dag)
    if (!length(eqs)) {
      showNotification("The learned structure has no equations to probe.",
                       type = "warning"); return()
    }
    withProgress(message = "Fitting MARS and GAM", value = 0.3, {
      rv$mars <- tryCatch(fit_mars(rv$scores, eqs, seed = input$seed),
                          error = function(e) NULL)
      incProgress(0.5)
      rv$gam  <- tryCatch(fit_gam(rv$scores, eqs), error = function(e) NULL)
    })
  })

  output$mars_table <- renderDT({
    req(rv$mars)
    datatable(summarise_mars(rv$mars), options = list(dom = "t"), rownames = FALSE)
  })

  output$mars_int_table <- renderDT({
    req(rv$mars)
    tab <- mars_interactions(rv$mars)
    if (is.null(tab)) {
      return(datatable(
        data.frame(result = "No interaction terms survived pruning in any equation."),
        rownames = FALSE, options = list(dom = "t")))
    }
    datatable(tab, rownames = FALSE, options = list(dom = "tp"))
  })

  output$gam_table <- renderDT({
    req(rv$gam)
    datatable(summarise_gam(rv$gam), options = list(dom = "tp"), rownames = FALSE)
  })

  output$cv_table <- renderDT({
    req(rv$gam)
    rows <- lapply(names(rv$gam), function(nm) {
      f <- stats::formula(rv$gam[[nm]])
      m <- tryCatch(cv_gam(rv$scores, f, seed = input$seed),
                    error = function(e) NULL)
      if (is.null(m)) return(NULL)
      data.frame(equation = nm,
                 family   = attr(m, "family"),
                 metric_1 = paste0(names(m)[1], " = ", round(m[[1]], 3)),
                 metric_2 = paste0(names(m)[2], " = ", round(m[[2]], 3)),
                 metric_3 = paste0(names(m)[3], " = ", round(m[[3]], 3)),
                 stringsAsFactors = FALSE)
    })
    rows <- do.call(rbind, rows)
    req(!is.null(rows))
    datatable(rows, options = list(dom = "t"), rownames = FALSE)
  })

  output$gam_int_table <- renderDT({
    req(rv$gam, rv$result)
    eqs <- dag_equations(rv$result$structure$dag)
    tab <- tryCatch(test_gam_interactions(rv$scores, eqs),
                    error = function(e) NULL)
    if (is.null(tab)) {
      return(datatable(
        data.frame(result = "No equation has two continuous predictors to test."),
        rownames = FALSE, options = list(dom = "t")))
    }
    datatable(tab, rownames = FALSE, options = list(dom = "tp")) |>
      formatStyle("supported", target = "row",
                  backgroundColor = styleEqual(TRUE, "#EFF4F3"))
  })

  # -- Probe an interaction ------------------------------------------------
  probe_equations <- reactive({
    req(rv$result)
    dag_equations(rv$result$structure$dag)
  })

  observeEvent(probe_equations(), {
    eqs <- probe_equations()
    ok <- names(eqs)[vapply(eqs, length, integer(1)) >= 2L]
    updateSelectInput(session, "probe_target", choices = ok)
  })

  observeEvent(input$probe_target, {
    eqs <- probe_equations()
    preds <- eqs[[input$probe_target]]
    req(length(preds) >= 2L)
    updateSelectInput(session, "probe_focal", choices = preds,
                      selected = preds[1])
    updateSelectInput(session, "probe_mod", choices = preds,
                      selected = preds[2])
  })

  probe <- eventReactive(input$run_probe, {
    req(rv$scores, input$probe_target, input$probe_focal, input$probe_mod)
    if (identical(input$probe_focal, input$probe_mod)) {
      showNotification("Choose two different predictors.", type = "warning")
      return(NULL)
    }
    withProgress(message = "Fitting the interaction", value = 0.5, {
      tryCatch(
        analyse_interaction(rv$scores, probe_equations(),
                            target = input$probe_target,
                            focal = input$probe_focal,
                            moderator = input$probe_mod),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        })
    })
  })

  output$probe_plot <- renderPlot({
    p <- probe(); req(!is.null(p))
    plot_interaction(p)
  })

  output$probe_slopes <- renderDT({
    p <- probe(); req(!is.null(p))
    datatable(p$slopes, rownames = FALSE, options = list(dom = "t"))
  })

  output$nl_test <- renderDT({
    req(rv$gam)
    datatable(test_nonlinearity(rv$scores, rv$gam), options = list(dom = "t"),
              rownames = FALSE)
  })

  output$pdp_plot <- renderPlot({
    req(rv$gam)
    n <- length(rv$gam)
    op <- par(mfrow = c(1, min(n, 3)), mar = c(4, 4, 2, 1))
    on.exit(par(op))
    for (nm in names(rv$gam)) {
      plot(rv$gam[[nm]], se = TRUE, shade = TRUE, col = "#2D5F5D",
           shade.col = "#EFF4F3", main = nm, select = 1)
    }
  })

  # -- Export --------------------------------------------------------------
  output$dl_script <- downloadHandler(
    filename = function() "predictlearn_analysis.R",
    content = function(file) {
      export_script(
        path = file,
        data_file = rv$data_path %||% "your_data.csv",
        mapping = rv$mapping[nzchar(trimws(rv$mapping$construct)), , drop = FALSE],
        higher_order = rv$hoc,
        blacklist = blacklist(),
        algorithm = input$algorithm,
        targets = input$targets,
        seed = input$seed
      )
    }
  )

  output$dl_scores <- downloadHandler(
    filename = function() "latent_variable_scores.csv",
    content = function(file) {
      req(rv$scores)
      utils::write.csv(rv$scores, file, row.names = FALSE)
    }
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
