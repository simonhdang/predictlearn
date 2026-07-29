#!/bin/bash
# =============================================================================
# PredictLearn launcher for macOS
#
# Double-click to run. Requires R, which is free from https://cran.r-project.org
# RStudio is NOT needed.
# =============================================================================

# ---- EDIT THIS LINE: your GitHub username and repository --------------------
GITHUB_REPO="simonhdang/predictlearn"
# -----------------------------------------------------------------------------

RBIN=$(command -v Rscript)
if [ -z "$RBIN" ]; then
  for p in /usr/local/bin/Rscript /opt/homebrew/bin/Rscript \
           /Library/Frameworks/R.framework/Resources/bin/Rscript; do
    [ -x "$p" ] && RBIN="$p" && break
  done
fi

if [ -z "$RBIN" ]; then
  osascript -e 'display alert "R is not installed" message "PredictLearn needs R, which is free.

Download it from cran.r-project.org, install it, then double-click this file again.

You do not need RStudio."'
  exit 1
fi

echo "Starting PredictLearn. Leave this window open while you work."
echo ""

"$RBIN" --no-save -e "
  repo <- '$GITHUB_REPO'
  need <- c('shiny','DT','seminr','bnlearn','earth','mgcv','caret','igraph','readxl','remotes')
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    message('First run: installing ', length(miss), ' packages. This takes a few minutes.')
    install.packages(miss, repos = 'https://cloud.r-project.org')
  }
  if (!requireNamespace('predictlearn', quietly = TRUE)) {
    remotes::install_github(repo, upgrade = 'never')
  }
  predictlearn::run_app()
"
