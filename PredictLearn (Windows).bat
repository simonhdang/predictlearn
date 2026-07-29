@echo off
REM ============================================================================
REM PredictLearn launcher for Windows
REM
REM Double-click to run. Requires R, free from https://cran.r-project.org
REM RStudio is NOT needed.
REM ============================================================================

REM ---- EDIT THIS LINE: your GitHub username and repository -------------------
set GITHUB_REPO=simonhdang/predictlearn
REM ----------------------------------------------------------------------------

where Rscript >nul 2>nul
if errorlevel 1 (
  echo.
  echo   R was not found on this computer.
  echo.
  echo   PredictLearn needs R, which is free: https://cran.r-project.org
  echo   Install it, then double-click this file again.
  echo   You do NOT need RStudio.
  echo.
  pause
  exit /b 1
)

echo Starting PredictLearn. Leave this window open while you work.
echo.

Rscript --no-save -e "repo <- '%GITHUB_REPO%'; need <- c('shiny','DT','seminr','bnlearn','earth','mgcv','caret','igraph','readxl','remotes'); miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]; if (length(miss)) { message('First run: installing ', length(miss), ' packages. This takes a few minutes.'); install.packages(miss, repos='https://cloud.r-project.org') }; if (!requireNamespace('predictlearn', quietly=TRUE)) remotes::install_github(repo, upgrade='never'); predictlearn::run_app()"

pause
