# Installing PredictLearn

PredictLearn runs on your own computer. Your data is never uploaded anywhere.

## Step 1 — install R

Download R from <https://cran.r-project.org> and install it. It is free and
open source. **You do not need RStudio.**

- Windows: click "Download R for Windows", then "base", then the download link
- macOS: click "Download R for macOS" and choose the version matching your chip
  (Apple silicon or Intel)

## Step 2 — launch

Double-click the launcher for your system:

- **Windows** — `PredictLearn (Windows).bat`
- **macOS** — `PredictLearn (Mac).command`

The first launch installs the packages PredictLearn needs and takes a few
minutes. Later launches are quick.

PredictLearn opens in your web browser. The address will start with
`127.0.0.1`, which means it is running on your own machine and not on the
internet.

To quit, close the browser tab and the black window behind it.

### macOS: "cannot be opened because it is from an unidentified developer"

Right-click the `.command` file, choose **Open**, then **Open** again. You only
need to do this once.

If the file will not run at all, open Terminal and paste:

    chmod +x "/path/to/PredictLearn (Mac).command"

dragging the file into the Terminal window to fill in the path.

## Alternative — from the R console

If you already use R, skip the launchers:

```r
install.packages("remotes")
remotes::install_github("simonhdang/predictlearn")
predictlearn::run_app()
```
