# install_packages.R - one-time setup (R 4.1 or newer)
# Rscript install_packages.R
pkgs <- c(
  "dplyr", "tidyr", "readr", "readxl", "writexl", "forcats", "scales",   # data
  "ggplot2",                                                              # charts
  "rmarkdown", "knitr", "htmltools",                                      # reports
  "nflreadr"                                                              # free nflverse data
)
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("All packages ready. Also needed: pandoc (bundled with RStudio) and Chrome or Edge for PDFs.")
