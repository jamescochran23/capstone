# ==========================================================
# scouting.R - load the offensive scouting pipeline
# ==========================================================
# Usage from R / RStudio (working directory = this folder):
#
#   source("scouting.R")
#   build_scouting_report("data/nflverse/SF_2025_nflverse_GamePBP.csv", team = "SF")
#
# Usage from a terminal:
#
#   Rscript scouting.R path/to/GamePBP.xlsx BUF output
#   Rscript scouting.R mock/MOCK_NYG_GamePBP.xlsx NYG output --mock
#
# Outputs land in output/<TEAM>/: an HTML report, a PDF report, every table
# as CSV plus one Excel workbook, and every chart as PNG.

local({
  needed <- c("readxl", "readr", "dplyr", "tidyr", "ggplot2", "scales", "forcats",
              "rmarkdown", "knitr", "writexl")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing: ", paste(missing, collapse = ", "))
    utils::install.packages(missing, repos = "https://cloud.r-project.org")
  }
})

.scouting_root <- local({
  # Find this file's folder whether it was source()d (any depth) or run by Rscript.
  ofiles <- as.character(unlist(lapply(sys.frames(), function(f) if (!is.null(f$ofile)) f$ofile)))
  ofiles <- ofiles[basename(ofiles) == "scouting.R"]
  this <- if (length(ofiles)) ofiles[length(ofiles)] else {
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", args[grepl("^--file=", args)])
    if (length(f) && basename(f) == "scouting.R") f else "scouting.R"
  }
  dirname(normalizePath(this, mustWork = FALSE))
})
pipeline_root <- function() .scouting_root

for (.f in sort(list.files(file.path(.scouting_root, "R"), pattern = "\\.R$", full.names = TRUE))) {
  source(.f, local = FALSE)
}
rm(.f)

# Run several teams in one go. `files` is a named vector: c(BUF = "path.xlsx").
build_many <- function(files, out_dir = "output", formats = c("html", "pdf"), config = list()) {
  lapply(names(files), function(tm) {
    tryCatch(build_scouting_report(files[[tm]], team = tm, out_dir = out_dir,
                                   formats = formats, config = config),
             error = function(e) {
               message("FAILED ", tm, ": ", conditionMessage(e))
               NULL
             })
  }) |> stats::setNames(names(files))
}

# ---- Command-line entry point ---------------------------------------------
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    cat("Usage: Rscript scouting.R <file.xlsx|csv> [TEAM] [OUT_DIR] [--mock] [--html-only]\n")
    quit(status = 1)
  }
  flags <- args[grepl("^--", args)]
  pos <- args[!grepl("^--", args)]
  build_scouting_report(
    file = pos[1],
    team = if (length(pos) >= 2) pos[2] else NULL,
    out_dir = if (length(pos) >= 3) pos[3] else "output",
    formats = if ("--html-only" %in% flags) "html" else c("html", "pdf"),
    config = if ("--mock" %in% flags) list(mock_data = TRUE) else list()
  )
}
