# ==========================================================
# run_nflverse.R - real-data reports from public nflverse data
# ==========================================================
# Rscript run_nflverse.R            (2025 season, regular + postseason)
# Rscript run_nflverse.R 2026       (another season; see README for gaps)
source("scouting.R")
args <- commandArgs(trailingOnly = TRUE)
season <- if (length(args)) as.integer(args[1]) else 2025
teams <- c("NYG", "CLE", "HOU", "SF")
d <- nflverse_season_data(season)
for (tm in teams) build_nflverse_report(tm, season, data = d, out_dir = "output_nflverse")
message("Done. Reports in output_nflverse/<TEAM>/")
build_summary(to_pff_code(teams), out_dir = "output_nflverse")
message("Cross-team page: output_nflverse/00_team_summary.html")
