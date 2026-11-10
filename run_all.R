# ==========================================================
# run_all.R - rebuild the mock-data demo
# ==========================================================
# Rscript run_all.R   (working directory = this folder)
#  1. regenerates the four MOCK files (fixed seeds, so results repeat)
#  2. builds HTML + PDF reports for NYG, CLV, HST, SF (mock) in output/
#  3. builds the cross-team summary page in output/
#
# To scout a team from your own PFF GamePBP export:
#   source("scouting.R"); build_scouting_report("path/to/file.xlsx", team = "BUF")
# Real public data for any team: see run_nflverse.R.

source("scouting.R")
source("mock/generate_mock_data.R")

generate_all_mocks(out_dir = "mock")

mock <- c(NYG = "mock/MOCK_NYG_GamePBP.xlsx", CLV = "mock/MOCK_CLV_GamePBP.xlsx",
          HST = "mock/MOCK_HST_GamePBP.xlsx", SF = "mock/MOCK_SF_GamePBP.xlsx")

invisible(build_many(mock, out_dir = "output", config = list(mock_data = TRUE)))
build_summary(names(mock), out_dir = "output")
message("Done. Open output/00_team_summary.html")
