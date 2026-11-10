# ==========================================================
# tests/stress_test.R - break the pipeline on purpose
# ==========================================================
# Run from the pipeline folder:   Rscript tests/stress_test.R
# Writes tests/stress_test_results.csv and prints a pass/fail summary.

source("scouting.R")
suppressPackageStartupMessages(library(dplyr))

out_root <- file.path(tempdir(), "stress")
dir.create(out_root, showWarnings = FALSE)
results <- list()

check <- function(name, expr, expect_error = NULL) {
  t0 <- Sys.time()
  status <- tryCatch({
    val <- force(expr)
    if (!is.null(expect_error)) "FAIL (expected an error, got none)" else if (isFALSE(val)) "FAIL" else "PASS"
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (!is.null(expect_error) && grepl(expect_error, msg, ignore.case = TRUE)) "PASS" else paste("FAIL:", msg)
  })
  secs <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
  results[[length(results) + 1]] <<- data.frame(test = name, status = status, seconds = secs)
  cat(sprintf("[%s] %s (%.1fs)\n", ifelse(startsWith(status, "PASS"), "PASS", "FAIL"), name, secs))
  if (!startsWith(status, "PASS")) cat("      ", status, "\n")
}

# Invariants every result set must satisfy.
check_invariants <- function(r) {
  res <- r$results; ov <- res$overview
  stopifnot(
    ov$runs + ov$dropbacks == ov$plays,
    abs(sum(res$personnel$share_of_snaps) - 1) < 1e-9,
    sum(res$personnel$total_plays) <= ov$plays,
    !"Unknown" %in% res$personnel$personnel,
    sum(res$alignment$total_plays) == ov$plays,
    all(res$personnel$pass_rate >= 0 & res$personnel$pass_rate <= 1),
    nrow(res$run_concepts) == 0 || sum(res$run_concepts$carries) == ov$runs,
    nrow(res$receivers) == 0 || abs(sum(res$receivers$target_share) - 1) < 1e-9,
    nrow(res$receivers) == 0 || all(res$receivers$receptions <= res$receivers$targets),
    all(is.na(res$tells$lean_rate) | res$tells$lean_rate >= 0.5),
    nrow(res$insights) >= 5
  )
  TRUE
}

src <- "mock/MOCK_SF_GamePBP.xlsx"   # base file for the edge cases (mock data)
teams <- c(NYG = "mock/MOCK_NYG_GamePBP.xlsx", CLV = "mock/MOCK_CLV_GamePBP.xlsx",
           HST = "mock/MOCK_HST_GamePBP.xlsx", SF = src)
mk <- list(mock_data = TRUE)

# ---- 1. Full builds: every team, HTML + PDF --------------------------------
for (tm in names(teams)) {
  check(paste("Full build (HTML+PDF) -", tm), {
    r <- build_scouting_report(teams[[tm]], team = tm, out_dir = "output", config = mk)
    stopifnot(file.exists(r$html), !is.na(r$pdf) && file.exists(r$pdf),
              length(list.files(file.path(r$out_dir, "charts"))) == 10)
    check_invariants(r)
  })
}

# ---- 2. Real public data (nflverse CSVs shipped in data/nflverse) -----------
for (f in Sys.glob("data/nflverse/*_2025_nflverse_GamePBP.csv")) {
  tm <- sub("_.*", "", basename(f))
  check(paste("nflverse CSV builds -", tm), {
    check_invariants(rebuild_nflverse_report(tm, out_dir = out_root, formats = character()))
  })
}
base_plays <- readRDS("output/SF/SF_results.rds")$res$overview$plays

# ---- 3. Input-format edge cases --------------------------------------------
raw <- readxl::read_excel(src, sheet = "GamePBP", col_types = "text")

check("CSV input instead of Excel", {
  f <- file.path(out_root, "sf.csv"); readr::write_csv(raw, f)
  check_invariants(build_scouting_report(f, out_dir = out_root, formats = character()))
})

check("Messy headers (UPPER CASE, spaces, dashes)", {
  d <- raw; names(d) <- gsub("_", " ", toupper(names(d))); names(d)[5] <- "pff-CLOCK"
  f <- file.path(out_root, "messy.csv"); readr::write_csv(d, f)
  check_invariants(build_scouting_report(f, out_dir = out_root, formats = character()))
})

check("Different sheet name is reported clearly", expect_error = "Sheet 'GamePBP' not found", {
  f <- file.path(out_root, "renamed.xlsx"); writexl::write_xlsx(list(Plays = raw), f)
  build_scouting_report(f, out_dir = out_root, formats = character())
})

check("Custom sheet name via config works", {
  f <- file.path(out_root, "renamed.xlsx")
  check_invariants(build_scouting_report(f, out_dir = out_root, formats = character(), config = list(sheet = "Plays")))
})

check("Multi-team file without team -> clear error", expect_error = "several offenses", {
  both <- bind_rows(raw, readxl::read_excel(teams[["NYG"]], sheet = "GamePBP", col_types = "text"))
  f <- file.path(out_root, "two_teams.csv"); readr::write_csv(both, f)
  build_scouting_report(f, out_dir = out_root, formats = character())
})

check("Multi-team file with team = 'SF'", {
  r <- build_scouting_report(file.path(out_root, "two_teams.csv"), team = "SF", out_dir = out_root,
                             formats = "html", config = list(mock_data = TRUE))
  check_invariants(r) && r$results$overview$team == "SF"
})

check("Unknown team code -> clear error", expect_error = "not found on offense", {
  build_scouting_report(src, team = "PIT", out_dir = out_root, formats = character())
})

check("Missing required column -> clear error", expect_error = "required columns are missing", {
  f <- file.path(out_root, "noreq.csv"); readr::write_csv(select(raw, -pff_DOWN), f)
  build_scouting_report(f, out_dir = out_root, formats = character())
})

check("Missing optional columns (concepts, targets, pressure, EPA) still renders", {
  d <- select(raw, -pff_RUNCONCEPTPRIMARY, -pff_PASSRECEIVERTARGET, -pff_TIMETOPRESSURE,
              -pff_EXPECTEDPOINTSADDED, -pff_PISTOL)
  f <- file.path(out_root, "noopt.csv"); readr::write_csv(d, f)
  r <- build_scouting_report(f, out_dir = out_root, formats = "html")
  file.exists(r$html) && length(r$issues) > 0
})

check("One game only (tiny sample)", {
  d <- filter(raw, pff_GSISGAMEKEY == pff_GSISGAMEKEY[1])
  f <- file.path(out_root, "onegame.csv"); readr::write_csv(d, f)
  r <- build_scouting_report(f, out_dir = out_root, formats = "html")
  file.exists(r$html) && r$results$overview$games == 1
})

check("Ten plays only", {
  f <- file.path(out_root, "ten.csv"); readr::write_csv(head(raw, 10), f)
  file.exists(build_scouting_report(f, out_dir = out_root, formats = "html")$html)
})

check("Pass-only offense (no runs at all)", {
  f <- file.path(out_root, "passonly.csv"); readr::write_csv(filter(raw, pff_RUNPASS != "R"), f)
  r <- build_scouting_report(f, out_dir = out_root, formats = "html")
  file.exists(r$html) && r$results$overview$runs == 0
})

check("Run-only offense (no passes at all)", {
  f <- file.path(out_root, "runonly.csv"); readr::write_csv(filter(raw, pff_RUNPASS != "P"), f)
  r <- build_scouting_report(f, out_dir = out_root, formats = "html")
  file.exists(r$html) && r$results$overview$dropbacks == 0
})

check("All under center (no shotgun / pistol)", {
  d <- mutate(raw, pff_SHOTGUN = NA, pff_PISTOL = NA)
  f <- file.path(out_root, "uc.csv"); readr::write_csv(d, f)
  r <- build_scouting_report(f, out_dir = out_root, formats = "html")
  nrow(r$results$alignment) == 1
})

check("Junk values in numeric columns are flagged, not fatal", {
  d <- raw; d$pff_GAINLOSS[1:5] <- c("five", "", "NULL", "n/a", "7 yds")
  f <- file.path(out_root, "junk.csv"); readr::write_csv(d, f)
  r <- build_scouting_report(f, out_dir = out_root, formats = character())
  any(grepl("pff_gainloss", r$issues))
})

check("Flags stored as T/F text instead of 0/1", {
  d <- mutate(raw, pff_GARBAGETIME = ifelse(pff_GARBAGETIME == "1", "T", "F"),
              pff_PLAYACTION = ifelse(pff_PLAYACTION == "1", "TRUE", "FALSE"))
  f <- file.path(out_root, "tf.csv"); readr::write_csv(d, f)
  r <- build_scouting_report(f, out_dir = out_root, formats = character())
  r$results$overview$plays == base_plays
})

check("Duplicate rows do not crash", {
  f <- file.path(out_root, "dup.csv"); readr::write_csv(bind_rows(raw, raw[1:50, ]), f)
  check_invariants(build_scouting_report(f, out_dir = out_root, formats = character()))
})

# ---- 4. Config switches -----------------------------------------------------
check("Config: merge '(*)' personnel tags", {
  r <- build_scouting_report(src, out_dir = out_root, formats = character(), config = list(merge_personnel_tags = TRUE))
  !any(grepl("\\(\\*\\)", r$results$personnel$personnel))
})
check("Config: PFF success flag as headline", {
  r <- build_scouting_report(src, out_dir = out_root, formats = character(), config = list(primary_success = "pff"))
  check_invariants(r)
})
check("Config: keep garbage time and kneels", {
  r <- build_scouting_report(src, out_dir = out_root, formats = character(),
                             config = list(exclude_garbage = FALSE, exclude_kneels = FALSE))
  r$results$overview$plays > base_plays
})
check("Config: unknown option -> clear error", expect_error = "Unknown config option", {
  build_scouting_report(src, out_dir = out_root, formats = character(), config = list(colour = "red"))
})

# ---- 5. Mock-data sanity: the generator's personalities come through --------
check("Mock SF is the most under-center / run-heavy offense", {
  ov <- lapply(c("NYG", "CLV", "HST", "SF"), function(t) readRDS(file.path("output", t, paste0(t, "_results.rds")))$res)
  pr <- vapply(ov, function(x) x$overview$pass_rate, 0)
  which.min(pr) == 4
})

res_df <- bind_rows(results)
write.csv(res_df, "tests/stress_test_results.csv", row.names = FALSE)
cat(sprintf("\n%d / %d tests passed\n", sum(startsWith(res_df$status, "PASS")), nrow(res_df)))
