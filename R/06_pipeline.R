# ==========================================================
# 06_pipeline.R - run everything and write the outputs
# ==========================================================

run_analyses <- function(pbp, cfg) {
  list(
    overview          = analyze_overview(pbp),
    personnel         = analyze_personnel(pbp),
    down_distance     = analyze_down_distance(pbp),
    field_zone        = analyze_field_zone(pbp),
    alignment         = analyze_alignment(pbp),
    alignment_pers    = analyze_alignment_personnel(pbp, cfg),
    run_concepts      = analyze_run_concepts(pbp),
    third_down        = analyze_third_down(pbp),
    third_down_tgts   = analyze_third_down_targets(pbp),
    receivers         = analyze_receivers(pbp),
    rushers           = analyze_rushers(pbp),
    player_impact     = analyze_player_impact(pbp),
    pass_context      = analyze_pass_context(pbp),
    depth             = analyze_depth(pbp),
    tells             = analyze_tells(pbp, cfg),
    predictability    = analyze_predictability(pbp)
  )
}

make_plots <- function(res, cfg) {
  list(
    personnel         = plot_personnel(res$personnel, cfg),
    down_distance     = plot_down_distance(res$down_distance, cfg),
    alignment         = plot_alignment(res$alignment, cfg),
    run_concepts      = plot_run_concepts(res$run_concepts, cfg),
    third_down        = plot_third_down(res$third_down, cfg),
    third_down_tgts   = plot_third_down_targets(res$third_down_tgts, cfg),
    receivers         = plot_receivers(res$receivers, cfg),
    receiver_style    = plot_receiver_style(res$receivers, cfg),
    rushers           = plot_rushers(res$rushers, cfg),
    tells             = plot_tells(res$tells, cfg)
  )
}

# Find a Chrome / Edge / Chromium binary for HTML -> PDF printing.
find_browser <- function() {
  env <- Sys.getenv(c("CHROME_PATH", "PAGEDOWN_CHROME"))
  cands <- c(
    env[nzchar(env)],
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"),
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    unname(Sys.which(c("google-chrome", "google-chrome-stable", "chromium", "chromium-browser", "msedge"))),
    Sys.glob("/opt/pw-browsers/chromium-*/chrome-linux/chrome")
  )
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (length(cands) == 0) NA_character_ else cands[1]
}

html_to_pdf <- function(html, pdf, browser = find_browser()) {
  if (is.na(browser)) {
    warning("No Chrome/Edge found, so the PDF was skipped. Set CHROME_PATH to your browser to enable it.",
            call. = FALSE)
    return(invisible(NA_character_))
  }
  url <- paste0("file:///", sub("^/", "", normalizePath(html, winslash = "/")))
  args <- c("--headless", "--disable-gpu", "--no-sandbox", "--no-pdf-header-footer",
            "--run-all-compositor-stages-before-draw", "--virtual-time-budget=10000",
            paste0("--print-to-pdf=", shQuote(normalizePath(pdf, winslash = "/", mustWork = FALSE))),
            shQuote(url))
  status <- suppressWarnings(system2(browser, args, stdout = FALSE, stderr = FALSE))
  if (!file.exists(pdf)) {
    warning("PDF printing failed (exit status ", status, ").", call. = FALSE)
    return(invisible(NA_character_))
  }
  invisible(pdf)
}

save_outputs <- function(res, plots, out_dir, team) {
  dir.create(file.path(out_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "charts"), recursive = TRUE, showWarnings = FALSE)

  for (nm in names(res)) {
    utils::write.csv(res[[nm]], file.path(out_dir, "tables", paste0(team, "_", nm, ".csv")), row.names = FALSE)
  }
  if (requireNamespace("writexl", quietly = TRUE)) {
    sheets <- res[vapply(res, function(x) nrow(x) > 0, logical(1))]
    names(sheets) <- substr(names(sheets), 1, 31)
    writexl::write_xlsx(sheets, file.path(out_dir, paste0(team, "_scouting_tables.xlsx")))
  }
  sizes <- list(down_distance = c(10, 4.8), third_down_tgts = c(11, 6.5), tells = c(10, 6))
  for (nm in names(plots)) {
    wh <- if (!is.null(sizes[[nm]])) sizes[[nm]] else c(9, 5.5)
    ggplot2::ggsave(file.path(out_dir, "charts", paste0(team, "_", nm, ".png")), plots[[nm]],
                    width = wh[1], height = wh[2], dpi = 200, bg = "white")
  }
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------
# file     : path to .xlsx/.csv in GamePBP format
# team     : club code on offense (e.g. "SF"); optional if file has one team
# out_dir  : where outputs go (a team sub-folder is created)
# formats  : any of "html", "pdf"
# config   : list of overrides for default_config(), e.g. list(mock_data = TRUE)
build_scouting_report <- function(file, team = NULL, out_dir = "output",
                                  formats = c("html", "pdf"), config = list(),
                                  quiet = TRUE) {
  cfg <- merge_config(config)
  t0 <- Sys.time()

  loaded <- load_pbp(file, sheet = cfg$sheet)
  team <- resolve_team(loaded$data, team)
  if (is.null(cfg$team_name)) {
    cfg$team_name <- if (team %in% names(TEAM_NAMES)) TEAM_NAMES[[team]] else team
  }
  pal <- if (team %in% names(TEAM_COLORS)) TEAM_COLORS[[team]] else c("#2B6CB0", "#C05621")
  if (is.null(cfg$color_pass)) cfg$color_pass <- pal[1]
  if (is.null(cfg$color_run)) cfg$color_run <- pal[2]
  if (isTRUE(cfg$mock_data)) cfg$team_name <- paste(cfg$team_name, "(MOCK)")
  pbp <- prepare_pbp(loaded$data, team, cfg)
  named <- apply_roster(pbp, team, cfg)
  pbp <- named$pbp
  loaded$issues <- c(loaded$issues, named$issues)
  res <- run_analyses(pbp, cfg)
  res$insights <- build_insights(res, cfg)
  plots <- make_plots(res, cfg)

  team_dir <- file.path(out_dir, team)
  dir.create(team_dir, recursive = TRUE, showWarnings = FALSE)
  save_outputs(res, plots, team_dir, team)

  report_env <- list(
    res = res, plots = plots, cfg = cfg, team = team, pbp = pbp,
    issues = loaded$issues, source_file = basename(file),
    source_md5 = unname(tools::md5sum(file))
  )
  rds <- file.path(team_dir, paste0(team, "_results.rds"))
  saveRDS(report_env[c("res", "cfg", "team", "issues", "source_file", "source_md5")], rds)

  html <- NA_character_; pdf <- NA_character_
  if (any(c("html", "pdf") %in% formats)) {
    template <- file.path(pipeline_root(), "report", "scouting_report.Rmd")
    html <- file.path(normalizePath(team_dir), paste0(team, "_scouting_report.html"))
    rmarkdown::render(
      template, output_file = basename(html), output_dir = dirname(html),
      intermediates_dir = tempdir(), knit_root_dir = tempdir(),
      params = list(report = report_env), envir = new.env(), quiet = quiet
    )
    if ("pdf" %in% formats) {
      pdf <- html_to_pdf(html, sub("\\.html$", ".pdf", html))
    }
  }

  message(sprintf("%s: %d plays, %d games -> %s (%.1fs)", team, nrow(pbp),
                  res$overview$games, team_dir, as.numeric(Sys.time() - t0, units = "secs")))
  invisible(list(team = team, results = res, plots = plots, html = html, pdf = pdf,
                 out_dir = team_dir, issues = loaded$issues))
}

# ---------------------------------------------------------------------------
# Cross-team summary page from saved results (output/<TEAM>/<TEAM>_results.rds)
# ---------------------------------------------------------------------------
build_summary <- function(teams, out_dir = "output", formats = c("html", "pdf")) {
  results <- lapply(teams, function(tm) readRDS(file.path(out_dir, tm, paste0(tm, "_results.rds"))))
  html <- file.path(normalizePath(out_dir), "00_team_summary.html")
  rmarkdown::render(file.path(pipeline_root(), "report", "summary.Rmd"),
                    output_file = basename(html), output_dir = dirname(html),
                    intermediates_dir = tempdir(), knit_root_dir = tempdir(),
                    params = list(results = results), envir = new.env(parent = globalenv()), quiet = TRUE)
  if ("pdf" %in% formats) html_to_pdf(html, sub("\\.html$", ".pdf", html))
  invisible(html)
}
