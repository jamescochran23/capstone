# ==========================================================
# 07_nflverse.R - run the pipeline on free nflverse data
# ==========================================================
# The PFF GamePBP file is the pipeline's native input. This file adds a
# second door in: it downloads public nflverse data with {nflreadr}, maps it
# onto the same GamePBP columns, writes a CSV, and hands that CSV to
# build_scouting_report(). Nothing downstream changes.
#
#   source("scouting.R")
#   build_nflverse_report("SF", season = 2025)
#   build_nflverse_report("CLE", season = 2025)   # nflverse or PFF club codes both work
#
# Sources joined (all by game + play id):
#   load_pbp()            play-by-play: down, distance, yard line, result, EPA, players
#   load_participation()  personnel, formation, coverage, route, pressure, rushers
#   load_ftn_charting()   QB alignment, play action, screen, RPO, catchable, throwaways
#   load_rosters_weekly() names, jersey numbers, roster positions
#
# What nflverse cannot supply is set to NA and the report says so (see
# NFLVERSE_GAPS below and the report's Methodology section).

# nflverse club code -> PFF club code (TEAM_COLORS / TEAM_NAMES use PFF codes).
NFLVERSE_TO_PFF <- c(ARI = "ARZ", BAL = "BLT", CLE = "CLV", HOU = "HST", LA = "LA", LAR = "LA")

to_pff_code <- function(x) ifelse(x %in% names(NFLVERSE_TO_PFF), NFLVERSE_TO_PFF[x], x)
to_nflverse_code <- function(x) {
  back <- stats::setNames(names(NFLVERSE_TO_PFF), NFLVERSE_TO_PFF)
  back <- back[!duplicated(names(back))]
  ifelse(x %in% names(back), back[x], x)
}

# One line per prompt item / column that nflverse covers only partly or not at all.
NFLVERSE_GAPS <- c(
  "Run concepts: nflverse has no blocking-scheme charting (inside zone, duo, power ...). Section 4 groups runs by gap and direction instead (e.g. 'Left Tackle', 'Middle').",
  "PFF success codes (pff_OFFSUCCESS): not in nflverse. Success uses the standard 40/60/100 yardage rule only.",
  "EPA / EP: nflfastR's public model, not PFF's. Values are close in spirit but not identical.",
  "Yards after contact: not in nflverse, so the before/after-contact split for rushers is unavailable (yards per carry is shown instead).",
  "Receiver alignment (slot / wide / backfield): not in nflverse. 'Main align' is blank.",
  "Personnel is counted from the roster positions of the 11 players on the field (RB+FB, TE). A WR listed as TE on the roster counts as a TE. '(*)' = 6+ offensive linemen.",
  "Garbage time is approximated with win probability (second half, pre-snap WP below 0.01% or above 99.99%). Calibrated against a licensed PFF sample.",
  "2-minute flag: inside the final 2:00 of either half (PFF's 4-minute flag has no equivalent).",
  "Pressure is NGS 'was_pressure' (yes/no); time to pressure is not available."
)

# ---------------------------------------------------------------------------
# Download (cached per season in data/nflverse/cache/)
# ---------------------------------------------------------------------------
nflverse_season_data <- function(season, cache_dir = file.path(pipeline_root(), "data", "nflverse", "cache"),
                                 refresh = FALSE) {
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    message("Installing nflreadr ...")
    utils::install.packages("nflreadr", repos = "https://cloud.r-project.org")
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  get <- function(name, fun) {
    f <- file.path(cache_dir, sprintf("%s_%d.rds", name, season))
    if (file.exists(f) && !refresh) return(readRDS(f))
    x <- tryCatch(as.data.frame(fun(season)), error = function(e) {
      warning(name, " ", season, " could not be downloaded: ", conditionMessage(e), call. = FALSE)
      data.frame()
    })
    if (nrow(x) > 0) saveRDS(x, f)
    x
  }
  list(
    season = season,
    pbp = get("pbp", nflreadr::load_pbp),
    participation = get("participation", nflreadr::load_participation),
    ftn = get("ftn", nflreadr::load_ftn_charting),
    rosters = get("rosters", nflreadr::load_rosters_weekly)
  )
}

# ---------------------------------------------------------------------------
# Helpers for list-like participation fields ("C;G;G;QB;RB;T;T;TE;WR;WR;WR")
# ---------------------------------------------------------------------------
count_pos <- function(pos_str, set) {
  vapply(strsplit(dplyr::coalesce(pos_str, ""), ";", fixed = TRUE),
         function(p) sum(p %in% set), integer(1))
}

personnel_from_positions <- function(pos_str) {
  n <- lengths(strsplit(dplyr::coalesce(pos_str, ""), ";", fixed = TRUE))
  rb <- count_pos(pos_str, c("RB", "FB", "HB"))
  te <- count_pos(pos_str, "TE")
  ol <- count_pos(pos_str, c("C", "G", "T", "OL", "OT", "OG"))
  defn <- count_pos(pos_str, c("CB", "S", "SS", "FS", "DB", "ILB", "OLB", "MLB", "LB", "DE", "DT", "NT", "DL"))
  out <- paste0(rb, te, ifelse(ol >= 6, " (*)", ""))
  out[is.na(pos_str) | pos_str == "" | n != 11 | rb > 9 | te > 9 | defn > 1] <- NA
  out
}

def_package_from_positions <- function(pos_str) {
  db <- count_pos(pos_str, c("CB", "S", "SS", "FS", "DB"))
  dplyr::case_when(
    is.na(pos_str) | pos_str == "" ~ NA_character_,
    db <= 3 ~ "HEAVY",
    db == 4 ~ "BASE",
    db == 5 ~ "NICKEL",
    db == 6 ~ "DIME",
    TRUE ~ "QUARTER"
  )
}

nz <- function(x) { x <- trimws(as.character(x)); x[x %in% c("", "NA")] <- NA; x }
flag <- function(x) ifelse(is.na(x), NA_real_, as.numeric(as.logical(x)))

# ---------------------------------------------------------------------------
# Map one offense's season onto the GamePBP columns
# ---------------------------------------------------------------------------
# team        : club code, nflverse ("CLE") or PFF ("CLV")
# season_type : any of "REG", "POST"
# weeks       : optional subset of weeks, e.g. 10:18
nflverse_to_gamepbp <- function(team, season = 2025, season_type = c("REG", "POST"),
                                weeks = NULL, data = NULL) {
  if (is.null(data)) data <- nflverse_season_data(season)
  nv_team <- to_nflverse_code(team)
  pff_team <- to_pff_code(nv_team)

  pbp <- data$pbp
  if (nrow(pbp) == 0) stop("No nflverse play-by-play for ", season, ".", call. = FALSE)
  pbp <- dplyr::filter(pbp, .data$posteam == nv_team, .data$season_type %in% !!season_type,
                       .data$play_type %in% c("pass", "run", "no_play", "qb_kneel", "qb_spike"),
                       dplyr::coalesce(.data$two_point_attempt, 0) == 0,
                       !is.na(.data$down))
  if (!is.null(weeks)) pbp <- dplyr::filter(pbp, .data$week %in% weeks)
  if (nrow(pbp) == 0) stop("No ", nv_team, " offensive plays in nflverse ", season, ".", call. = FALSE)

  part <- data$participation
  if (nrow(part) > 0) {
    part <- dplyr::transmute(part, game_id = .data$nflverse_game_id, play_id = as.numeric(.data$play_id),
                             p_formation = nz(.data$offense_formation),
                             p_off_pos = nz(.data$offense_positions),
                             p_def_pos = nz(.data$defense_positions),
                             p_rushers = suppressWarnings(as.numeric(.data$number_of_pass_rushers)),
                             p_ttt = suppressWarnings(as.numeric(.data$time_to_throw)),
                             p_pressure = .data$was_pressure,
                             p_route = nz(.data$route),
                             p_cov = nz(.data$defense_coverage_type),
                             p_manzone = nz(.data$defense_man_zone_type))
    pbp <- dplyr::left_join(pbp, part, by = c("game_id", "play_id"))
  } else {
    for (v in c("p_formation", "p_off_pos", "p_def_pos", "p_route", "p_cov", "p_manzone")) pbp[[v]] <- NA_character_
    for (v in c("p_rushers", "p_ttt", "p_pressure")) pbp[[v]] <- NA_real_
  }

  ftn <- data$ftn
  if (nrow(ftn) > 0) {
    ftn <- dplyr::transmute(ftn, game_id = .data$nflverse_game_id, play_id = as.numeric(.data$nflverse_play_id),
                            f_qb = nz(.data$qb_location), f_pa = .data$is_play_action,
                            f_screen = .data$is_screen_pass, f_rpo = .data$is_rpo,
                            f_catchable = .data$is_catchable_ball, f_throwaway = .data$is_throw_away,
                            f_rushers = suppressWarnings(as.numeric(.data$n_pass_rushers))) |>
      dplyr::distinct(.data$game_id, .data$play_id, .keep_all = TRUE)
    pbp <- dplyr::left_join(pbp, ftn, by = c("game_id", "play_id"))
  } else {
    pbp$f_qb <- NA_character_
    for (v in c("f_pa", "f_screen", "f_rpo", "f_catchable", "f_throwaway", "f_rushers")) pbp[[v]] <- NA
  }

  # Roster position by player (most common across the season).
  ro <- data$rosters
  pos_lookup <- character()
  if (nrow(ro) > 0) {
    rp <- ro |>
      dplyr::filter(!is.na(.data$gsis_id)) |>
      dplyr::count(.data$gsis_id, .data$position) |>
      dplyr::group_by(.data$gsis_id) |>
      dplyr::slice_max(.data$n, n = 1, with_ties = FALSE)
    pos_lookup <- stats::setNames(dplyr::recode(rp$position, RB = "HB"), rp$gsis_id)
  }

  is_sack <- dplyr::coalesce(pbp$sack == 1, FALSE)
  is_scramble <- dplyr::coalesce(pbp$qb_scramble == 1, FALSE)
  is_dropback <- dplyr::coalesce(pbp$qb_dropback == 1, FALSE) | dplyr::coalesce(pbp$pass == 1, FALSE)
  no_play <- pbp$play_type == "no_play"

  runpass <- dplyr::case_when(
    no_play ~ "X",
    pbp$play_type == "qb_kneel" ~ "R",
    pbp$play_type == "qb_spike" ~ "P",
    is_dropback | is_scramble ~ "P",
    pbp$play_type == "run" ~ "R",
    TRUE ~ "X"
  )

  pass_result <- dplyr::case_when(
    runpass != "P" ~ NA_character_,
    pbp$play_type == "qb_spike" ~ "SPIKE",
    is_sack ~ "SACK",
    is_scramble ~ "RUN",
    dplyr::coalesce(pbp$interception == 1, FALSE) ~ "INTERCEPTION",
    dplyr::coalesce(pbp$complete_pass == 1, FALSE) ~ "COMPLETE",
    dplyr::coalesce(as.logical(pbp$f_throwaway), FALSE) ~ "THROWN AWAY",
    dplyr::coalesce(pbp$incomplete_pass == 1, FALSE) ~ "INCOMPLETE",
    TRUE ~ NA_character_
  )

  # QB alignment: FTN charting first (U / S / P), pbp shotgun flag as fallback.
  qb <- ifelse(pbp$f_qb %in% c("U", "S", "P"), pbp$f_qb,
               ifelse(dplyr::coalesce(pbp$shotgun, 0) == 1, "S", "U"))

  run_gap <- dplyr::case_when(
    runpass != "R" ~ NA_character_,
    pbp$play_type == "qb_kneel" ~ NA_character_,
    pbp$run_location %in% "middle" ~ "Middle",
    !is.na(pbp$run_gap) & !is.na(pbp$run_location) ~
      paste(tools::toTitleCase(pbp$run_location), tools::toTitleCase(pbp$run_gap)),
    !is.na(pbp$run_location) ~ tools::toTitleCase(pbp$run_location),
    TRUE ~ NA_character_
  )

  half_secs <- pbp$half_seconds_remaining
  wp <- dplyr::coalesce(pbp$vegas_wp, pbp$wp)
  # Calibrated on a licensed PFF sample: PFF's flag fires only when the game is
  # effectively decided (WP outside 0.01%-99.99% in the 2nd half).
  garbage <- as.numeric(pbp$qtr >= 3 & !is.na(wp) & (wp < 0.0001 | wp > 0.9999))
  two_min <- ifelse(!is.na(half_secs) & half_secs <= 120 & pbp$qtr %in% c(2, 4), 2, 0)

  rushers <- dplyr::coalesce(pbp$p_rushers, pbp$f_rushers)
  target <- ifelse(runpass == "P" & !is_sack & !is_scramble, nz(pbp$receiver_player_id), NA_character_)
  carrier <- dplyr::case_when(
    runpass == "R" ~ nz(pbp$rusher_player_id),
    is_scramble ~ nz(pbp$rusher_player_id),
    pass_result %in% "COMPLETE" ~ nz(pbp$receiver_player_id),
    TRUE ~ NA_character_
  )
  width <- dplyr::case_when(pbp$pass_location == "left" ~ 10, pbp$pass_location == "middle" ~ 26.65,
                            pbp$pass_location == "right" ~ 43.3, TRUE ~ NA_real_)
  coverage <- dplyr::case_when(
    is.na(pbp$p_cov) ~ NA_character_,
    pbp$p_cov == "2_MAN" ~ "COVER 2 MAN",
    TRUE ~ gsub("_", " ", pbp$p_cov)
  )

  out <- data.frame(
    pff_GAMEDATE = as.character(pbp$game_date),
    pff_GSISGAMEKEY = pbp$old_game_id,
    pff_GSISPLAYID = pbp$play_id,
    pff_QUARTER = pbp$qtr,
    pff_CLOCK = pbp$time,
    pff_OFFTEAM = pff_team,
    pff_DEFTEAM = to_pff_code(pbp$defteam),
    pff_DOWN = pbp$down,
    pff_DISTANCE = pbp$ydstogo,
    ABSOLUTEYARDLINE = pbp$yardline_100,
    pff_RUNPASS = runpass,
    pff_GAINLOSS = ifelse(no_play, NA, pbp$yards_gained),
    pff_PASSRESULT = pass_result,
    pff_FIRSTDOWNGAINED = as.numeric(dplyr::coalesce(pbp$first_down_rush == 1 | pbp$first_down_pass == 1, FALSE)),
    pff_EXPECTEDPOINTS = pbp$ep,
    pff_EXPECTEDPOINTSADDED = pbp$epa,
    pff_YARDSAFTERCATCH = pbp$yards_after_catch,
    pff_YARDSAFTERCONTACT = NA_real_,
    pff_PENALTY = ifelse(dplyr::coalesce(pbp$penalty == 1, FALSE), "Y", NA),
    pff_GARBAGETIME = garbage,
    pff_2MINUTE = two_min,
    pff_SHOTGUN = ifelse(qb %in% "S", "S", NA),
    pff_PISTOL = ifelse(qb %in% "P", "P", NA),
    pff_BACKSET = ifelse(pbp$play_type == "qb_kneel", "VICTORY", NA),
    pff_OFFFORMATIONGROUP = pbp$p_formation,
    pff_OFFPERSONNELBASIC = personnel_from_positions(pbp$p_off_pos),
    pff_DEFPACKAGE = def_package_from_positions(pbp$p_def_pos),
    pff_PLAYACTION = flag(pbp$f_pa),
    pff_RUNPASSOPTION = flag(pbp$f_rpo),
    pff_SCREEN = flag(pbp$f_screen),
    pff_NUMRUSHERS = rushers,
    pff_TIMETOTHROW = pbp$p_ttt,
    pff_TIMETOPRESSURE = ifelse(runpass == "P", flag(pbp$p_pressure), NA),
    pff_PASSDEPTH = ifelse(is.na(target), NA, pbp$air_yards),
    pff_PASSWIDTH = width,
    pff_CATCHABLE = flag(pbp$f_catchable),
    pff_PASSCOVERAGEBASIC = coverage,
    pff_PASSROUTETARGETGROUP = ifelse(is.na(target), NA, pbp$p_route),
    pff_PASSRECEIVERTARGET = target,
    pff_PASSRECEIVERPOSITIONTARGET = NA_character_,
    pff_PASSRECEIVERROSTERPOSITIONTARGET = ifelse(is.na(target), NA, unname(pos_lookup[target])),
    pff_BALLCARRIER = carrier,
    pff_RUNCONCEPTPRIMARY = run_gap,
    pff_RUNCONCEPTSECONDARY = NA_character_,
    pff_PASSCONCEPTTARGETED = NA_character_,
    pff_OFFSUCCESS = NA_character_,
    nfl_SEASON = pbp$season,
    nfl_WEEK = pbp$week,
    nfl_SEASONTYPE = pbp$season_type,
    nfl_PLAYDESC = pbp$desc,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$pff_GSISGAMEKEY, out$pff_GSISPLAYID), ]

  attr(out, "coverage") <- list(
    plays = nrow(out),
    personnel = mean(!is.na(out$pff_OFFPERSONNELBASIC[out$pff_RUNPASS != "X"])),
    ftn = mean(!is.na(pbp$f_qb[runpass != "X"])),
    games = length(unique(out$pff_GSISGAMEKEY))
  )
  out
}

# Roster CSV in the pipeline's format, keyed by gsis id, real names + jersey.
nflverse_roster <- function(pbp_out, data) {
  ro <- data$rosters
  ids <- unique(stats::na.omit(c(pbp_out$pff_PASSRECEIVERTARGET, pbp_out$pff_BALLCARRIER)))
  if (nrow(ro) == 0 || length(ids) == 0) return(NULL)
  ro |>
    dplyr::filter(.data$gsis_id %in% ids) |>
    dplyr::arrange(dplyr::desc(.data$week)) |>
    dplyr::distinct(.data$gsis_id, .keep_all = TRUE) |>
    dplyr::transmute(player_id = .data$gsis_id,
                     player_name = dplyr::coalesce(.data$full_name,
                                                   paste(.data$first_name, .data$last_name)),
                     jersey = .data$jersey_number, position = .data$position,
                     source = paste0("nflverse weekly rosters ", data$season))
}

# ---------------------------------------------------------------------------
# One call: download, map, write CSV + roster, build the report
# ---------------------------------------------------------------------------
build_nflverse_report <- function(team, season = 2025, season_type = c("REG", "POST"), weeks = NULL,
                                  out_dir = "output_nflverse", formats = c("html", "pdf"),
                                  config = list(), data = NULL) {
  if (is.null(data)) data <- nflverse_season_data(season)
  gp <- nflverse_to_gamepbp(team, season, season_type, weeks, data)
  pff_team <- gp$pff_OFFTEAM[1]
  cov <- attr(gp, "coverage")

  dir_data <- file.path(pipeline_root(), "data", "nflverse")
  dir.create(file.path(dir_data, "rosters"), recursive = TRUE, showWarnings = FALSE)
  tag <- paste0(pff_team, "_", season, if (!is.null(weeks)) paste0("_wk", min(weeks), "-", max(weeks)))
  csv <- file.path(dir_data, paste0(tag, "_nflverse_GamePBP.csv"))
  utils::write.csv(gp, csv, row.names = FALSE, na = "")

  ro <- nflverse_roster(gp, data)
  roster_file <- NULL
  if (!is.null(ro)) {
    roster_file <- file.path(dir_data, "rosters", paste0(tag, "_roster.csv"))
    utils::write.csv(ro, roster_file, row.names = FALSE)
  }

  gaps <- NFLVERSE_GAPS
  if (cov$personnel < 0.5) {
    gaps <- c(sprintf("Personnel / formation / coverage / routes: nflverse participation data covers only %.0f%% of these plays (it is typically posted after the season), so those sections are mostly empty.",
                      100 * cov$personnel), gaps)
  }
  if (cov$ftn < 0.9) {
    gaps <- c(sprintf("FTN charting (QB alignment, play action, screens, RPO) covers %.0f%% of plays; uncharted plays fall back to the pbp shotgun flag and count as no play action / no screen.",
                      100 * cov$ftn), gaps)
  }

  cfg <- utils::modifyList(list(
    data_source = "nflverse",
    data_source_notes = gaps,
    run_concept_label = "Run gap and direction",
    roster_file = roster_file,
    pff_success_codes = character(), pff_failure_codes = character()
  ), config)

  build_scouting_report(csv, team = pff_team, out_dir = out_dir, formats = formats, config = cfg)
}

# ---------------------------------------------------------------------------
# Rebuild a report from a CSV already in data/nflverse/ (no download needed)
# ---------------------------------------------------------------------------
# team : PFF club code as in the file name, e.g. "CLV", "HST", "NYG", "SF"
rebuild_nflverse_report <- function(team, season = 2025, out_dir = "output_nflverse",
                                    formats = c("html", "pdf"), config = list()) {
  team <- to_pff_code(team)
  dir_data <- file.path(pipeline_root(), "data", "nflverse")
  tag <- paste0(team, "_", season)
  csv <- file.path(dir_data, paste0(tag, "_nflverse_GamePBP.csv"))
  if (!file.exists(csv)) stop("No saved nflverse file: ", csv, ". Run build_nflverse_report() first.", call. = FALSE)
  roster_file <- file.path(dir_data, "rosters", paste0(tag, "_roster.csv"))
  cfg <- utils::modifyList(list(
    data_source = "nflverse",
    data_source_notes = NFLVERSE_GAPS,
    run_concept_label = "Run gap and direction",
    roster_file = if (file.exists(roster_file)) roster_file else NULL,
    pff_success_codes = character(), pff_failure_codes = character()
  ), config)
  build_scouting_report(csv, team = team, out_dir = out_dir, formats = formats, config = cfg)
}
