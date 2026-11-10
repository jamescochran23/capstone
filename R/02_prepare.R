# ==========================================================
# 02_prepare.R - build the offensive sample and derived fields
# ==========================================================

clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "UNDEFINED", "Unknown", "UNKNOWN", "NA")] <- NA
  x
}

# Pick the team to scout. If the file holds only one offense, use it.
resolve_team <- function(data, team = NULL) {
  teams <- sort(unique(stats::na.omit(data$pff_offteam)))
  if (is.null(team) || is.na(team) || team == "") {
    if (length(teams) == 1) return(teams)
    stop("This file has several offenses (", paste(teams, collapse = ", "),
         "). Pass team = \"XXX\" to choose one.", call. = FALSE)
  }
  if (!team %in% teams) {
    stop("Team '", team, "' not found on offense. Offenses in file: ",
         paste(teams, collapse = ", "), call. = FALSE)
  }
  team
}

prepare_pbp <- function(data, team, cfg = default_config()) {
  n_input <- sum(data$pff_offteam == team, na.rm = TRUE)

  off <- dplyr::filter(data, .data$pff_offteam == team)

  # ---- Sample filters, each step counted for the methodology table ------
  steps <- list(c("Offensive snaps in file", nrow(off)))

  off <- dplyr::filter(off, .data$pff_runpass %in% c("R", "P"))
  steps[[length(steps) + 1]] <- c("Run or pass plays (drops penalties / no-plays)", nrow(off))

  off <- dplyr::filter(off, is.na(.data$pff_down) | .data$pff_down %in% cfg$valid_downs)
  steps[[length(steps) + 1]] <- c("Downs 1-4 (drops PATs / 2-pt tries)", nrow(off))

  if (isTRUE(cfg$exclude_garbage)) {
    off <- dplyr::filter(off, is.na(.data$pff_garbagetime) | .data$pff_garbagetime == 0)
    steps[[length(steps) + 1]] <- c("Excluding garbage time", nrow(off))
  }
  if (isTRUE(cfg$exclude_kneels)) {
    kneel <- toupper(dplyr::coalesce(off$pff_backset, "")) == "VICTORY" |
      toupper(dplyr::coalesce(off$pff_passresult, "")) == "SPIKE"
    off <- off[!kneel, ]
    steps[[length(steps) + 1]] <- c("Excluding QB kneels and spikes", nrow(off))
  }
  if (isTRUE(cfg$exclude_two_minute)) {
    off <- dplyr::filter(off, is.na(.data$pff_2minute) | .data$pff_2minute == 0)
    steps[[length(steps) + 1]] <- c("Excluding 2- and 4-minute drills", nrow(off))
  }

  if (nrow(off) == 0) {
    stop("No plays left for ", team, " after filtering.", call. = FALSE)
  }

  pass_fail_results <- c("INCOMPLETE", "THROWN AWAY", "BATTED PASS", "HIT AS THREW",
                         "INTERCEPTION", "SPIKE")

  std_share <- cfg$std_success_share

  off <- off |>
    dplyr::mutate(
      play_type = dplyr::if_else(.data$pff_runpass == "R", "Run", "Pass"),
      pass_result = toupper(clean_text(.data$pff_passresult)),

      qb_alignment = dplyr::case_when(
        toupper(dplyr::coalesce(.data$pff_pistol, "")) %in% c("P", "1", "T", "TRUE", "Y") ~ "Pistol",
        toupper(dplyr::coalesce(.data$pff_shotgun, "")) %in% c("S", "1", "T", "TRUE", "Y") ~ "Shotgun",
        TRUE ~ "Under Center"
      ),
      qb_alignment = factor(.data$qb_alignment, levels = c("Under Center", "Shotgun", "Pistol")),

      personnel_raw = clean_text(.data$pff_offpersonnelbasic),
      personnel = if (isTRUE(cfg$merge_personnel_tags)) {
        trimws(sub("\\s*\\(\\*\\)\\s*$", "", .data$personnel_raw))
      } else {
        .data$personnel_raw
      },
      personnel = dplyr::coalesce(.data$personnel, "Unknown"),

      # Yards gained: incompletions etc. are recorded as NA; they are 0 yards.
      yards = dplyr::case_when(
        !is.na(.data$pff_gainloss) ~ .data$pff_gainloss,
        .data$play_type == "Pass" & .data$pass_result %in% pass_fail_results ~ 0,
        TRUE ~ NA_real_
      ),

      distance_bucket = cut(.data$pff_distance, breaks = cfg$distance_breaks,
                            labels = cfg$distance_labels, right = TRUE),
      down_label = factor(paste0(c("1st", "2nd", "3rd", "4th")[.data$pff_down], " Down"),
                          levels = paste0(c("1st", "2nd", "3rd", "4th"), " Down")),
      field_zone = cut(.data$absoluteyardline, breaks = cfg$field_breaks,
                       labels = cfg$field_labels, right = TRUE),

      first_down = as.numeric(
        dplyr::coalesce(.data$pff_firstdowngained == 1, FALSE) |
          dplyr::coalesce(.data$yards >= .data$pff_distance, FALSE)
      ),

      success_pff = dplyr::case_when(
        toupper(.data$pff_offsuccess) %in% toupper(cfg$pff_success_codes) ~ 1,
        toupper(.data$pff_offsuccess) %in% toupper(cfg$pff_failure_codes) ~ 0,
        TRUE ~ NA_real_
      ),
      success_std = dplyr::if_else(
        is.na(.data$yards) | is.na(.data$pff_distance) | is.na(.data$pff_down),
        NA_real_,
        as.numeric(.data$yards >= unname(std_share[as.character(.data$pff_down)]) * .data$pff_distance |
                     .data$first_down == 1)
      ),
      success = if (cfg$primary_success == "pff") .data$success_pff else .data$success_std,

      epa = .data$pff_expectedpointsadded,

      explosive = dplyr::case_when(
        is.na(.data$yards) ~ NA_real_,
        .data$play_type == "Run" ~ as.numeric(.data$yards >= cfg$explosive_run),
        TRUE ~ as.numeric(.data$yards >= cfg$explosive_pass)
      ),

      is_sack = as.numeric(dplyr::coalesce(.data$pass_result == "SACK", FALSE)),
      is_scramble = as.numeric(dplyr::coalesce(.data$pass_result == "RUN", FALSE)),
      is_int = as.numeric(dplyr::coalesce(.data$pass_result == "INTERCEPTION", FALSE)),
      is_complete = as.numeric(dplyr::coalesce(.data$pass_result == "COMPLETE", FALSE)),
      # No pressure charting at all (e.g. nflverse before participation is posted) -> NA, not "never pressured".
      pressured = if (all(is.na(.data$pff_timetopressure))) NA_real_ else
        as.numeric(dplyr::coalesce(.data$pff_timetopressure > 0, FALSE)),
      play_action = dplyr::coalesce(.data$pff_playaction, 0),
      rpo = dplyr::coalesce(.data$pff_runpassoption, 0),
      screen = dplyr::coalesce(.data$pff_screen, 0),

      # A target is a pass aimed at a receiver (sacks and scrambles excluded).
      target_player = clean_text(.data$pff_passreceivertarget),
      is_target = as.numeric(.data$play_type == "Pass" & !is.na(.data$target_player) &
                               .data$is_sack == 0 & .data$is_scramble == 0),
      target_position = dplyr::coalesce(clean_text(.data$pff_passreceiverrosterpositiontarget), "UNK"),
      target_alignment = clean_text(.data$pff_passreceiverpositiontarget),
      air_yards = .data$pff_passdepth,
      yac = .data$pff_yardsaftercatch,

      ball_carrier = clean_text(.data$pff_ballcarrier),
      run_concept = dplyr::if_else(
        .data$play_type == "Run",
        dplyr::coalesce(clean_text(.data$pff_runconceptprimary), "Unclassified"),
        NA_character_
      ),

      # Neutral situation: early downs, not in a 2/4-minute drill.
      early_down = .data$pff_down %in% c(1, 2),
      neutral = .data$early_down & dplyr::coalesce(.data$pff_2minute, 0) == 0
    )

  attr(off, "sample_steps") <- do.call(rbind, steps) |>
    as.data.frame(stringsAsFactors = FALSE) |>
    stats::setNames(c("step", "plays")) |>
    dplyr::mutate(plays = as.integer(.data$plays))
  attr(off, "team") <- team
  off
}

# ---------------------------------------------------------------------------
# Optional roster: swap "SF 23" for "C. McCaffrey (#23)" in every table and chart.
# Returns list(pbp, issues).
apply_roster <- function(pbp, team, cfg) {
  path <- cfg$roster_file
  if (is.null(path)) {
    auto <- file.path(pipeline_root(), "data", "rosters", paste0(team, "_roster.csv"))
    if (file.exists(auto)) path <- auto
  }
  if (is.null(path) || isTRUE(cfg$mock_data)) return(list(pbp = pbp, issues = character(), roster = NULL))
  ro <- readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  if (!all(c("player_id", "player_name") %in% names(ro))) {
    stop("Roster file needs columns player_id and player_name: ", path, call. = FALSE)
  }
  # Jersey comes from a "jersey" column when present (nflverse rosters, keyed
  # by gsis id), otherwise from the id itself ("SF 23").
  short <- function(name, id, jersey) {
    parts <- strsplit(name, " ")[[1]]
    first <- parts[1]; rest <- paste(parts[-1], collapse = " ")
    num <- suppressWarnings(as.integer(if (!is.na(jersey)) jersey else sub("^.*\\s", "", id)))
    if (is.na(num)) sprintf("%s. %s", substr(first, 1, 1), rest)
    else sprintf("%s. %s (#%d)", substr(first, 1, 1), rest, num)
  }
  jersey <- if ("jersey" %in% names(ro)) ro$jersey else rep(NA_character_, nrow(ro))
  ro$label <- mapply(short, ro$player_name, ro$player_id, jersey)
  lookup <- stats::setNames(ro$label, ro$player_id)
  relabel <- function(x) ifelse(!is.na(x) & x %in% names(lookup), lookup[x], x)
  ids <- unique(stats::na.omit(c(pbp$target_player, pbp$ball_carrier)))
  issues <- character()
  unmatched <- setdiff(ids, ro$player_id)
  if (length(unmatched)) issues <- c(issues, paste("Roster: no name for", paste(sort(unmatched), collapse = ", ")))
  pbp$target_player <- relabel(pbp$target_player)
  pbp$ball_carrier <- relabel(pbp$ball_carrier)
  issues <- c(issues, paste0("Player names from ", basename(path),
                             if ("source" %in% names(ro)) paste0(" (", ro$source[1], ")") else ""))
  list(pbp = pbp, issues = issues, roster = ro)
}
