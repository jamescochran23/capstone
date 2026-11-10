# ==========================================================
# 03_analysis.R - one function per scouting question
# ==========================================================
# Every function takes the prepared play table (from prepare_pbp) and
# returns a tidy tibble. Rates carry sample sizes, and the main rates carry
# 95% Wilson intervals so small samples are visible, not hidden.

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sum  <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

wilson <- function(x, n, z = 1.96) {
  p <- ifelse(n > 0, x / n, NA_real_)
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  list(lo = pmax(0, centre - half), hi = pmin(1, centre + half))
}

# Standard efficiency block reused by most tables.
eff_summary <- function(df) {
  dplyr::summarise(
    df,
    plays = dplyr::n(),
    success_rate = safe_mean(.data$success),
    epa_per_play = safe_mean(.data$epa),
    yards_per_play = safe_mean(.data$yards),
    first_down_rate = safe_mean(.data$first_down),
    explosive_rate = safe_mean(.data$explosive),
    .groups = "drop"
  )
}

# Run/pass split plus efficiency of each, for any grouping.
# `vars` is a character vector of grouping column names.
tendency_by <- function(pbp, vars) {
  by_type <- pbp |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(vars, "play_type")))) |>
    eff_summary()

  totals <- pbp |>
    dplyr::group_by(dplyr::across(dplyr::all_of(vars))) |>
    dplyr::summarise(
      total_plays = dplyr::n(),
      passes = sum(.data$play_type == "Pass"),
      success_all = safe_mean(.data$success),
      epa_all = safe_mean(.data$epa),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pass_rate = .data$passes / .data$total_plays,
      pass_rate_lo = wilson(.data$passes, .data$total_plays)$lo,
      pass_rate_hi = wilson(.data$passes, .data$total_plays)$hi,
      share_of_snaps = .data$total_plays / sum(.data$total_plays)
    )

  wide <- by_type |>
    dplyr::select(dplyr::all_of(c(vars, "play_type", "plays", "success_rate", "epa_per_play", "yards_per_play"))) |>
    tidyr::pivot_wider(
      names_from = "play_type",
      values_from = c("plays", "success_rate", "epa_per_play", "yards_per_play"),
      names_glue = "{tolower(play_type)}_{.value}"
    )

  for (col in c("run_plays", "pass_plays")) if (!col %in% names(wide)) wide[[col]] <- 0L
  for (col in c("run_success_rate", "pass_success_rate", "run_epa_per_play", "pass_epa_per_play",
                "run_yards_per_play", "pass_yards_per_play")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }

  dplyr::left_join(totals, wide, by = vars) |>
    dplyr::mutate(dplyr::across(c("run_plays", "pass_plays"), ~ dplyr::coalesce(as.integer(.x), 0L))) |>
    dplyr::arrange(dplyr::desc(.data$total_plays))
}

# ---- 0. Sample overview ---------------------------------------------------
analyze_overview <- function(pbp) {
  pbp |>
    dplyr::summarise(
      team = attr(pbp, "team"),
      games = dplyr::n_distinct(.data$pff_gsisgamekey),
      first_game = suppressWarnings(min(.data$pff_gamedate, na.rm = TRUE)),
      last_game = suppressWarnings(max(.data$pff_gamedate, na.rm = TRUE)),
      opponents = paste(sort(unique(stats::na.omit(.data$pff_defteam))), collapse = ", "),
      plays = dplyr::n(),
      plays_per_game = dplyr::n() / dplyr::n_distinct(.data$pff_gsisgamekey),
      runs = sum(.data$play_type == "Run"),
      dropbacks = sum(.data$play_type == "Pass"),
      pass_rate = mean(.data$play_type == "Pass"),
      neutral_pass_rate = safe_mean(ifelse(.data$neutral, .data$play_type == "Pass", NA)),
      success_rate = safe_mean(.data$success),
      success_rate_pff = safe_mean(.data$success_pff),
      epa_per_play = safe_mean(.data$epa),
      epa_per_dropback = safe_mean(ifelse(.data$play_type == "Pass", .data$epa, NA)),
      epa_per_run = safe_mean(ifelse(.data$play_type == "Run", .data$epa, NA)),
      sack_rate = safe_mean(ifelse(.data$play_type == "Pass", .data$is_sack, NA)),
      pressure_rate = safe_mean(ifelse(.data$play_type == "Pass", .data$pressured, NA)),
      play_action_rate = safe_mean(ifelse(.data$play_type == "Pass", .data$play_action, NA)),
      explosive_rate = safe_mean(.data$explosive),
      third_down_conv = safe_mean(ifelse(.data$pff_down == 3, .data$first_down, NA))
    )
}

# ---- 1. Personnel ---------------------------------------------------------
# Plays with no recorded personnel ("Unknown") are left out of personnel
# tables; the report notes how many were dropped.
analyze_personnel <- function(pbp) tendency_by(dplyr::filter(pbp, .data$personnel != "Unknown"), "personnel")

# ---- 2. Down and distance -------------------------------------------------
analyze_down_distance <- function(pbp) {
  pbp |>
    dplyr::filter(!is.na(.data$down_label), !is.na(.data$distance_bucket)) |>
    tendency_by(c("down_label", "distance_bucket")) |>
    dplyr::arrange(.data$down_label, .data$distance_bucket)
}

analyze_field_zone <- function(pbp) {
  pbp |>
    dplyr::filter(!is.na(.data$field_zone)) |>
    tendency_by("field_zone") |>
    dplyr::arrange(.data$field_zone)
}

# ---- 3. QB alignment ------------------------------------------------------
analyze_alignment <- function(pbp) tendency_by(pbp, "qb_alignment") |> dplyr::arrange(.data$qb_alignment)

analyze_alignment_personnel <- function(pbp, cfg) {
  tendency_by(dplyr::filter(pbp, .data$personnel != "Unknown"), c("personnel", "qb_alignment")) |>
    dplyr::filter(.data$total_plays >= max(3, floor(cfg$min_n_group / 2)))
}

# ---- 4. Run concepts ------------------------------------------------------
analyze_run_concepts <- function(pbp) {
  runs <- dplyr::filter(pbp, .data$play_type == "Run")
  if (nrow(runs) == 0) return(dplyr::tibble())
  runs |>
    dplyr::group_by(.data$run_concept) |>
    dplyr::summarise(
      carries = dplyr::n(),
      yards_per_carry = safe_mean(.data$yards),
      total_yards = safe_sum(.data$yards),
      median_yards = stats::median(.data$yards, na.rm = TRUE),
      success_rate = safe_mean(.data$success),
      epa_per_carry = safe_mean(.data$epa),
      epa_se = stats::sd(.data$epa, na.rm = TRUE) / sqrt(sum(!is.na(.data$epa))),
      first_down_rate = safe_mean(.data$first_down),
      explosive_rate = safe_mean(.data$explosive),
      stuff_rate = safe_mean(as.numeric(.data$yards <= 0)),
      under_center_rate = mean(.data$qb_alignment == "Under Center"),
      top_personnel = names(sort(table(.data$personnel), decreasing = TRUE))[1],
      .groups = "drop"
    ) |>
    dplyr::mutate(carry_share = .data$carries / sum(.data$carries)) |>
    dplyr::arrange(dplyr::desc(.data$carries))
}

# ---- 5. Third down --------------------------------------------------------
analyze_third_down <- function(pbp) {
  third <- dplyr::filter(pbp, .data$pff_down == 3, !is.na(.data$distance_bucket))
  if (nrow(third) == 0) return(dplyr::tibble())
  third |>
    dplyr::group_by(.data$distance_bucket) |>
    dplyr::summarise(
      plays = dplyr::n(),
      pass_rate = mean(.data$play_type == "Pass"),
      conversions = sum(.data$first_down, na.rm = TRUE),
      conversion_rate = mean(.data$first_down, na.rm = TRUE),
      conv_lo = wilson(sum(.data$first_down, na.rm = TRUE), dplyr::n())$lo,
      conv_hi = wilson(sum(.data$first_down, na.rm = TRUE), dplyr::n())$hi,
      epa_per_play = safe_mean(.data$epa),
      shotgun_rate = mean(.data$qb_alignment != "Under Center"),
      avg_air_yards = safe_mean(ifelse(.data$is_target == 1, .data$air_yards, NA)),
      sack_rate = safe_mean(ifelse(.data$play_type == "Pass", .data$is_sack, NA)),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$distance_bucket)
}

receiver_block <- function(df) {
  dplyr::summarise(
    df,
    targets = dplyr::n(),
    receptions = sum(.data$is_complete),
    receiving_yards = sum(ifelse(.data$is_complete == 1, dplyr::coalesce(.data$yards, 0), 0)),
    first_downs = sum(.data$first_down, na.rm = TRUE),
    epa_per_target = safe_mean(.data$epa),
    success_rate = safe_mean(.data$success),
    adot = safe_mean(.data$air_yards),
    yac_per_rec = safe_mean(ifelse(.data$is_complete == 1, .data$yac, NA)),
    explosive_rec = sum(.data$is_complete == 1 & dplyr::coalesce(.data$yards, 0) >= 20),
    interceptions = sum(.data$is_int),
    .groups = "drop"
  ) |>
    dplyr::mutate(
      catch_rate = .data$receptions / .data$targets,
      yards_per_target = .data$receiving_yards / .data$targets,
      first_down_rate = .data$first_downs / .data$targets
    )
}

analyze_third_down_targets <- function(pbp) {
  t3 <- dplyr::filter(pbp, .data$pff_down == 3, .data$is_target == 1, !is.na(.data$distance_bucket))
  if (nrow(t3) == 0) return(dplyr::tibble())
  overall <- t3 |>
    dplyr::group_by(.data$target_player, .data$target_position) |>
    receiver_block() |>
    dplyr::mutate(distance_bucket = "All 3rd Downs", target_share = .data$targets / sum(.data$targets))
  by_bucket <- t3 |>
    dplyr::group_by(.data$distance_bucket, .data$target_player, .data$target_position) |>
    receiver_block() |>
    dplyr::group_by(.data$distance_bucket) |>
    dplyr::mutate(target_share = .data$targets / sum(.data$targets)) |>
    dplyr::ungroup() |>
    dplyr::mutate(distance_bucket = as.character(.data$distance_bucket))
  dplyr::bind_rows(overall, by_bucket) |>
    dplyr::arrange(.data$distance_bucket != "All 3rd Downs", .data$distance_bucket, dplyr::desc(.data$targets))
}

# ---- 6. Player production -------------------------------------------------
analyze_receivers <- function(pbp) {
  tg <- dplyr::filter(pbp, .data$is_target == 1)
  if (nrow(tg) == 0) return(dplyr::tibble())
  main_align <- tg |>
    dplyr::count(.data$target_player, .data$target_alignment) |>
    dplyr::group_by(.data$target_player) |>
    dplyr::slice_max(.data$n, n = 1, with_ties = FALSE) |>
    dplyr::select("target_player", main_alignment = "target_alignment")
  main_route <- tg |>
    dplyr::filter(!is.na(.data$pff_passroutetargetgroup)) |>
    dplyr::count(.data$target_player, .data$pff_passroutetargetgroup) |>
    dplyr::group_by(.data$target_player) |>
    dplyr::slice_max(.data$n, n = 1, with_ties = FALSE) |>
    dplyr::select("target_player", top_route_group = "pff_passroutetargetgroup")

  # Position: most common roster position for the player.
  tg |>
    dplyr::group_by(.data$target_player) |>
    dplyr::mutate(target_position = names(sort(table(.data$target_position), decreasing = TRUE))[1]) |>
    dplyr::group_by(.data$target_player, .data$target_position) |>
    receiver_block() |>
    dplyr::mutate(
      target_share = .data$targets / sum(.data$targets)
    ) |>
    dplyr::left_join(main_align, by = "target_player") |>
    dplyr::left_join(main_route, by = "target_player") |>
    dplyr::arrange(dplyr::desc(.data$targets))
}

analyze_rushers <- function(pbp) {
  ru <- dplyr::filter(pbp, .data$play_type == "Run", !is.na(.data$ball_carrier))
  if (nrow(ru) == 0) return(dplyr::tibble())
  top_concept <- ru |>
    dplyr::count(.data$ball_carrier, .data$run_concept) |>
    dplyr::group_by(.data$ball_carrier) |>
    dplyr::slice_max(.data$n, n = 1, with_ties = FALSE) |>
    dplyr::select("ball_carrier", top_concept = "run_concept")
  ru |>
    dplyr::group_by(.data$ball_carrier) |>
    dplyr::summarise(
      carries = dplyr::n(),
      rushing_yards = safe_sum(.data$yards),
      yards_per_carry = safe_mean(.data$yards),
      success_rate = safe_mean(.data$success),
      epa_per_carry = safe_mean(.data$epa),
      first_down_rate = safe_mean(.data$first_down),
      explosive_rate = safe_mean(.data$explosive),
      stuff_rate = safe_mean(as.numeric(.data$yards <= 0)),
      yaco_per_carry = safe_mean(.data$pff_yardsaftercontact),
      yards_before_contact = safe_mean(.data$yards - .data$pff_yardsaftercontact),
      .groups = "drop"
    ) |>
    dplyr::mutate(carry_share = .data$carries / sum(.data$carries)) |>
    dplyr::left_join(top_concept, by = "ball_carrier") |>
    dplyr::arrange(dplyr::desc(.data$carries))
}

# Total offensive involvement: rushing + receiving EPA per player.
analyze_player_impact <- function(pbp) {
  rush <- pbp |>
    dplyr::filter(.data$play_type == "Run", !is.na(.data$ball_carrier)) |>
    dplyr::group_by(player = .data$ball_carrier) |>
    dplyr::summarise(rush_plays = dplyr::n(), rush_epa = safe_sum(.data$epa),
                     rush_yards = safe_sum(.data$yards), .groups = "drop")
  rec <- pbp |>
    dplyr::filter(.data$is_target == 1) |>
    dplyr::group_by(player = .data$target_player) |>
    dplyr::summarise(targets = dplyr::n(), rec_epa = safe_sum(.data$epa),
                     rec_yards = sum(ifelse(.data$is_complete == 1, dplyr::coalesce(.data$yards, 0), 0)),
                     .groups = "drop")
  dplyr::full_join(rush, rec, by = "player") |>
    dplyr::mutate(
      dplyr::across(c("rush_plays", "targets", "rush_epa", "rec_epa", "rush_yards", "rec_yards"),
                    ~ dplyr::coalesce(.x, 0)),
      touches_or_targets = .data$rush_plays + .data$targets,
      total_epa = .data$rush_epa + .data$rec_epa,
      epa_per_opportunity = .data$total_epa / .data$touches_or_targets,
      scrimmage_yards = .data$rush_yards + .data$rec_yards
    ) |>
    dplyr::arrange(dplyr::desc(.data$total_epa))
}

# ---- 7. Passing-game context ---------------------------------------------
analyze_pass_context <- function(pbp) {
  dp <- dplyr::filter(pbp, .data$play_type == "Pass")
  if (nrow(dp) == 0) return(dplyr::tibble())
  dplyr::bind_rows(
    dp |> dplyr::group_by(split = "Play action", value = ifelse(.data$play_action == 1, "Yes", "No")) |> eff_summary(),
    dp |> dplyr::group_by(split = "Screen", value = ifelse(.data$screen == 1, "Yes", "No")) |> eff_summary(),
    if (any(!is.na(dp$pressured)))
      dp |> dplyr::group_by(split = "Pressure", value = ifelse(.data$pressured == 1, "Pressured", "Clean")) |> eff_summary(),
    dp |> dplyr::group_by(split = "Coverage", value = dplyr::coalesce(.data$pff_passcoveragebasic, "Unknown")) |> eff_summary(),
    dp |> dplyr::group_by(split = "Defensive package", value = dplyr::coalesce(.data$pff_defpackage, "Unknown")) |> eff_summary()
  ) |>
    dplyr::group_by(.data$split) |>
    dplyr::mutate(share = .data$plays / sum(.data$plays)) |>
    dplyr::ungroup()
}

analyze_depth <- function(pbp) {
  tg <- dplyr::filter(pbp, .data$is_target == 1, !is.na(.data$air_yards))
  if (nrow(tg) == 0) return(dplyr::tibble())
  tg |>
    dplyr::mutate(depth = cut(.data$air_yards, c(-Inf, 0, 9, 19, Inf),
                              labels = c("Behind LOS", "Short (1-9)", "Intermediate (10-19)", "Deep (20+)")),
                  side = dplyr::case_when(
                    is.na(.data$pff_passwidth) ~ NA_character_,
                    .data$pff_passwidth < 53.3 / 3 ~ "Left",
                    .data$pff_passwidth > 2 * 53.3 / 3 ~ "Right",
                    TRUE ~ "Middle")) |>
    dplyr::group_by(.data$depth) |>
    dplyr::summarise(
      targets = dplyr::n(),
      completion_rate = mean(.data$is_complete),
      epa_per_target = safe_mean(.data$epa),
      yards_per_target = mean(ifelse(.data$is_complete == 1, dplyr::coalesce(.data$yards, 0), 0)),
      int_rate = mean(.data$is_int),
      .groups = "drop"
    ) |>
    dplyr::mutate(share = .data$targets / sum(.data$targets))
}

# ---- 8. Predictability: the pre-snap "tells" -----------------------------
# Every combination of pre-snap cues with enough plays, flagged when the
# run or pass rate is at or above the tell threshold.
analyze_tells <- function(pbp, cfg) {
  combos <- list(
    c("personnel"),
    c("qb_alignment"),
    c("personnel", "qb_alignment"),
    c("down_label", "distance_bucket"),
    c("qb_alignment", "down_label"),
    c("personnel", "down_label")
  )
  out <- lapply(combos, function(vars) {
    pbp |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(vars), ~ !is.na(.x) & .x != "Unknown")) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(vars))) |>
      dplyr::summarise(plays = dplyr::n(), passes = sum(.data$play_type == "Pass"),
                       epa = safe_mean(.data$epa), .groups = "drop") |>
      dplyr::mutate(
        cue = apply(dplyr::across(dplyr::all_of(vars), as.character), 1, paste, collapse = " + "),
        cue_type = paste(vars, collapse = " x ")
      ) |>
      dplyr::select("cue_type", "cue", "plays", "passes", "epa")
  })
  dplyr::bind_rows(out) |>
    dplyr::filter(.data$plays >= cfg$min_n_group) |>
    dplyr::mutate(
      pass_rate = .data$passes / .data$plays,
      lean = ifelse(.data$pass_rate >= 0.5, "Pass", "Run"),
      lean_rate = pmax(.data$pass_rate, 1 - .data$pass_rate),
      lean_lo = wilson(round(.data$lean_rate * .data$plays), .data$plays)$lo,
      is_tell = .data$lean_rate >= cfg$tell_threshold
    ) |>
    dplyr::arrange(dplyr::desc(.data$is_tell), dplyr::desc(.data$lean_lo))
}

# Simple predictability score: how much better than "always guess the
# majority play type" can a defense do by knowing personnel + alignment +
# down + distance bucket? Reported in-sample (leave-one-out would be
# better with more data; flagged in the methodology).
analyze_predictability <- function(pbp) {
  base <- max(mean(pbp$play_type == "Pass"), mean(pbp$play_type == "Run"))
  cell <- pbp |>
    dplyr::group_by(.data$personnel, .data$qb_alignment, .data$down_label, .data$distance_bucket) |>
    dplyr::mutate(n_cell = dplyr::n(),
                  # leave-one-out pass rate inside the cell
                  loo_pass = (sum(.data$play_type == "Pass") - (.data$play_type == "Pass")) /
                    pmax(.data$n_cell - 1, 1)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      overall_pass = mean(.data$play_type == "Pass"),
      guess_pass = ifelse(.data$n_cell > 1, .data$loo_pass >= 0.5, .data$overall_pass >= 0.5),
      correct = (.data$guess_pass & .data$play_type == "Pass") | (!.data$guess_pass & .data$play_type == "Run")
    )
  dplyr::tibble(
    baseline_accuracy = base,
    presnap_accuracy = mean(cell$correct),
    lift = mean(cell$correct) - base
  )
}
