# ==========================================================
# 05_insights.R - turn the tables into plain-English findings
# ==========================================================
# Rule-based, so the same logic runs for every team. Every finding states
# its sample size. Findings below the minimum sample are never generated.
# These are a first draft for the analyst: read the tables before
# presenting any of them.

f_pct <- function(x) ifelse(is.na(x), "n/a", scales::percent(x, accuracy = 1))
f_epa <- function(x) ifelse(is.na(x), "n/a", sprintf("%+.2f", x))
f_num <- function(x, d = 1) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = d))

finding <- function(category, text, n = NA_integer_, metric = NA_real_) {
  dplyr::tibble(category = category, finding = text, n = as.integer(n), metric = metric)
}

# filter() that tolerates the empty, column-less tables returned when a
# section has no data (e.g. a pass-only file has no run concepts).
nz_filter <- function(df, ...) if (is.null(df) || nrow(df) == 0) dplyr::tibble() else dplyr::filter(df, ...)

build_insights <- function(res, cfg) {
  out <- list()
  ov <- res$overview
  add <- function(x) out[[length(out) + 1]] <<- x

  # ---------------- Tendencies ----------------
  add(finding("Tendency", sprintf(
    "Passes on %s of all plays and %s of neutral early-down plays (1st/2nd down, outside 2-minute drills).",
    f_pct(ov$pass_rate), f_pct(ov$neutral_pass_rate)), ov$plays, ov$pass_rate))

  al <- res$alignment
  uc <- al[al$qb_alignment == "Under Center", ]
  sg <- al[al$qb_alignment == "Shotgun", ]
  if (nrow(uc) == 1 && nrow(sg) == 1 && uc$total_plays >= cfg$min_n_group && sg$total_plays >= cfg$min_n_group) {
    add(finding("Tendency", sprintf(
      "QB alignment is a strong run/pass key: under center they run %s of the time (n=%d); in shotgun they pass %s (n=%d).",
      f_pct(1 - uc$pass_rate), uc$total_plays, f_pct(sg$pass_rate), sg$total_plays),
      uc$total_plays + sg$total_plays, abs(sg$pass_rate - uc$pass_rate)))
  }
  pi <- al[al$qb_alignment == "Pistol", ]
  if (nrow(pi) == 1 && pi$total_plays >= cfg$min_n_group) {
    add(finding("Tendency", sprintf("Pistol is a real part of the menu: %d plays, %s pass.",
                                    pi$total_plays, f_pct(pi$pass_rate)), pi$total_plays, pi$pass_rate))
  }

  pers <- nz_filter(res$personnel, .data$total_plays >= cfg$min_n_group)
  if (nrow(pers) > 0) {
    top <- pers[1, ]
    add(finding("Tendency", sprintf(
      "Base personnel is %s (%s of snaps, %s pass). %d groupings reach %d+ plays.",
      top$personnel, f_pct(top$share_of_snaps), f_pct(top$pass_rate), nrow(pers), cfg$min_n_group),
      top$total_plays, top$share_of_snaps))
  }

  tells <- nz_filter(res$tells, .data$is_tell)
  if (nrow(tells) > 0) {
    # Keep the strongest, and avoid near-duplicate cues.
    t <- tells |> dplyr::distinct(.data$cue, .keep_all = TRUE) |> dplyr::slice_head(n = 4)
    for (i in seq_len(nrow(t))) {
      add(finding("Tendency", sprintf("Tell: %s -> %s %s of the time (n=%d, 95%% lower bound %s).",
                                      t$cue[i], tolower(t$lean[i]), f_pct(t$lean_rate[i]), t$plays[i],
                                      f_pct(t$lean_lo[i])), t$plays[i], t$lean_rate[i]))
    }
  }

  cw <- if (identical(cfg$data_source, "nflverse")) "run gap" else "run concept"
  rc <- nz_filter(res$run_concepts, .data$run_concept != "Unclassified")
  if (nrow(rc) > 0) {
    top2 <- utils::head(rc, 2)
    add(finding("Tendency", sprintf(
      "Run game is built on %s: %s of carries.",
      paste(top2$run_concept, collapse = " and "), f_pct(sum(top2$carries) / sum(res$run_concepts$carries))),
      sum(top2$carries), sum(top2$carry_share)))
  }

  # ---------------- Strengths ----------------
  rc_ok <- nz_filter(rc, .data$carries >= cfg$min_n_concept)
  if (nrow(rc_ok) > 0) {
    best <- rc_ok[which.max(rc_ok$epa_per_carry), ]
    if (isTRUE(best$epa_per_carry > 0)) {
      add(finding("Strength", sprintf(
        "Most efficient %s: %s, %s EPA/carry, %s YPC, %s success (n=%d).",
        cw, best$run_concept, f_epa(best$epa_per_carry), f_num(best$yards_per_carry),
        f_pct(best$success_rate), best$carries), best$carries, best$epa_per_carry))
    }
  }

  rec <- nz_filter(res$receivers, .data$targets >= cfg$min_targets)
  if (nrow(rec) > 0) {
    top_vol <- rec[1, ]
    add(finding("Strength", sprintf(
      "Primary target: %s (%s), %s target share, %d catches for %d yards, %s EPA/target, %d first downs.",
      top_vol$target_player, top_vol$target_position, f_pct(top_vol$target_share), top_vol$receptions,
      round(top_vol$receiving_yards), f_epa(top_vol$epa_per_target), top_vol$first_downs),
      top_vol$targets, top_vol$epa_per_target))
    best <- rec[which.max(rec$epa_per_target), ]
    if (isTRUE(best$target_player != top_vol$target_player && best$epa_per_target > 0)) {
      add(finding("Strength", sprintf(
        "Most efficient receiver: %s (%s), %s EPA/target on %d targets, aDOT %s, %s YAC/rec.",
        best$target_player, best$target_position, f_epa(best$epa_per_target), best$targets,
        f_num(best$adot), f_num(best$yac_per_rec)), best$targets, best$epa_per_target))
    }
  }

  ru <- nz_filter(res$rushers, .data$carries >= cfg$min_carries)
  if (nrow(ru) > 0) {
    lead <- ru[1, ]
    style <- if (!is.na(lead$yaco_per_carry) && !is.na(lead$yards_before_contact) &&
                 lead$yaco_per_carry > lead$yards_before_contact) "after contact" else "before contact"
    top_word <- if (identical(cfg$data_source, "nflverse")) "Favorite gap" else "Top concept"
    txt <- if (is.na(lead$yaco_per_carry)) {
      sprintf("Lead back %s: %s of carries, %s YPC, %s success. %s: %s.",
              lead$ball_carrier, f_pct(lead$carry_share), f_num(lead$yards_per_carry),
              f_pct(lead$success_rate), top_word, lead$top_concept)
    } else {
      sprintf("Lead back %s: %s of carries, %s YPC, %s success, most yards come %s (%s YACo/carry). %s: %s.",
              lead$ball_carrier, f_pct(lead$carry_share), f_num(lead$yards_per_carry), f_pct(lead$success_rate),
              style, f_num(lead$yaco_per_carry), top_word, lead$top_concept)
    }
    add(finding("Strength", txt, lead$carries, lead$epa_per_carry))
  }

  imp <- nz_filter(res$player_impact, .data$touches_or_targets >= cfg$min_targets)
  if (nrow(imp) > 0 && !is.na(imp$total_epa[1])) {
    p <- imp[1, ]
    add(finding("Strength", sprintf(
      "Most total value: %s, %s total EPA on %d touches + targets (%d scrimmage yards).",
      p$player, f_num(p$total_epa), as.integer(p$touches_or_targets), round(p$scrimmage_yards)),
      p$touches_or_targets, p$total_epa))
  }

  pc <- res$pass_context
  pa <- nz_filter(pc, .data$split == "Play action")
  if (nrow(pa) == 2 && all(pa$plays >= cfg$min_n_group) && !anyNA(pa$epa_per_play)) {
    yes <- pa[pa$value == "Yes", ]; no <- pa[pa$value == "No", ]
    diff <- yes$epa_per_play - no$epa_per_play
    add(finding(ifelse(diff > 0, "Strength", "Vulnerability"), sprintf(
      "Play action: %s of dropbacks, %s EPA/play vs %s without (n=%d vs %d).",
      f_pct(yes$share), f_epa(yes$epa_per_play), f_epa(no$epa_per_play), yes$plays, no$plays),
      yes$plays, diff))
  }

  dep <- res$depth
  deep <- nz_filter(dep, .data$depth == "Deep (20+)")
  if (nrow(deep) == 1 && deep$targets >= cfg$min_n_group / 2 && !is.na(deep$epa_per_target) &&
      (deep$epa_per_target > 0.3 || deep$epa_per_target < 0)) {
    add(finding(ifelse(deep$epa_per_target > 0.3, "Strength", "Vulnerability"), sprintf(
      "Deep shots (20+ air yards): %s of targets, %s complete, %s EPA/target (n=%d).",
      f_pct(deep$share), f_pct(deep$completion_rate), f_epa(deep$epa_per_target), deep$targets),
      deep$targets, deep$epa_per_target))
  }

  # ---------------- Vulnerabilities ----------------
  pr <- nz_filter(pc, .data$split == "Pressure")
  if (nrow(pr) == 2 && all(pr$plays >= cfg$min_n_group) && !anyNA(pr$epa_per_play)) {
    yes <- pr[pr$value == "Pressured", ]; no <- pr[pr$value == "Clean", ]
    add(finding("Vulnerability", sprintf(
      "Pressure breaks the passing game: %s EPA/dropback when pressured vs %s clean. Pressured on %s of dropbacks; sack rate %s.",
      f_epa(yes$epa_per_play), f_epa(no$epa_per_play), f_pct(yes$share), f_pct(ov$sack_rate)),
      yes$plays, yes$epa_per_play - no$epa_per_play))
  }

  if (nrow(rc_ok) > 1) {
    worst <- rc_ok[which.min(rc_ok$epa_per_carry), ]
    if (isTRUE(worst$epa_per_carry < 0)) {
      add(finding("Vulnerability", sprintf(
        "Least efficient %s: %s, %s EPA/carry, stuffed (0 or fewer yards) on %s (n=%d).",
        cw, worst$run_concept, f_epa(worst$epa_per_carry), f_pct(worst$stuff_rate), worst$carries),
        worst$carries, worst$epa_per_carry))
    }
  }

  td <- nz_filter(res$third_down, .data$plays >= 5)
  if (nrow(td) > 0) {
    w <- td[which.min(td$conversion_rate), ]
    add(finding("Vulnerability", sprintf(
      "On 3rd and %s they convert only %s (n=%d); they pass %s of the time there. Overall 3rd-down conversion: %s.",
      w$distance_bucket, f_pct(w$conversion_rate), w$plays, f_pct(w$pass_rate), f_pct(ov$third_down_conv)),
      w$plays, w$conversion_rate))
  }

  if (nrow(pers) > 1) {
    w <- pers[which.min(pers$epa_all), ]
    if (isTRUE(w$epa_all < 0)) {
      add(finding("Vulnerability", sprintf(
        "Least productive personnel: %s, %s EPA/play (n=%d, %s pass).",
        w$personnel, f_epa(w$epa_all), w$total_plays, f_pct(w$pass_rate)), w$total_plays, w$epa_all))
    }
  }

  if (nrow(rec) > 1) {
    vol <- nz_filter(rec, .data$target_share >= 0.08)
    if (nrow(vol) > 0) {
      w <- vol[which.min(vol$epa_per_target), ]
      if (isTRUE(w$epa_per_target < 0.1)) {
        add(finding("Vulnerability", sprintf(
          "High-volume, low-return target: %s (%s), %s of targets but %s EPA/target and %s catch rate.",
          w$target_player, w$target_position, f_pct(w$target_share), f_epa(w$epa_per_target),
          f_pct(w$catch_rate)), w$targets, w$epa_per_target))
      }
    }
  }

  pred <- res$predictability
  add(finding("Tendency", sprintf(
    "Predictability: knowing personnel, alignment, down and distance lifts a defense's run/pass guess from %s to %s (leave-one-out).",
    f_pct(pred$baseline_accuracy), f_pct(pred$presnap_accuracy)), ov$plays, pred$lift))

  dplyr::bind_rows(out) |>
    dplyr::mutate(category = factor(.data$category, levels = c("Tendency", "Strength", "Vulnerability"))) |>
    dplyr::arrange(.data$category)
}
