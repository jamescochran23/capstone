# ==========================================================
# generate_mock_data.R - simulate GamePBP files for testing
# ==========================================================
# !! MOCK DATA !!  Produces play-by-play in the exact 56-column GamePBP
# PFF-style GamePBP layout, simulated drive by drive
# from the knobs in team_profiles.R. Output files are named
# MOCK_<TEAM>_GamePBP.xlsx and include a README_MOCK sheet.
#
# Usage (working directory = pipeline folder):
#   source("mock/generate_mock_data.R")
#   generate_all_mocks()                 # NYG, CLV, HST, SF -> mock/
#   generate_mock_team(mock_profiles$SF, games = 8)
#
# Player IDs look like the source file ("SF 23") but the jersey numbers
# are drawn at random within each position's usual number range. They are
# not meant to identify any real player; the README_MOCK sheet maps each
# ID to its simulated role (WR1, RB1, ...).

local({
  ofiles <- as.character(unlist(lapply(sys.frames(), function(f) if (!is.null(f$ofile)) f$ofile)))
  ofiles <- ofiles[basename(ofiles) == "generate_mock_data.R"]
  here <- if (length(ofiles)) dirname(normalizePath(ofiles[length(ofiles)])) else "mock"
  source(file.path(here, "team_profiles.R"), local = FALSE)
})

GAMEPBP_COLUMNS <- c(
  "pff_GAMEDATE", "pff_GSISGAMEKEY", "pff_GSISPLAYID", "ABSOLUTEYARDLINE", "pff_CLOCK",
  "pff_GARBAGETIME", "pff_2MINUTE", "pff_QUARTER", "pff_OFFTEAM", "pff_DEFTEAM", "pff_RUNPASS",
  "pff_PLAYACTION", "pff_RUNPASSOPTION", "pff_SCREEN", "pff_PASSCOVERAGEBASIC", "pff_NUMRUSHERS",
  "pff_DOWN", "pff_DISTANCE", "pff_GAINLOSS", "pff_EXPECTEDPOINTS", "pff_EXPECTEDPOINTSADDED",
  "pff_YARDSAFTERCATCH", "pff_YARDSAFTERCONTACT", "pff_PENALTY", "pff_PASSRESULT", "pff_BALLCARRIER",
  "pff_TIMETOTHROW", "pff_TIMETOPRESSURE", "pff_PASSROUTETARGET", "pff_PASSROUTETARGETGROUP",
  "pff_SHOTGUN", "pff_PISTOL", "pff_FIRSTDOWNGAINED", "pff_OFFPERSONNELBASIC", "pff_OFFFORMATIONGROUP",
  "pff_BACKSET", "pff_OFFFORMATIONNAME", "pff_RUNCONCEPTPRIMARY", "pff_RUNCONCEPTSECONDARY",
  "pff_PASSCONCEPTHALFFIELDLEFT", "pff_PASSCONCEPTHALFFIELDRIGHT", "pff_PASSCONCEPTHALFFIELDSPECIAL",
  "pff_PASSCONCEPTTARGETED", "pff_OFFSUCCESS", "pff_DEFSUCCESS", "pff_BOXPLAYERS", "pff_PASSWIDTH",
  "pff_PASSDEPTH", "pff_DROPBACKDEPTH", "pff_DROPBACKTYPE", "pff_CATCHABLE", "pff_DEFFRONT",
  "pff_DEFPACKAGE", "pff_PASSRECEIVERTARGET", "pff_PASSRECEIVERPOSITIONTARGET",
  "pff_PASSRECEIVERROSTERPOSITIONTARGET"
)

PFF_CLUBS <- c("ARZ", "ATL", "BLT", "BUF", "CAR", "CHI", "CIN", "CLV", "DAL", "DEN", "DET", "GB",
               "HST", "IND", "JAX", "KC", "LA", "LAC", "LV", "MIA", "MIN", "NE", "NO", "NYG", "NYJ",
               "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")

# Expected points: linear fit to public nflverse 2025 plays (NYG, CLE, HOU, SF;
# EP ~ yardline + yardline^2 + down + distance; RMSE about 0.40).
mock_ep <- function(yl, down, dist) {
  6.49532 - 0.06876 * yl + 0.00006 * yl^2 - 0.53960 * (down == 2) - 1.21134 * (down == 3) -
    2.48497 * (down == 4) - 0.06633 * pmin(dist, 20)
}

rbern <- function(p) stats::runif(1) < p
pick <- function(x, w = NULL) if (length(x) == 1) x else sample(x, 1, prob = w)
logistic <- function(z) 1 / (1 + exp(-z))
logit <- function(p) log(p / (1 - p))

personnel_pass_effect <- function(p) {
  base <- sub("\\s*\\(\\*\\)$", "", p)
  eff <- switch(base, `10` = 1.2, `11` = 0.7, `12` = -0.2, `20` = -0.4, `21` = -0.35,
                `13` = -1.3, `22` = -1.4, `32` = -2, 0)
  if (grepl("\\(\\*\\)", p)) eff <- eff - 1.8
  eff
}

# ---------------------------------------------------------------------------
simulate_play <- function(st, prof, ids) {
  r <- list()
  down <- st$down; dist <- st$dist; yl <- st$yl
  late <- st$two_min; four_min <- st$four_min

  # ---- personnel ----------------------------------------------------------
  pers_names <- names(prof$personnel); w <- prof$personnel
  heavy <- grepl("^(13|22|32)|\\(\\*\\)", pers_names)
  light <- pers_names %in% c("10", "11")
  if ((down >= 3 && dist >= 4) || late) w <- ifelse(light, w * 6, w * 0.4)
  if ((dist <= 1 && down >= 3) || yl <= 2 || four_min) w <- ifelse(heavy, w * 6, w)
  pers <- pick(pers_names, w)

  # ---- run or pass --------------------------------------------------------
  z <- logit(prof$neutral_pass) + personnel_pass_effect(pers)
  if (down == 1) z <- z - 0.05
  if (down == 2) z <- z + 0.18 * (dist - 7)
  if (down >= 3) z <- z + ifelse(dist >= 4, 2.6, ifelse(dist >= 2, 0.9, -0.9))
  if (yl <= 3) z <- z - 0.8
  if (late) z <- z + 1.6
  if (four_min) z <- z - 1.8
  if (st$diff <= -14) z <- z + 0.8
  is_pass <- rbern(logistic(z))

  # ---- QB alignment -------------------------------------------------------
  g <- prof$shotgun[[pers]]
  pg <- logistic(logit(min(max(g, 0.02), 0.98)) + ifelse(is_pass, 1.3, -1.1) + ifelse(late, 2, 0))
  align <- if (!is_pass && rbern(prof$pistol * 1.6)) "Pistol" else if (rbern(pg)) "Shotgun" else "Under Center"

  r$pff_OFFPERSONNELBASIC <- pers
  r$pff_SHOTGUN <- if (align == "Shotgun") "S" else NA
  r$pff_PISTOL <- if (align == "Pistol") "P" else NA
  r$pff_RUNPASS <- if (is_pass) "P" else "R"
  r$pff_BACKSET <- switch(align,
    Shotgun = pick(c("GUN WEAK", "GUN STRONG", "GUN SPLIT", "GUN EMPTY"),
                   c(0.45, 0.3, ifelse(grepl("^2", pers), 0.4, 0.08), ifelse(pers %in% c("10", "11"), 0.08, 0.01))),
    Pistol = "PISTOL",
    `Under Center` = if (grepl("^2", pers)) pick(c("I", "I STRONG", "I WEAK", "OFFSET")) else pick(c("ACE", "DOT", "ACE STRONG"), c(.6, .3, .1))
  )
  r$pff_OFFFORMATIONGROUP <- pick(c("2x2", "3x1", "1x3", "2x1", "1x2", "1x1"),
                                  if (heavy[pers_names == pers]) c(.1, .05, .05, .35, .3, .15) else c(.32, .2, .2, .12, .12, .04))
  r$pff_OFFFORMATIONNAME <- pick(c("TRIPS - OPEN", "SLOT - PRO", "SLOT - SLOT", "PRO - OPEN", "TREY - OPEN",
                                   "SLOT - CLOSED", "TOAD WING - OPEN", "PRO - CLOSED", "CLOSED - CLOSED"))
  r$pff_DEFPACKAGE <- if (light[pers_names == pers]) pick(c("NICKEL", "DIME", "BASE"), c(.65, .2, .15)) else
    pick(c("BASE", "NICKEL", "HEAVY"), c(.55, .3, .15))
  r$pff_PASSCOVERAGEBASIC <- if (yl <= 3) "GOAL LINE" else pick(
    c("COVER 3", "COVER 1", "COVER 6", "COVER 4", "COVER 2", "COVER 0", "COVER 3 SEAM", "COVER 2 MAN"),
    c(.27, .2, .17, .1, .09, .05, .07, .05))

  qb <- ids[["QB"]]
  rush_pool <- names(prof$roster)[vapply(prof$roster, function(p) !is.null(p$rush_w) && p$pos != "QB", logical(1))]
  tgt_pool <- names(prof$roster)[vapply(prof$roster, function(p) !is.null(p$tgt), logical(1))]

  yards <- NA_real_; result <- NA_character_; carrier <- NA_character_; turnover <- FALSE
  r$pff_PLAYACTION <- 0; r$pff_RUNPASSOPTION <- 0; r$pff_SCREEN <- 0; r$pff_NUMRUSHERS <- 0

  if (!is_pass) {
    # ---------------- RUN ----------------
    sneak <- dist <= 1 && down >= 3 && rbern(0.4)
    qb_run <- !sneak && rbern(prof$qb_designed_run)
    concept <- if (sneak) "SNEAK" else pick(names(prof$run_concepts), prof$run_concepts)
    if (concept == "SNEAK" && !sneak) concept <- "INSIDE ZONE"
    who <- if (sneak || qb_run) "QB" else pick(rush_pool, vapply(prof$roster[rush_pool], `[[`, 0, "rush_w"))
    tilt <- if (who == "QB") 0.6 else prof$roster[[who]]$ypc %||% 0
    mu <- prof$concept_ypc[[concept]] + tilt
    if (sneak) mu <- 1.8
    yards <- round(stats::rnorm(1, mu - 1.6, 2.7))
    if (!sneak && rbern(0.075 * mu / 4.3)) yards <- yards + 6 + round(stats::rexp(1, 1 / 10))
    yards <- max(yards, -5)
    carrier <- ids[[who]]
    r$pff_RUNCONCEPTPRIMARY <- concept
    r$pff_RUNCONCEPTSECONDARY <- if (grepl("^2", pers) && concept %in% c("MAN", "INSIDE ZONE", "POWER") && rbern(.5)) "LEAD" else
      if (qb_run && rbern(.5)) "READ OPTION" else NA
    r$pff_RUNPASSOPTION <- as.numeric(align != "Under Center" && rbern(prof$rpo * 2))
    if (rbern(0.008)) turnover <- TRUE   # fumble lost
  } else {
    # ---------------- PASS ----------------
    pa <- rbern(prof$play_action * ifelse(align == "Under Center", 1.9, 0.55) * ifelse(down >= 3 & dist >= 5, 0.3, 1))
    screen <- !pa && rbern(prof$screen)
    r$pff_PLAYACTION <- as.numeric(pa); r$pff_SCREEN <- as.numeric(screen)
    r$pff_NUMRUSHERS <- pick(c(4, 5, 3, 6), c(.69, .2, .06, .05))
    p_press <- prof$pressure + ifelse(down >= 3 & dist >= 7, 0.08, 0) - ifelse(pa | screen, 0.1, 0) +
      0.05 * (r$pff_NUMRUSHERS - 4)
    pressured <- rbern(p_press)
    r$pff_TIMETOTHROW <- round(max(0.9, stats::rnorm(1, ifelse(pa, 3.1, 2.7), 0.7)), 1)
    r$pff_TIMETOPRESSURE <- if (pressured) round(stats::runif(1, 1.6, 3.6), 1) else NA
    r$pff_DROPBACKDEPTH <- if (align == "Shotgun") pick(c(6, 7, 8, 9), c(.15, .3, .35, .2)) else pick(c(7, 8, 9, 10), c(.2, .3, .3, .2))
    r$pff_DROPBACKTYPE <- if (pa) pick(c("SD", "SR", "SL", "RR"), c(.5, .2, .2, .1)) else pick(c("SD", "SR", "SL"), c(.85, .08, .07))

    if (rbern(ifelse(pressured, prof$sack_given_pressure, 0.012))) {
      result <- "SACK"; yards <- -round(stats::runif(1, 2, 11)); carrier <- qb
      if (rbern(0.06)) turnover <- TRUE
    } else if (rbern(ifelse(pressured, prof$scramble * 1.9, prof$scramble * 0.4))) {
      result <- "RUN"; yards <- round(max(-2, stats::rnorm(1, 5, 4) + ifelse(rbern(.1), 9, 0))); carrier <- qb
    } else if (rbern(ifelse(pressured, 0.07, 0.015))) {
      result <- "THROWN AWAY"
    } else {
      wts <- vapply(prof$roster[tgt_pool], `[[`, 0, "tgt")
      if (down == 3) for (b in names(prof$third_down_boost)) if (b %in% tgt_pool) wts[b] <- wts[b] * prof$third_down_boost[[b]]
      if (screen) wts <- wts * ifelse(grepl("^R|^F", tgt_pool), 4, 1)
      who <- pick(tgt_pool, wts); pl <- prof$roster[[who]]
      depth <- if (screen) round(stats::runif(1, -4, 0)) else round(pl$adot + stats::rnorm(1, 0, ifelse(pl$pos == "WR", 6.5, 4)))
      depth <- min(depth, yl)
      p_comp <- logistic(1.75 - 0.085 * depth + pl$skill + prof$catch_bonus - 0.7 * pressured)
      r$pff_PASSDEPTH <- depth
      r$pff_PASSWIDTH <- round(stats::runif(1, 2, 51))
      r$pff_PASSRECEIVERTARGET <- ids[[who]]
      r$pff_PASSRECEIVERROSTERPOSITIONTARGET <- pl$pos
      r$pff_PASSRECEIVERPOSITIONTARGET <- switch(pl$pos, HB = pick(c("HB-L", "HB-R", "HB")), FB = "FB-L",
                                                 pl$align %||% "RWR")
      grp <- if (depth <= 0) pick(c("H2", "SA", "0R")) else if (depth <= 5) pick(c("1R", "2R", "0R")) else
        if (depth <= 12) pick(c("3R", "4R", "6R")) else if (depth <= 19) pick(c("7R", "8R", "XR")) else "9R"
      r$pff_PASSROUTETARGETGROUP <- grp
      r$pff_PASSROUTETARGET <- paste0(substr(grp, 1, 1), "(", depth, ")")
      r$pff_CATCHABLE <- as.numeric(rbern(min(0.97, p_comp + 0.12)))
      if (rbern(prof$int_rate * (0.55 + max(depth, 0) / 25) * ifelse(pressured, 1.5, 1))) {
        result <- "INTERCEPTION"; turnover <- TRUE
      } else if (rbern(p_comp)) {
        result <- "COMPLETE"
        yac <- round(stats::rgamma(1, shape = 1.1, scale = pl$yac / 1.1))
        if (depth > 15) yac <- round(yac * 0.8)
        yards <- depth + yac
        if (yards > yl) { yac <- max(0, yl - depth); yards <- yl }
        r$pff_YARDSAFTERCATCH <- yac
        carrier <- ids[[who]]
        if (rbern(0.006)) turnover <- TRUE
      } else {
        result <- "INCOMPLETE"
      }
    }
    lc <- c("Iso Deep", "All Go", "Dagger", "Flat-7", "Stick", "Sail", "Dragon", "Ohio", "Hoss", "Quick Outs", "China", "Mesh")
    r$pff_PASSCONCEPTHALFFIELDLEFT <- pick(lc); r$pff_PASSCONCEPTHALFFIELDRIGHT <- pick(lc)
    r$pff_PASSCONCEPTHALFFIELDSPECIAL <- if (screen) pick(c("RB Screen", "WR Screen", "TE Screen")) else NA
    if (!is.null(r$pff_PASSRECEIVERTARGET)) r$pff_PASSCONCEPTTARGETED <- pick(c(r$pff_PASSCONCEPTHALFFIELDLEFT, r$pff_PASSCONCEPTHALFFIELDRIGHT))
  }

  if (!is.na(yards)) yards <- min(yards, yl)
  r$pff_GAINLOSS <- yards
  r$pff_PASSRESULT <- result
  r$pff_BALLCARRIER <- carrier
  if (!is.na(yards) && !is.na(carrier) && (is.na(result) || result %in% c("COMPLETE", "RUN"))) {
    yaco <- if (yards <= 0) 0 else max(0, min(yards, round(yards * stats::runif(1, 0.3, 0.75) + stats::rnorm(1, 0, 1))))
    r$pff_YARDSAFTERCONTACT <- yaco
  }

  # ---- outcome / EP --------------------------------------------------------
  g <- dplyr::coalesce(yards, 0)
  td <- !turnover && g >= yl
  fd <- !turnover && !td && g >= dist
  ep0 <- mock_ep(yl, down, dist)
  new_yl <- yl - g
  if (td) {
    epa <- 6.95 - ep0
  } else if (turnover) {
    epa <- -mock_ep(min(99, 100 - new_yl), 1, 10) - ep0
  } else if (fd) {
    epa <- mock_ep(new_yl, 1, min(10, new_yl)) - ep0
  } else if (down == 4) {
    epa <- -mock_ep(min(99, 100 - new_yl), 1, 10) - ep0
  } else {
    epa <- mock_ep(new_yl, down + 1, dist - g) - ep0
  }
  epa <- epa + stats::rnorm(1, 0, 0.12)
  r$pff_EXPECTEDPOINTS <- round(ep0 + stats::rnorm(1, 0, 0.15), 3)
  r$pff_EXPECTEDPOINTSADDED <- round(epa, 3)
  r$pff_FIRSTDOWNGAINED <- if (fd || td) 1 else NA

  std_ok <- g >= c(0.4, 0.6, 1, 1)[down] * dist
  code <- if (td || fd) pick(c("T", "1G"), c(.55, .45)) else if (std_ok && down <= 2) pick(c("T", "2A"), c(.65, .35)) else
    pick(c("F", "3R"), c(.65, .35))
  if (rbern(0.06)) code <- NA
  r$pff_OFFSUCCESS <- code
  r$pff_DEFSUCCESS <- if (is.na(code)) NA else c(T = "F", F = "T", `1G` = "3R", `3R` = "1G", `2A` = "2A")[[code]]
  r$pff_BOXPLAYERS <- as.character(if (is_pass) pick(6:7, c(.6, .4)) else pick(6:8, c(.3, .5, .2)))

  list(row = r, gain = g, td = td, fd = fd, turnover = turnover, new_yl = new_yl)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

simulate_game <- function(prof, ids, game_idx, opp, date, gamekey) {
  rows <- list(); play_id <- 40
  t <- 0                        # elapsed game seconds
  own <- 0; opp_pts <- 0
  we_receive_first <- rbern(0.5)
  if (!we_receive_first) t <- t + stats::runif(1, 120, 260)

  add_row <- function(r, st) {
    q <- min(4, floor(t / 900) + 1)
    left <- max(0, 900 * q - t)
    r$pff_QUARTER <- q
    r$pff_CLOCK <- sprintf("%02d:%02d", floor(left / 60), round(left %% 60) %% 60)
    r$pff_DOWN <- st$down; r$pff_DISTANCE <- st$dist; r$ABSOLUTEYARDLINE <- st$yl
    r$pff_2MINUTE <- if (st$two_min) 2 else if (st$four_min) 4 else 0
    r$pff_GARBAGETIME <- as.numeric(q == 4 && abs(own - opp_pts) >= 17 || q >= 3 && abs(own - opp_pts) >= 28)
    play_id <<- play_id + round(stats::runif(1, 18, 70))
    r$pff_GSISPLAYID <- play_id
    rows[[length(rows) + 1]] <<- r
  }

  while (t < 3540) {
    yl <- if (rbern(0.55)) 75 else round(stats::runif(1, 55, 88))
    if (rbern(0.08)) yl <- round(stats::runif(1, 25, 50))
    st <- list(down = 1, dist = min(10, yl), yl = yl)
    drive_over <- FALSE
    while (!drive_over && t < 3600) {
      q <- min(4, floor(t / 900) + 1); left_half <- ifelse(q <= 2, 1800 - t, 3600 - t)
      st$diff <- own - opp_pts
      st$two_min <- left_half <= 120 && (q == 2 || st$diff <= 0)
      st$four_min <- q == 4 && 3600 - t <= 240 && st$diff > 0
      # Victory formation
      if (q == 4 && 3600 - t <= 100 && st$diff > 0) {
        for (k in 1:min(3, 4 - st$down + 1)) {
          add_row(list(pff_RUNPASS = "R", pff_GAINLOSS = -1, pff_BALLCARRIER = ids[["QB"]], pff_BACKSET = "VICTORY",
                       pff_OFFPERSONNELBASIC = "22", pff_RUNCONCEPTPRIMARY = "UNDEFINED", pff_OFFFORMATIONGROUP = "1x1",
                       pff_OFFFORMATIONNAME = "CLOSED - CLOSED", pff_PLAYACTION = 0, pff_RUNPASSOPTION = 0,
                       pff_SCREEN = 0, pff_NUMRUSHERS = 0, pff_EXPECTEDPOINTS = round(mock_ep(st$yl, st$down, st$dist), 3),
                       pff_EXPECTEDPOINTSADDED = round(stats::rnorm(1, -0.05, 0.03), 3), pff_OFFSUCCESS = "F",
                       pff_DEFSUCCESS = "T", pff_DEFPACKAGE = "HEAVY", pff_PASSCOVERAGEBASIC = "GOAL LINE"), st)
          t <- t + 40; st$down <- st$down + 1; st$dist <- st$dist + 1; st$yl <- st$yl + 1
          if (st$down > 4) break
        }
        t <- 3600; break
      }
      # Pre-snap penalty (no play)
      if (rbern(0.015)) {
        add_row(list(pff_RUNPASS = "X", pff_PENALTY = paste(prof$team, sprintf("%02d", sample(60:79, 1)), "FST"),
                     pff_OFFPERSONNELBASIC = pick(names(prof$personnel), prof$personnel),
                     pff_PLAYACTION = 0, pff_RUNPASSOPTION = 0, pff_SCREEN = 0, pff_NUMRUSHERS = 0), st)
        st$yl <- min(99, st$yl + 5); st$dist <- st$dist + 5
        next
      }
      out <- simulate_play(st, prof, ids)
      add_row(out$row, st)
      t <- t + ifelse(st$two_min, stats::runif(1, 8, 22), stats::runif(1, 26, 42))
      if (out$td) { own <- own + 7; drive_over <- TRUE }
      else if (out$turnover) drive_over <- TRUE
      else if (out$fd) st <- list(down = 1, dist = min(10, out$new_yl), yl = out$new_yl)
      else if (st$down == 4) drive_over <- TRUE
      else {
        st$down <- st$down + 1; st$dist <- st$dist - out$gain; st$yl <- out$new_yl
        if (st$dist <= 0) st$dist <- 1
        if (st$down == 4) {
          go <- (st$dist <= 1 && st$yl <= 60 && rbern(0.55)) || (st$dist <= 3 && st$yl <= 4 && rbern(0.5)) ||
            (q == 4 && st$diff < 0 && 3600 - t < 360)
          if (!go) {
            if (st$yl <= 35 && rbern(0.82)) own <- own + 3
            drive_over <- TRUE
          }
        }
      }
      if (st$yl >= 100) drive_over <- TRUE
    }
    # Opponent possession
    t <- t + stats::runif(1, 80, 210)
    if (abs(t - 1800) < 200 && t > 1800 && t < 2000) t <- max(t, 1800)
    u <- stats::runif(1); opp_pts <- opp_pts + ifelse(u < 0.21, 7, ifelse(u < 0.36, 3, 0))
  }

  df <- dplyr::bind_rows(lapply(rows, function(r) {
    r <- r[!vapply(r, is.null, logical(1))]
    dplyr::as_tibble(lapply(r, function(v) if (length(v) == 0) NA else v))
  }))
  df$pff_GAMEDATE <- date; df$pff_GSISGAMEKEY <- gamekey
  df$pff_OFFTEAM <- prof$team; df$pff_DEFTEAM <- opp
  df
}

# Random but position-appropriate jersey numbers, unique within a team.
mock_jerseys <- function(prof) {
  ranges <- list(QB = 1:19, HB = 20:39, FB = 40:49, WR = c(10:19, 80:89), TE = 80:89)
  used <- integer()
  ids <- list()
  for (nm in names(prof$roster)) {
    pool <- setdiff(ranges[[prof$roster[[nm]]$pos]], used)
    n <- if (length(pool) == 1) pool else sample(pool, 1)
    used <- c(used, n)
    ids[[nm]] <- sprintf("%s %02d", prof$team, n)
  }
  ids
}

generate_mock_team <- function(prof, games = 8, out_dir = "mock", season_start = as.Date("2026-09-13"),
                               write = TRUE) {
  set.seed(prof$seed)
  ids <- mock_jerseys(prof)
  opps <- sample(setdiff(PFF_CLUBS, prof$team), games)
  bye <- sample(5:9, 1)
  weeks <- setdiff(seq_len(games + 1), bye)[seq_len(games)]
  dates <- season_start + 7 * (weeks - 1)
  keys <- 90000 + as.integer(factor(prof$team, levels = names(mock_profiles))) * 100 + seq_len(games)

  pbp <- dplyr::bind_rows(lapply(seq_len(games), function(i) simulate_game(prof, ids, i, opps[i], dates[i], keys[i])))
  for (col in setdiff(GAMEPBP_COLUMNS, names(pbp))) pbp[[col]] <- NA
  pbp <- pbp[, GAMEPBP_COLUMNS]

  if (write) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    readme <- data.frame(MOCK_DATA_NOTICE = c(
      paste0("MOCK DATA - ", prof$name, " offense, ", games, " simulated games."),
      "Simulated drive by drive from invented team profiles (mock/team_profiles.R) for pipeline testing and demonstration.",
      "Not real charting data. Player IDs look like jersey numbers but are randomly assigned placeholders, not real players.",
      "Opponents and dates are randomly assigned. Columns match the PFF-style GamePBP layout.",
      "pff_BOXPLAYERS holds only the box count; pff_DEFFRONT is left blank.",
      paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), " with seed ", prof$seed, "."),
      "", "Simulated roster key (jersey numbers are random within position ranges):",
      paste0(unlist(ids), " = simulated ", names(ids), " (", vapply(prof$roster, `[[`, "", "pos"), ")")
    ))
    path <- file.path(out_dir, paste0("MOCK_", prof$team, "_GamePBP.xlsx"))
    writexl::write_xlsx(list(GamePBP = pbp, README_MOCK = readme), path)
    message("Wrote ", path, " (", nrow(pbp), " rows)")
  }
  invisible(pbp)
}

generate_all_mocks <- function(out_dir = "mock", games = 8) {
  invisible(lapply(mock_profiles, generate_mock_team, games = games, out_dir = out_dir))
}

if (!interactive() && sys.nframe() == 0) generate_all_mocks()
