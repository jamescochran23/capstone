# ==========================================================
# team_profiles.R - knobs for the MOCK play-by-play generator
# ==========================================================
# !! MOCK DATA !!  These profiles are invented to give each simulated
# offense a distinct, plausible "personality" so the pipeline can be
# stress-tested. They are loosely styled on widely discussed schematic
# identities but are NOT derived from real charting data, and no number
# here should be read as a fact about the real team or its players.
#
# Club codes follow the PFF convention used in the source file
# (e.g. HST for Houston, CLV for Cleveland, BLT for Baltimore).
#
# Each roster entry: id suffix, roster position, receiving weight, aDOT,
# YAC/rec mean, catch skill (+/- logit), rushing weight, YPC tilt.

mock_profiles <- list(

  NYG = list(
    team = "NYG", name = "New York Giants (MOCK)", seed = 1101,
    neutral_pass = 0.60,
    personnel = c(`11` = 0.70, `12` = 0.16, `13` = 0.04, `21` = 0.05, `11 (*)` = 0.03, `22` = 0.02),
    shotgun = c(`11` = 0.86, `12` = 0.55, `13` = 0.20, `21` = 0.40, `11 (*)` = 0.6, `22` = 0.15),
    pistol = 0.06,
    run_concepts = c(`INSIDE ZONE` = 0.30, `MAN` = 0.22, `OUTSIDE ZONE` = 0.14, `POWER` = 0.10,
                     `COUNTER` = 0.09, `DRAW` = 0.08, `TRAP` = 0.03, `SNEAK` = 0.02, `TRICK` = 0.02),
    concept_ypc = c(`INSIDE ZONE` = 4.4, `MAN` = 4.0, `OUTSIDE ZONE` = 3.6, `POWER` = 4.9,
                    `COUNTER` = 5.4, `DRAW` = 5.0, `TRAP` = 3.5, `SNEAK` = 1.8, `TRICK` = 6.5),
    qb_designed_run = 0.10, play_action = 0.17, rpo = 0.06, screen = 0.07,
    pressure = 0.40, sack_given_pressure = 0.24, scramble = 0.07, int_rate = 0.022,
    catch_bonus = -0.05,
    roster = list(
      QB = list(id = "Q1", pos = "QB", rush_w = 1),
      RB1 = list(id = "R1", pos = "HB", tgt = 0.10, adot = 0.5, yac = 7.5, skill = 0.3, rush_w = 0.58, ypc = 0.4),
      RB2 = list(id = "R2", pos = "HB", tgt = 0.04, adot = 0.0, yac = 6.0, skill = 0.0, rush_w = 0.30, ypc = -0.3),
      WR1 = list(id = "W1", pos = "WR", tgt = 0.27, adot = 11.5, yac = 5.2, skill = 0.35, align = "RWR"),
      WR2 = list(id = "W2", pos = "WR", tgt = 0.16, adot = 13.5, yac = 3.6, skill = -0.15, align = "LWR"),
      WR3 = list(id = "W3", pos = "WR", tgt = 0.14, adot = 7.0, yac = 4.5, skill = 0.10, align = "SLWR"),
      WR4 = list(id = "W4", pos = "WR", tgt = 0.06, adot = 16.0, yac = 3.0, skill = -0.30, align = "RWR"),
      TE1 = list(id = "T1", pos = "TE", tgt = 0.17, adot = 7.5, yac = 5.0, skill = 0.25, align = "TE-R"),
      TE2 = list(id = "T2", pos = "TE", tgt = 0.04, adot = 5.0, yac = 3.0, skill = 0.0, align = "TE-L")
    ),
    third_down_boost = c(WR3 = 1.6, TE1 = 1.3)
  ),

  CLV = list(
    team = "CLV", name = "Cleveland Browns (MOCK)", seed = 1102,
    neutral_pass = 0.55,
    personnel = c(`11` = 0.52, `12` = 0.27, `13` = 0.06, `21` = 0.07, `12 (*)` = 0.05, `22` = 0.03),
    shotgun = c(`11` = 0.80, `12` = 0.40, `13` = 0.12, `21` = 0.30, `12 (*)` = 0.15, `22` = 0.10),
    pistol = 0.03,
    run_concepts = c(`OUTSIDE ZONE` = 0.34, `INSIDE ZONE` = 0.18, `MAN` = 0.14, `POWER` = 0.10,
                     `COUNTER` = 0.08, `DRAW` = 0.07, `PULL LEAD` = 0.04, `SNEAK` = 0.03, `TRICK` = 0.02),
    concept_ypc = c(`OUTSIDE ZONE` = 4.9, `INSIDE ZONE` = 4.0, `MAN` = 3.9, `POWER` = 4.2,
                    `COUNTER` = 4.6, `DRAW` = 3.2, `PULL LEAD` = 5.2, `SNEAK` = 1.6, `TRICK` = 5.0),
    qb_designed_run = 0.03, play_action = 0.24, rpo = 0.03, screen = 0.09,
    pressure = 0.38, sack_given_pressure = 0.22, scramble = 0.04, int_rate = 0.030,
    catch_bonus = -0.10,
    roster = list(
      QB = list(id = "Q1", pos = "QB", rush_w = 1),
      RB1 = list(id = "R1", pos = "HB", tgt = 0.08, adot = 0.0, yac = 6.5, skill = 0.2, rush_w = 0.62, ypc = 0.5),
      RB2 = list(id = "R2", pos = "HB", tgt = 0.05, adot = 1.0, yac = 5.5, skill = 0.1, rush_w = 0.33, ypc = -0.4),
      WR1 = list(id = "W1", pos = "WR", tgt = 0.25, adot = 13.0, yac = 4.0, skill = 0.20, align = "LWR"),
      WR2 = list(id = "W2", pos = "WR", tgt = 0.14, adot = 10.0, yac = 4.4, skill = 0.0, align = "RWR"),
      WR3 = list(id = "W3", pos = "WR", tgt = 0.09, adot = 8.0, yac = 3.5, skill = -0.1, align = "SLWR"),
      TE1 = list(id = "T1", pos = "TE", tgt = 0.24, adot = 8.5, yac = 4.8, skill = 0.45, align = "TE-R"),
      TE2 = list(id = "T2", pos = "TE", tgt = 0.08, adot = 6.0, yac = 4.0, skill = 0.10, align = "TE-L")
    ),
    third_down_boost = c(TE1 = 1.7, WR2 = 1.2)
  ),

  HST = list(
    team = "HST", name = "Houston Texans (MOCK)", seed = 1103,
    neutral_pass = 0.58,
    personnel = c(`11` = 0.64, `12` = 0.17, `21` = 0.10, `13` = 0.03, `21 (*)` = 0.03, `10` = 0.03),
    shotgun = c(`11` = 0.80, `12` = 0.45, `21` = 0.35, `13` = 0.15, `21 (*)` = 0.10, `10` = 1.0),
    pistol = 0.08,
    run_concepts = c(`OUTSIDE ZONE` = 0.38, `INSIDE ZONE` = 0.16, `MAN` = 0.12, `COUNTER` = 0.09,
                     `DRAW` = 0.09, `POWER` = 0.07, `TRICK` = 0.04, `SNEAK` = 0.03, `PULL LEAD` = 0.02),
    concept_ypc = c(`OUTSIDE ZONE` = 4.1, `INSIDE ZONE` = 4.5, `MAN` = 3.4, `COUNTER` = 4.8,
                    `DRAW` = 5.6, `POWER` = 3.8, `TRICK` = 7.0, `SNEAK` = 1.9, `PULL LEAD` = 3.5),
    qb_designed_run = 0.07, play_action = 0.21, rpo = 0.05, screen = 0.06,
    pressure = 0.36, sack_given_pressure = 0.26, scramble = 0.06, int_rate = 0.024,
    catch_bonus = 0.0,
    roster = list(
      QB = list(id = "Q1", pos = "QB", rush_w = 1),
      RB1 = list(id = "R1", pos = "HB", tgt = 0.07, adot = 0.0, yac = 6.0, skill = 0.1, rush_w = 0.66, ypc = 0.2),
      RB2 = list(id = "R2", pos = "HB", tgt = 0.03, adot = 1.0, yac = 7.0, skill = 0.0, rush_w = 0.28, ypc = 0.0),
      FB1 = list(id = "F1", pos = "FB", tgt = 0.02, adot = 2.0, yac = 4.0, skill = 0.2, rush_w = 0.03, ypc = -1),
      WR1 = list(id = "W1", pos = "WR", tgt = 0.26, adot = 12.5, yac = 4.6, skill = 0.40, align = "LWR"),
      WR2 = list(id = "W2", pos = "WR", tgt = 0.20, adot = 15.5, yac = 3.4, skill = 0.15, align = "RWR"),
      WR3 = list(id = "W3", pos = "WR", tgt = 0.13, adot = 8.5, yac = 5.5, skill = 0.25, align = "SLiWR"),
      TE1 = list(id = "T1", pos = "TE", tgt = 0.14, adot = 7.0, yac = 4.2, skill = 0.15, align = "TE-R"),
      TE2 = list(id = "T2", pos = "TE", tgt = 0.03, adot = 4.0, yac = 3.0, skill = 0.0, align = "TE-L")
    ),
    third_down_boost = c(WR1 = 1.4, WR3 = 1.5)
  ),

  SF = list(
    team = "SF", name = "San Francisco 49ers (MOCK)", seed = 1104,
    neutral_pass = 0.50,
    personnel = c(`21` = 0.38, `11` = 0.34, `12` = 0.12, `22` = 0.06, `21 (*)` = 0.04, `20` = 0.03, `13` = 0.03),
    shotgun = c(`21` = 0.25, `11` = 0.70, `12` = 0.30, `22` = 0.10, `21 (*)` = 0.05, `20` = 0.55, `13` = 0.10),
    pistol = 0.04,
    run_concepts = c(`OUTSIDE ZONE` = 0.36, `INSIDE ZONE` = 0.14, `MAN` = 0.10, `COUNTER` = 0.08,
                     `TRAP` = 0.08, `PULL LEAD` = 0.06, `TRICK` = 0.07, `DRAW` = 0.05, `POWER` = 0.04,
                     `SNEAK` = 0.02),
    concept_ypc = c(`OUTSIDE ZONE` = 5.1, `INSIDE ZONE` = 4.6, `MAN` = 3.9, `COUNTER` = 5.0,
                    `TRAP` = 5.6, `PULL LEAD` = 4.4, `TRICK` = 8.0, `DRAW` = 4.0, `POWER` = 3.6, `SNEAK` = 1.7),
    qb_designed_run = 0.02, play_action = 0.30, rpo = 0.02, screen = 0.10,
    pressure = 0.32, sack_given_pressure = 0.20, scramble = 0.04, int_rate = 0.025,
    catch_bonus = 0.10,
    roster = list(
      QB = list(id = "Q1", pos = "QB", rush_w = 1),
      RB1 = list(id = "R1", pos = "HB", tgt = 0.16, adot = 1.0, yac = 8.5, skill = 0.5, rush_w = 0.70, ypc = 0.6),
      RB2 = list(id = "R2", pos = "HB", tgt = 0.03, adot = 0.0, yac = 5.0, skill = 0.0, rush_w = 0.18, ypc = -0.4),
      FB1 = list(id = "F1", pos = "FB", tgt = 0.05, adot = 3.0, yac = 5.0, skill = 0.3, rush_w = 0.04, ypc = -0.8),
      WR1 = list(id = "W1", pos = "WR", tgt = 0.22, adot = 9.5, yac = 7.4, skill = 0.30, align = "RWR"),
      WR2 = list(id = "W2", pos = "WR", tgt = 0.17, adot = 11.5, yac = 4.4, skill = 0.15, align = "LWR"),
      WR3 = list(id = "W3", pos = "WR", tgt = 0.06, adot = 14.0, yac = 3.0, skill = -0.1, align = "SLWR"),
      WR4 = list(id = "W4", pos = "WR", tgt = 0.03, adot = 4.0, yac = 8.0, skill = 0.0, align = "RWR", rush_w = 0.03, ypc = 3.0),
      TE1 = list(id = "T1", pos = "TE", tgt = 0.24, adot = 8.5, yac = 6.2, skill = 0.40, align = "TE-R")
    ),
    third_down_boost = c(TE1 = 1.5, WR1 = 1.3)
  )
)
