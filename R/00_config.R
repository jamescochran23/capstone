# ==========================================================
# 00_config.R - every tunable choice in one place
# ==========================================================
# Change values here (or pass overrides to build_scouting_report())
# instead of editing the analysis code. Each choice is also printed in
# the report's Methodology section so readers know what was assumed.

default_config <- function() {
  list(
    # --- Sample definition -------------------------------------------------
    sheet               = "GamePBP",   # Excel sheet to read (ignored for CSV)
    exclude_garbage     = TRUE,        # drop pff_GARBAGETIME == 1
    exclude_two_minute  = FALSE,       # set TRUE to drop 2- and 4-minute drills
    exclude_kneels      = TRUE,        # drop QB kneels (pff_BACKSET "VICTORY") and spikes
    valid_downs         = 1:4,         # down 0 = PATs / 2-pt tries

    # --- Success definitions -------------------------------------------------
    # PFF's pff_OFFSUCCESS column ("degree of success"). PFF does not publish
    # the code list, so the mapping below was inferred from a licensed PFF sample:
    #   T, 1G, 2A -> successful (1G = 1st down gained on 74-100% of plays;
    #                2A = 1st/2nd-down gains meeting the 40/60% threshold)
    #   F, 3R     -> unsuccessful (3R/F plays almost never meet the threshold)
    # Codes not listed are treated as unknown (NA) and excluded.
    pff_success_codes   = c("T", "1G", "2A", "1", "TRUE"),
    pff_failure_codes   = c("F", "3R", "0", "FALSE"),
    # Standard (yardage-based) success: share of the line to gain needed
    # on each down. 1st: 40%, 2nd: 60%, 3rd/4th: 100%.
    std_success_share   = c(`1` = 0.40, `2` = 0.60, `3` = 1.00, `4` = 1.00),
    # Which success measure drives the headline numbers: "std" or "pff".
    primary_success     = "std",

    # --- Personnel -----------------------------------------------------------
    # TRUE folds tagged groups such as "21 (*)" into "21".
    merge_personnel_tags = FALSE,

    # --- Explosive-play thresholds (yards) ----------------------------------
    explosive_run       = 10,
    explosive_pass      = 20,

    # --- Buckets -------------------------------------------------------------
    distance_breaks     = c(0, 3, 6, 10, Inf),
    distance_labels     = c("Short (1-3)", "Medium (4-6)", "Long (7-10)", "Extra Long (11+)"),
    # ABSOLUTEYARDLINE is yards TO the opponent's end zone (per the PFF
    # reference sheet; expected points confirm it), so small = close to scoring.
    field_breaks        = c(-Inf, 10, 20, 50, 80, Inf),
    field_labels        = c("Goal to Go (1-10)", "Red Zone (11-20)", "Plus Territory (21-50)",
                            "Own Territory (51-80)", "Backed Up (81-99)"),

    # --- Reporting thresholds ------------------------------------------------
    min_n_group         = 10,   # minimum plays for a group to appear in charts / insights
    min_n_concept       = 5,    # minimum carries for a run concept
    min_targets         = 5,    # minimum targets for a receiver to be ranked
    min_carries         = 10,   # minimum carries for a rusher to be ranked
    tell_threshold      = 0.80, # run or pass rate at/above this counts as a "tell"
    top_n_players       = 8,

    # --- Player names ----------------------------------------------------------
    # The data only has club + jersey ("SF 23"). A roster CSV with columns
    # player_id, player_name (optional: position) adds names to every table
    # and chart. NULL = look for data/rosters/<TEAM>_roster.csv automatically.
    roster_file         = NULL,

    # --- Look ----------------------------------------------------------------
    team_name           = NULL,  # display name; defaults to the club code
    # NULL = use the team's colors from TEAM_COLORS (primary = pass,
    # secondary = run); set hex codes here to override.
    color_pass          = NULL,
    color_run           = NULL,
    mock_data           = FALSE, # TRUE stamps MOCK DATA on every page and chart

    # --- Data source -------------------------------------------------------
    # "pff" = PFF GamePBP file (native). "nflverse" = public nflverse data
    # mapped by build_nflverse_report() (R/07_nflverse.R); the report then
    # lists what nflverse cannot supply in data_source_notes.
    data_source         = "pff",
    data_source_notes   = character(),
    run_concept_label   = "Run concepts"
  )
}

# PFF club codes -> display names (used when cfg$team_name is NULL).
TEAM_NAMES <- c(
  ARZ = "Arizona Cardinals", ATL = "Atlanta Falcons", BLT = "Baltimore Ravens", BUF = "Buffalo Bills",
  CAR = "Carolina Panthers", CHI = "Chicago Bears", CIN = "Cincinnati Bengals", CLV = "Cleveland Browns",
  DAL = "Dallas Cowboys", DEN = "Denver Broncos", DET = "Detroit Lions", GB = "Green Bay Packers",
  HST = "Houston Texans", IND = "Indianapolis Colts", JAX = "Jacksonville Jaguars", KC = "Kansas City Chiefs",
  LA = "Los Angeles Rams", LAC = "Los Angeles Chargers", LV = "Las Vegas Raiders", MIA = "Miami Dolphins",
  MIN = "Minnesota Vikings", NE = "New England Patriots", NO = "New Orleans Saints", NYG = "New York Giants",
  NYJ = "New York Jets", PHI = "Philadelphia Eagles", PIT = "Pittsburgh Steelers", SEA = "Seattle Seahawks",
  SF = "San Francisco 49ers", TB = "Tampa Bay Buccaneers", TEN = "Tennessee Titans", WAS = "Washington Commanders"
)

# Team colors: primary (used for Pass) and secondary (used for Run).
# Where a club's secondary color is too close to its primary or too pale
# to read on white, a darker official accent is used instead.
TEAM_COLORS <- list(
  ARZ = c("#97233F", "#000000"), ATL = c("#A71930", "#000000"), BLT = c("#241773", "#9E7C0C"),
  BUF = c("#00338D", "#C60C30"), CAR = c("#0085CA", "#101820"), CHI = c("#0B162A", "#C83803"),
  CIN = c("#000000", "#FB4F14"), CLV = c("#311D00", "#FF3C00"), DAL = c("#003594", "#869397"),
  DEN = c("#002244", "#FB4F14"), DET = c("#0076B6", "#5A6B75"), GB = c("#203731", "#C49A00"),
  HST = c("#03202F", "#A71930"), IND = c("#002C5F", "#A2AAAD"), JAX = c("#006778", "#9F792C"),
  KC = c("#E31837", "#C29A00"), LA = c("#003594", "#D4A500"), LAC = c("#0080C6", "#C9A100"),
  LV = c("#000000", "#A5ACAF"), MIA = c("#008E97", "#FC4C02"), MIN = c("#4F2683", "#D4A500"),
  NE = c("#002244", "#C60C30"), NO = c("#101820", "#A28D5B"), NYG = c("#0B2265", "#A71930"),
  NYJ = c("#125740", "#000000"), PHI = c("#004C54", "#A5ACAF"), PIT = c("#101820", "#D4A000"),
  SEA = c("#002244", "#69BE28"), SF = c("#AA0000", "#B3995D"), TB = c("#D50A0A", "#34302B"),
  TEN = c("#0C2340", "#4B92DB"), WAS = c("#5A1414", "#D4A000")
)

merge_config <- function(overrides = list()) {
  cfg <- default_config()
  unknown <- setdiff(names(overrides), names(cfg))
  if (length(unknown) > 0) {
    stop("Unknown config option(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  cfg[names(overrides)] <- overrides
  cfg
}
