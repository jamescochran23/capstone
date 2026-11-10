# Offensive Scouting Pipeline (R)

**One function call turns a team's play-by-play into a coach-ready offensive
scouting report**: tendencies, strengths and vulnerabilities, as HTML and PDF,
with every table as CSV/Excel and every chart as PNG.

```r
source("scouting.R")
build_scouting_report("path/to/GamePBP.xlsx", team = "BUF")
```

Built by James Cochran (Texas A&M University) as a master's capstone and
presented at the Texas A&M Football Analytics Symposium, November 2026.

## What the report answers

| Scouting question | Report section |
|---|---|
| Personnel run/pass tendencies | 1. Personnel (with 95% intervals) |
| Down-and-distance tendencies | 2. Down & distance heatmap + field zones |
| QB alignment (under center / shotgun / pistol) | 3. QB alignment, alignment x personnel |
| Most used and most effective run concepts | 4. Run concepts: volume vs EPA, stuff rate |
| Third-down behavior and key targets | 5. Third down by distance, target leaders |
| Most effective players and how they produce | 6. Receivers (aDOT vs YAC), rushers, total EPA |

Also: passing context (play action, screens, pressure, depth, coverage),
pre-snap "tells", a run/pass predictability score, and auto-written findings,
each with its sample size. Reports use each team's colors.

## Sample output (real public data)

`output_nflverse/` has full 2025-season reports built from free nflverse data:

- [New York Giants](output_nflverse/NYG/NYG_scouting_report.pdf)
- [Cleveland Browns](output_nflverse/CLV/CLV_scouting_report.pdf)
- [Houston Texans](output_nflverse/HST/HST_scouting_report.pdf)
- [San Francisco 49ers](output_nflverse/SF/SF_scouting_report.pdf)
- [Cross-team summary](output_nflverse/00_team_summary.pdf)

`output_nflverse_2026/` has the same four offenses for the 2026 season through
Week 9 (pulled November 10, 2026). nflverse had not posted 2026 participation
data yet, so these reports have no personnel, formation, coverage or pressure
sections, and the predictability score uses QB alignment, down and distance only.
Rebuild with `build_nflverse_report("NYG", season = 2026, out_dir = "output_nflverse_2026")`.

Download the `.html` versions for the interactive layout.

## How to run

1. Install R 4.1+ (RStudio recommended; it bundles pandoc). Chrome or Edge is
   used to print the PDFs; no LaTeX needed.
2. Clone or download this repo and set it as the working directory.
3. Install packages once: `Rscript install_packages.R`
   (missing packages also install automatically on first run).

Then pick a data source:

```r
source("scouting.R")

# A. Rebuild the sample reports from the CSVs in data/nflverse/ (offline)
rebuild_nflverse_report("SF")

# B. Any team, any season, from nflverse (downloads with {nflreadr})
build_nflverse_report("KC", season = 2025)
build_nflverse_report("CLE", season = 2025, weeks = 10:18)

# C. Your own PFF-style GamePBP export (.xlsx sheet "GamePBP", or .csv)
build_scouting_report("path/to/file.xlsx", team = "BUF")
```

From a terminal:

```
Rscript run_nflverse.R        # NYG, CLE, HOU, SF reports + summary (downloads)
Rscript run_all.R             # mock-data demo: 4 mock files, reports, summary
Rscript tests/stress_test.R   # 30 stress / regression checks
```

Outputs land in `output/<TEAM>/` or `output_nflverse/<TEAM>/`.

## Data sources

| Source | Gives the report |
|---|---|
| **PFF-style GamePBP** (native input, 56 columns) | everything, including run concepts, PFF success, yards after contact, receiver alignment |
| nflverse `load_pbp()` (nflfastR) | down, distance, yard line, result, yards, EPA, target, ball carrier, run gap |
| nflverse `load_participation()` (NGS) | personnel, formation, defensive package, coverage, route, pressure, rushers |
| nflverse `load_ftn_charting()` (FTN) | QB alignment, play action, screen, RPO |
| nflverse `load_rosters_weekly()` | player names and jersey numbers |

The pipeline was developed on a licensed PFF play-by-play sample. That data is
**not** included in this repository, and neither are any results from it. The
nflverse adapter (`R/07_nflverse.R`) was checked play by play against that
sample before use.

**What nflverse can't see** (also printed in each report): blocking-scheme run
concepts (section 4 becomes run gap and direction), PFF success codes (the
standard 40/60/100 rule is used), yards after contact, and receiver alignment.
EPA is nflfastR's public model. Pressure is a yes/no flag. Participation data
(personnel, coverage, routes) is posted after the season.

## Data assumptions

Every choice lives in `R/00_config.R` and can be overridden per run, and each
report prints them in its Methodology section.

- **Sample:** one offense, run/pass plays only, downs 1-4, no garbage time, no
  QB kneels or spikes. Each filter step is counted in the report.
- **Field position:** `ABSOLUTEYARDLINE` is yards **to** the opponent's end
  zone, so 1-20 is the red zone.
- **Success:** the standard rule (40% of the line to gain on 1st down, 60% on
  2nd, 100% on 3rd/4th) is the headline. PFF's `pff_OFFSUCCESS` codes are not
  published; the inferred mapping is T / 1G / 2A = success and F / 3R = failure.
- **Personnel "(*)"** (for example "21 (*)") is a jumbo package with an extra
  offensive lineman. It is kept separate by default
  (`merge_personnel_tags = TRUE` folds it in).
- Incomplete passes count as 0 yards; sacks and scrambles are dropbacks but not
  targets. First down = first-down flag or yards >= distance.
- **Player names:** add `data/rosters/<TEAM>_roster.csv` with columns
  `player_id,player_name` (for example `SF 23,Christian McCaffrey`).

```r
build_scouting_report(file, team = "BUF", config = list(
  merge_personnel_tags = TRUE,   # fold "21 (*)" jumbo into "21"
  primary_success = "pff",       # headline success from pff_OFFSUCCESS
  exclude_garbage = FALSE,
  color_pass = "#000000", color_run = "#FFB612"
))
```

## Mock data

`mock/` holds four **MOCK** GamePBP files (Giants, Browns, Texans, 49ers)
simulated drive by drive from invented team profiles, to stress-test the
pipeline in the full PFF layout. Their numbers and jersey IDs are illustrative
and do not describe the real teams. Seeds are fixed, so every rebuild is
identical.

## Folder map

```
scouting.R            load everything / command-line entry
install_packages.R    one-time package setup
run_nflverse.R        real-data reports for NYG, CLE, HOU, SF
run_all.R             mock-data demo
R/00_config.R         settings, team names and colors
R/01_load.R           read + validate any GamePBP file
R/02_prepare.R        sample filters and derived fields
R/03_analysis.R       one function per scouting question
R/04_plots.R          charts
R/05_insights.R       rule-based findings
R/06_pipeline.R       build_scouting_report(), build_summary(), PDF printing
R/07_nflverse.R       nflverse -> GamePBP adapter
report/               R Markdown templates + CSS
mock/                 mock generator, profiles, MOCK_*.xlsx
data/nflverse/        2025 and 2026 GamePBP CSVs and rosters built from nflverse
output_nflverse/      sample reports, 2025 season
output_nflverse_2026/ sample reports, 2026 Weeks 1-9
tests/stress_test.R   stress / regression checks
```

## Credits

- Play-by-play, participation and rosters: [nflverse](https://nflverse.nflverse.com/)
  ({nflreadr}, nflfastR), NFL Next Gen Stats participation, and
  [FTN Data](https://ftndata.com/) charting via nflverse.
- GamePBP column layout: Pro Football Focus (PFF).
- Built in R with dplyr, tidyr, ggplot2, readxl, writexl, rmarkdown and knitr.
- Team colors are used for identification only. This project is not affiliated
  with the NFL, any NFL club, PFF or FTN.

## License

[MIT](LICENSE) © 2026 James Cochran
