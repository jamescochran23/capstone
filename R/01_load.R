# ==========================================================
# 01_load.R - read any team's play-by-play and validate it
# ==========================================================
# Accepts .xlsx/.xls (sheet "GamePBP" by default) or .csv in the same
# column layout as a PFF GamePBP export. Everything is read
# as text first and converted deliberately, which avoids the Excel type
# problems in the original script (numbers stored as text, 0/1 flags
# formatted as times, and so on).

# Columns the report cannot run without.
REQUIRED_COLS <- c(
  "pff_gsisgamekey", "pff_offteam", "pff_runpass",
  "pff_down", "pff_distance", "pff_gainloss"
)

# Columns that power individual sections. If one is missing the pipeline
# adds it as all-NA, warns, and the affected section says "not available".
OPTIONAL_COLS <- c(
  "pff_gamedate", "pff_gsisplayid", "absoluteyardline", "pff_clock",
  "pff_garbagetime", "pff_2minute", "pff_quarter", "pff_defteam",
  "pff_playaction", "pff_runpassoption", "pff_screen", "pff_numrushers",
  "pff_expectedpoints", "pff_expectedpointsadded", "pff_yardsaftercatch",
  "pff_yardsaftercontact", "pff_penalty", "pff_passresult", "pff_ballcarrier",
  "pff_timetothrow", "pff_timetopressure", "pff_passroutetargetgroup",
  "pff_shotgun", "pff_pistol", "pff_firstdowngained", "pff_offpersonnelbasic",
  "pff_offformationgroup", "pff_backset", "pff_runconceptprimary",
  "pff_runconceptsecondary", "pff_passconcepttargeted", "pff_offsuccess",
  "pff_passdepth", "pff_passwidth", "pff_catchable", "pff_defpackage",
  "pff_passcoveragebasic", "pff_passreceivertarget",
  "pff_passreceiverpositiontarget", "pff_passreceiverrosterpositiontarget"
)

NUMERIC_COLS <- c(
  "pff_gsisgamekey", "pff_gsisplayid", "absoluteyardline", "pff_garbagetime",
  "pff_2minute", "pff_quarter", "pff_playaction", "pff_runpassoption",
  "pff_screen", "pff_numrushers", "pff_down", "pff_distance", "pff_gainloss",
  "pff_expectedpoints", "pff_expectedpointsadded", "pff_yardsaftercatch",
  "pff_yardsaftercontact", "pff_timetothrow", "pff_timetopressure",
  "pff_firstdowngained", "pff_passdepth", "pff_passwidth", "pff_catchable"
)

standardize_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

to_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL", "N/A", "-")] <- NA
  x[toupper(x) %in% c("T", "TRUE", "Y", "YES")] <- "1"
  x[toupper(x) %in% c("F", "FALSE", "N", "NO")] <- "0"
  suppressWarnings(as.numeric(x))
}

# Read the raw file. Returns a tibble with standardized names, all text.
read_pbp_file <- function(path, sheet = "GamePBP") {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))

  raw <- switch(
    ext,
    xlsx = ,
    xls = {
      sheets <- readxl::excel_sheets(path)
      if (!sheet %in% sheets) {
        stop("Sheet '", sheet, "' not found. Sheets in this file: ",
             paste(sheets, collapse = ", "), call. = FALSE)
      }
      readxl::read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal")
    },
    csv = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                          show_col_types = FALSE, progress = FALSE),
    stop("Unsupported file type '.", ext, "'. Use .xlsx, .xls or .csv.", call. = FALSE)
  )

  names(raw) <- standardize_names(names(raw))
  dplyr::as_tibble(raw)
}

# Check the schema, add missing optional columns, convert types.
# Returns list(data = tibble, issues = character vector of warnings).
validate_pbp <- function(raw) {
  issues <- character()

  missing_req <- setdiff(REQUIRED_COLS, names(raw))
  if (length(missing_req) > 0) {
    stop("These required columns are missing: ", paste(missing_req, collapse = ", "),
         "\nColumns found: ", paste(names(raw), collapse = ", "), call. = FALSE)
  }

  missing_opt <- setdiff(OPTIONAL_COLS, names(raw))
  if (length(missing_opt) > 0) {
    issues <- c(issues, paste0("Missing optional column(s), related metrics shown as unavailable: ",
                               paste(missing_opt, collapse = ", ")))
    for (col in missing_opt) raw[[col]] <- NA_character_
  }

  # Trim every text column; empty strings become NA.
  raw <- dplyr::mutate(raw, dplyr::across(dplyr::everything(), function(v) {
    v <- trimws(as.character(v))
    v[v == ""] <- NA
    v
  }))

  for (col in intersect(NUMERIC_COLS, names(raw))) {
    before_na <- sum(is.na(raw[[col]]))
    raw[[col]] <- to_numeric(raw[[col]])
    coerced <- sum(is.na(raw[[col]])) - before_na
    if (coerced > 0) {
      issues <- c(issues, sprintf("%s: %d value(s) were not numeric and were set to NA.", col, coerced))
    }
  }

  # Excel sometimes stores 0/1 flags with a time format; when read as text
  # they arrive as day fractions. Anything > 0 is a "yes".
  for (flag in c("pff_screen", "pff_playaction", "pff_runpassoption", "pff_garbagetime")) {
    raw[[flag]] <- dplyr::if_else(is.na(raw[[flag]]), NA_real_, as.numeric(raw[[flag]] > 0))
  }

  # Dates read as text arrive as Excel serial numbers ("46005") or ISO text.
  gd <- raw$pff_gamedate
  serial <- suppressWarnings(as.numeric(gd))
  raw$pff_gamedate <- dplyr::if_else(
    !is.na(serial),
    as.Date(serial, origin = "1899-12-30"),
    as.Date(ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}", gd), substr(gd, 1, 10), NA_character_))
  )

  if (nrow(raw) == 0) stop("The file has no rows.", call. = FALSE)

  list(data = raw, issues = issues)
}

load_pbp <- function(path, sheet = "GamePBP") {
  validate_pbp(read_pbp_file(path, sheet))
}
