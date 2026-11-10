# ==========================================================
# 04_plots.R - charts for each section
# ==========================================================
# All charts share one theme. Pass = blue, Run = orange everywhere, so a
# reader learns the encoding once. When cfg$mock_data is TRUE every chart
# carries a "MOCK DATA" caption.

theme_scout <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      plot.caption = ggplot2::element_text(colour = "grey45", hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.justification = "left",
      strip.text = ggplot2::element_text(face = "bold")
    )
}

scout_caption <- function(cfg, extra = NULL) {
  base <- if (isTRUE(cfg$mock_data)) "MOCK DATA - simulated for demonstration, not real play-by-play." else
    if (identical(cfg$data_source, "nflverse")) "Data: nflverse (nflfastR pbp, NGS participation, FTN charting)" else NULL
  paste(c(extra, base), collapse = "  ")
}

# Black or white text, whichever reads better on a given fill.
ink_on <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  unname(ifelse(lum > 0.25, "#1A202C", "#FFFFFF"))
}

play_colors <- function(cfg) c(Pass = cfg$color_pass, Run = cfg$color_run)

pct <- function(x, acc = 1) scales::percent(x, accuracy = acc)

empty_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, colour = "grey40") +
    ggplot2::theme_void()
}

# Stacked run/pass bars for any tendency table with a label column.
plot_run_pass_bars <- function(tbl, label_col, title, cfg, subtitle = NULL, min_n = cfg$min_n_group) {
  if (nrow(tbl) == 0) return(empty_plot("No plays to chart"))
  d <- dplyr::filter(tbl, .data$total_plays >= min_n)
  if (nrow(d) == 0) return(empty_plot("Not enough plays to chart"))
  d <- d |>
    dplyr::mutate(label = paste0(as.character(.data[[label_col]]), "  (n=", .data$total_plays, ")"),
                  label = stats::reorder(.data$label, .data$total_plays)) |>
    dplyr::select("label", "pass_rate") |>
    dplyr::mutate(run_rate = 1 - .data$pass_rate) |>
    tidyr::pivot_longer(c("pass_rate", "run_rate"), names_to = "type", values_to = "rate") |>
    dplyr::mutate(type = ifelse(.data$type == "pass_rate", "Pass", "Run"),
                  type = factor(.data$type, levels = c("Run", "Pass")))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$rate, y = .data$label, fill = .data$type)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$rate >= 0.08, pct(.data$rate), ""),
                                    colour = .data$type),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 3.3, fontface = "bold", show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(Pass = ink_on(cfg$color_pass), Run = ink_on(cfg$color_run)), guide = "none") +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey30", linewidth = 0.3) +
    ggplot2::scale_x_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_fill_manual(values = play_colors(cfg), breaks = c("Pass", "Run")) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Share of plays", y = NULL, fill = NULL,
                  caption = scout_caption(cfg)) +
    theme_scout()
}

plot_personnel <- function(tbl, cfg) {
  plot_run_pass_bars(tbl, "personnel", "Run/pass split by personnel", cfg,
                     subtitle = paste0("Groupings with at least ", cfg$min_n_group, " plays. n = number of plays in that grouping"))
}

plot_alignment <- function(tbl, cfg) {
  plot_run_pass_bars(tbl, "qb_alignment", "Run/pass split by QB alignment", cfg, min_n = 1,
                     subtitle = "n = number of plays from that alignment; read small groups with caution")
}

plot_down_distance <- function(tbl, cfg) {
  if (nrow(tbl) == 0) return(empty_plot("No down-and-distance data"))
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$distance_bucket, y = forcats::fct_rev(.data$down_label),
                                    fill = .data$pass_rate)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(pct(.data$pass_rate), " pass\nn=", .data$total_plays),
                                    colour = cut(.data$pass_rate, c(-Inf, 0.25, 0.75, Inf), labels = c("run", "mid", "pass"))),
                       size = 3.3, lineheight = 0.9, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(run = ink_on(cfg$color_run), mid = "#1A202C",
                                            pass = ink_on(cfg$color_pass)), guide = "none") +
    ggplot2::scale_fill_gradient2(low = cfg$color_run, mid = "#F7F7F7", high = cfg$color_pass,
                                  midpoint = 0.5, limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(title = "Pass rate by down and distance",
                  subtitle = "Each box: share of plays that were passes, and n = number of plays in that down-and-distance situation",
                  x = "Yards to go", y = NULL,
                  fill = "Pass rate", caption = scout_caption(cfg)) +
    theme_scout() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.key.width = ggplot2::unit(1.2, "cm"))
}

plot_run_concepts <- function(tbl, cfg) {
  if (nrow(tbl) == 0) return(empty_plot("Not available for this data set"))
  d <- dplyr::filter(tbl, .data$carries >= cfg$min_n_concept, .data$run_concept != "Unclassified")
  if (nrow(d) == 0) return(empty_plot("Not enough classified runs to chart"))
  d <- dplyr::mutate(d,
                     label = paste0(.data$run_concept, " (", .data$carries, ")"),
                     label = stats::reorder(.data$label, .data$carries),
                     lo = .data$epa_per_carry - .data$epa_se,
                     hi = .data$epa_per_carry + .data$epa_se)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$epa_per_carry, y = .data$label)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lo, xmax = .data$hi), height = 0, colour = "grey60",
                            na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(size = .data$carries), colour = cfg$color_run) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f YPC | %s succ", .data$yards_per_carry,
                                                    pct(.data$success_rate))),
                       nudge_y = 0.42, size = 3, colour = "grey30") +
    ggplot2::scale_size_area(max_size = 6, guide = "none") +
    ggplot2::labs(title = paste0(cfg$run_concept_label, ": volume and efficiency"),
                  subtitle = "Ordered by carries (in brackets). Point = EPA per carry, bar = +/- 1 standard error",
                  x = "EPA per carry", y = NULL, caption = scout_caption(cfg)) +
    theme_scout()
}

plot_third_down <- function(tbl, cfg) {
  if (nrow(tbl) == 0) return(empty_plot("No third-down plays"))
  d <- tbl |>
    dplyr::select("distance_bucket", "plays", `Pass rate` = "pass_rate", `Conversion rate` = "conversion_rate") |>
    tidyr::pivot_longer(c("Pass rate", "Conversion rate"), names_to = "metric", values_to = "value")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$distance_bucket, y = .data$value, fill = .data$metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = pct(.data$value)),
                       position = ggplot2::position_dodge(width = 0.75), vjust = -0.4, size = 3.2) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1.08), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = c(`Pass rate` = cfg$color_pass, `Conversion rate` = "#718096")) +
    ggplot2::scale_x_discrete(labels = function(x) paste0(x, "\nn=", tbl$plays[match(x, tbl$distance_bucket)])) +
    ggplot2::labs(title = "Third down: how often they throw and how often they convert",
                  subtitle = "n = number of third-down plays at that distance",
                  x = NULL, y = NULL, fill = NULL, caption = scout_caption(cfg)) +
    theme_scout()
}

plot_third_down_targets <- function(tbl, cfg, top_n = 3) {
  if (nrow(tbl) == 0) return(empty_plot("No third-down targets"))
  d <- tbl |>
    dplyr::filter(.data$distance_bucket != "All 3rd Downs") |>
    dplyr::group_by(.data$distance_bucket) |>
    dplyr::slice_max(.data$targets, n = top_n, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(distance_bucket = factor(.data$distance_bucket, levels = cfg$distance_labels),
                  label = paste0(.data$target_player, " (", .data$target_position, ")"),
                  label = tidytext_reorder(.data$label, .data$targets, .data$distance_bucket))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$targets, y = .data$label)) +
    ggplot2::geom_col(fill = cfg$color_pass, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(.data$targets, " tgt, ", .data$first_downs, " 1D")),
                       hjust = -0.08, size = 3) +
    ggplot2::facet_wrap(~ distance_bucket, scales = "free_y", drop = TRUE) +
    ggplot2::scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.6))) +
    ggplot2::labs(title = "Third-down target leaders by distance", x = "Targets", y = NULL,
                  caption = scout_caption(cfg, "1D = first downs gained on those targets.")) +
    theme_scout()
}

# Reorder within facets without needing the tidytext package.
tidytext_reorder <- function(x, by, within) {
  key <- paste(x, within, sep = "___")
  stats::reorder(key, by)
}

plot_receivers <- function(tbl, cfg) {
  if (nrow(tbl) == 0) return(empty_plot("Not available for this data set"))
  d <- dplyr::filter(tbl, .data$targets >= cfg$min_targets)
  if (nrow(d) == 0) return(empty_plot("No receivers meet the target minimum"))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$targets, y = .data$epa_per_target)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = .data$receiving_yards, colour = .data$target_position), alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = .data$target_player), vjust = -1.1, size = 3.1) +
    ggplot2::scale_size_area(max_size = 10, name = "Rec. yards") +
    ggplot2::scale_colour_manual(values = c(WR = cfg$color_pass, TE = "#6B46C1", HB = cfg$color_run,
                                            FB = "#975A16", QB = "grey40", UNK = "grey60"),
                                 name = "Position") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::labs(title = "Receiving volume vs. efficiency",
                  subtitle = paste0("Players with at least ", cfg$min_targets, " targets"),
                  x = "Targets", y = "EPA per target", caption = scout_caption(cfg)) +
    theme_scout()
}

plot_receiver_style <- function(tbl, cfg) {
  if (nrow(tbl) == 0) return(empty_plot("Not available for this data set"))
  d <- dplyr::filter(tbl, .data$targets >= cfg$min_targets, !is.na(.data$adot))
  if (nrow(d) == 0) return(empty_plot("No depth-of-target data"))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$adot, y = .data$yac_per_rec)) +
    ggplot2::geom_point(ggplot2::aes(size = .data$targets), colour = cfg$color_pass, alpha = 0.8, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$target_player), vjust = -1.1, size = 3.1, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = stats::median(d$adot, na.rm = TRUE), linetype = "dashed", colour = "grey50") +
    ggplot2::geom_hline(yintercept = stats::median(d$yac_per_rec, na.rm = TRUE), linetype = "dashed", colour = "grey50") +
    ggplot2::scale_size_area(max_size = 9, name = "Targets") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
    ggplot2::labs(title = "How receivers produce: depth of target vs. yards after catch",
                  subtitle = "Right = targeted deeper downfield; up = more yards after the catch. Dashed = team medians",
                  x = "Average depth of target (yards)", y = "YAC per reception", caption = scout_caption(cfg)) +
    theme_scout()
}

plot_rushers <- function(tbl, cfg) {
  if (nrow(tbl) == 0) return(empty_plot("Not available for this data set"))
  d <- dplyr::filter(tbl, .data$carries >= cfg$min_carries)
  if (nrow(d) == 0) return(empty_plot("No rushers meet the carry minimum"))
  if (all(is.na(d$yaco_per_carry))) {
    # No yards-after-contact data (e.g. nflverse): plain yards per carry.
    d <- dplyr::mutate(d, label = stats::reorder(paste0(.data$ball_carrier, " (", .data$carries, " car)"),
                                                 .data$carries))
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data$yards_per_carry, y = .data$label)) +
      ggplot2::geom_col(width = 0.65, fill = cfg$color_run) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f YPC | %s succ", .data$yards_per_carry,
                                                      pct(.data$success_rate))),
                         hjust = -0.08, size = 3, colour = "grey30") +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.35))) +
      ggplot2::labs(title = "Rushers: yards per carry",
                    subtitle = "Yards after contact is not in this data source, so carries are not split",
                    x = "Yards per carry", y = NULL, caption = scout_caption(cfg)) +
      theme_scout())
  }
  d <- d |>
    dplyr::mutate(label = paste0(.data$ball_carrier, " (", .data$carries, " car)"),
                  label = stats::reorder(.data$label, .data$carries),
                  before = pmax(.data$yards_before_contact, 0), after = .data$yaco_per_carry) |>
    dplyr::select("label", "before", "after", "success_rate", "epa_per_carry") |>
    tidyr::pivot_longer(c("before", "after"), names_to = "part", values_to = "yds") |>
    dplyr::mutate(part = factor(ifelse(.data$part == "before", "Before contact", "After contact"),
                                levels = c("After contact", "Before contact")))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$yds, y = .data$label, fill = .data$part)) +
    ggplot2::geom_col(width = 0.65, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c(`Before contact` = cfg$color_run, `After contact` = cfg$color_pass),
                               breaks = c("Before contact", "After contact")) +
    ggplot2::labs(title = "Rushers: where the yards come from",
                  subtitle = "Yards per carry split into before and after first contact",
                  x = "Yards per carry", y = NULL, fill = NULL, caption = scout_caption(cfg)) +
    theme_scout()
}

plot_tells <- function(tbl, cfg, top_n = 12) {
  if (nrow(tbl) == 0) return(empty_plot("Not available for this data set"))
  d <- tbl |> dplyr::filter(.data$is_tell) |> dplyr::slice_head(n = top_n)
  if (nrow(d) == 0) return(empty_plot("No pre-snap cue reaches the tell threshold"))
  d <- dplyr::mutate(d, label = paste0(.data$cue, "  (n=", .data$plays, ")"),
                     label = stats::reorder(.data$label, .data$lean_lo))
  ggplot2::ggplot(d, ggplot2::aes(y = .data$label)) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$lean_lo, xend = .data$lean_rate, yend = .data$label,
                                       colour = .data$lean), linewidth = 2.2, alpha = 0.35) +
    ggplot2::geom_point(ggplot2::aes(x = .data$lean_rate, colour = .data$lean), size = 3.5) +
    ggplot2::geom_text(ggplot2::aes(x = .data$lean_rate, label = paste0(pct(.data$lean_rate), " ", .data$lean)),
                       hjust = -0.25, size = 3.1) +
    ggplot2::scale_colour_manual(values = play_colors(cfg)) +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0.5, 1.1), breaks = seq(0.5, 1, 0.1)) +
    ggplot2::labs(title = "Strongest pre-snap tells",
                  subtitle = "Dot = observed rate; faded bar runs down to the 95% lower bound; n = plays with that cue",
                  x = "Share of plays matching the lean", y = NULL, colour = NULL,
                  caption = scout_caption(cfg)) +
    theme_scout() + ggplot2::theme(legend.position = "none")
}
