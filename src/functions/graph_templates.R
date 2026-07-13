
# Set general layout for plots ----


stopifnot(require("tidyverse"),
          require("assertthat"),
          require("ggdist"),
          require("glue"),
          require("plotly"),
          require("ggtext"),
          require("scales"),
          require("soiltexture"),
          require("rempsyc"))



## Depth colours ----

col_m01 <- "#472D7B"
col_m13 <- "#2C728E"
col_m36 <- "#27AD81"
col_m61 <- "#AADC32"

scale_colour_depth <- function(name = NULL) {

  scale_colour_manual(
    name = name,
    values = c(
      M01 = col_m01,
      M13 = col_m13,
      M36 = col_m36,
      M61 = col_m61
    ),
    labels = c(
      M01 = "0-10 cm",
      M13 = "10-30 cm",
      M36 = "30-60 cm",
      M61 = "60-100 cm"
    )
  )

}


code_layer_to_factor <- function(code_layer) {

  layer_levels <- c("OFH", "0 - 10 cm", "10 - 30 cm",
                    "30 - 60 cm", "60 - 100 cm")

  layer_labels <- c(
    OFH = "OFH",
    M01 = "0 - 10 cm",
    M13 = "10 - 30 cm",
    M36 = "30 - 60 cm",
    M61 = "60 - 100 cm"
  )

  factor(layer_labels[code_layer],
         levels = layer_levels)
}




## Theme for single plots ----

theme_single <- function(base_size = 10,
                         aspect_ratio = 0.9,
                         lineheight = 1.3) {

  theme_bw(base_size = base_size) +
    theme(
      axis.text.x =
        element_markdown(
          size = base_size,
          margin = margin(t = 6)),
      axis.text.y =
        element_markdown(
          size = base_size,
          margin = margin(r = 4)),
      axis.title.x =
        element_markdown(
          size = base_size,
          lineheight = lineheight,
          hjust = 1,
          margin = margin(t = 5)
        ),
      axis.title.y =
        element_markdown(
          size = base_size,
          lineheight = lineheight,
          vjust = 1,
          hjust = 0,
          angle = 0,
          margin = margin(r = 8) #5
        ),
      axis.ticks =
        element_line(linewidth = 0.3),
      plot.title =
        element_markdown(
          size = base_size,
          lineheight = lineheight,
          margin = margin(b = 10)
        ),
      panel.grid.minor =
        element_blank(),
      legend.key.height =
        unit(0.6, "cm"),
      legend.text =
        element_markdown(size = base_size,
                         lineheight = lineheight),
      legend.justification =
        "top",
      plot.margin =
        margin(
          t = 0.5,
          r = 0.5,
          b = 0.5,
          l = 0.5,
          unit = "cm"
        ),
      aspect.ratio =
        aspect_ratio
    )

}


## Theme for faceted plots per partner as a function of depth ----

theme_facet <- function(base_size = 10,
                        aspect_ratio = 0.7,
                        lineheight = 1.2,
                        val_min = 0,
                        val_max = NA,
                        nrow_legend_keys = 1) {

  val_max_expansion <- if (is.na(val_max)) 0.05 else 0
  legend_key_height <- if (nrow_legend_keys == 1) 0.6 else 1.1

  list(

    coord_flip(),

    scale_y_continuous(
      limits = c(val_min, val_max),
      expand = expansion(mult = c(0, val_max_expansion)),
      labels = function(x) ifelse(x == 0, "", x)
    ),

    scale_x_reverse(
      breaks = c(0, 10, 30, 60, 100),
      labels = c(0, "", 30, 60, 100),
      expand = c(0, 0)
    ),

    facet_wrap(
      ~ plot_code_simple,
      ncol = 4,
      labeller = labeller(
        plot_code_simple = function(x) {
          x %>%
            str_replace_all("__", "__\n") %>%
            str_wrap(width = 18)
        }
      )
    ),

    theme_bw(),

    theme(
      axis.text.x =
        element_markdown(
          size = base_size,
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 4)
        ),
      axis.text.y =
        element_markdown(
          size = base_size,
          margin = margin(r = 4)),
      axis.title.x =
        element_blank(),
      axis.title.y =
        element_markdown(
          size = base_size,
          lineheight = lineheight,
          vjust = 1,
          hjust = 0,
          angle = 0
        ),
      axis.ticks =
        element_line(linewidth = 0.3),
      strip.text =
        element_text(
          hjust = 0,
          size = .8 * base_size
        ),
      plot.title =
        element_markdown(
          size = base_size,
          lineheight = lineheight,
          margin = margin(b = 10)
        ),
      panel.grid.minor.y =
        element_blank(),
      panel.grid.minor.x =
        element_line(linewidth = 0.5),
      legend.key.height =
        unit(legend_key_height, "cm"), # 0.6 (1 row) or 1.1 (2 rows)
      legend.text =
        element_text(
          lineheight = lineheight,
          size = base_size
        ),
      # legend.justification =
      #   "top",
      plot.margin =
        margin(
          t = 0.5,
          r = 0.5,
          b = 0.5,
          l = 0.5,
          unit = "cm"
        ),
      aspect.ratio =
        aspect_ratio
    )
  )
}




## Histogram (density distribution) ----



# Shared internals

stat_interval_color_scale <- function() {
    scale_color_viridis_d(
      option    = "viridis",
      direction = -1,
      begin     = 0.1,
      end       = 0.8,
      labels    = c("0.05 - 0.95",
                    "0.10 - 0.90",
                    "0.25 - 0.75",
                    "0.40 - 0.60"),
      name      = "Quantile"
    )
}




theme_distr <- function(base_size = 10,
                        aspect_ratio = 0.7,
                        col_panel = "white",
                        col_background = NULL,
                        col_front = "black") {
  theme(
    panel.background = element_rect(fill = col_panel),
    plot.background = element_rect(fill = col_background,
                                   colour = col_background),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = col_background %||% "#D8E0E0",
                                      linewidth = 0.5),
    panel.grid.minor.x = element_line(color = col_background %||% "#D8E0E0",
                                      linewidth = 0.5),
    plot.title = element_markdown(size = base_size, colour = col_front,
                                  lineheight = 1.4,
                                  margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = base_size, colour = col_front,
                                     margin = margin(t = 0, b = 10)),
    plot.caption = element_markdown(size = base_size, hjust = 0,
                                    lineheight = 1.4, colour = col_front,
                                    margin = margin(t = 4)),
    plot.caption.position = "plot",
    axis.title = element_markdown(size = base_size, colour = col_front,
                                  hjust = 1, lineheight = 1.4),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_markdown(size = base_size, colour = col_front,
                                   margin = margin(t = 6, b = 3)),
    axis.text.y = element_markdown(size = base_size, colour = col_front,
                                   vjust = 0.5,
                                   margin = margin(r = 5)),
    legend.title = element_text(size = base_size,
                                colour = col_front),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.3, "cm"),
    legend.background = element_rect(fill = col_background),
    # legend.justification = c("left", "top"),
    legend.text = element_markdown(size = base_size, colour = col_front,
                                   lineheight = 1.3),
    legend.spacing.y = unit(0.2, "cm"),
    legend.margin = margin(t = 0, b = 0, l = 5, r = 5),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
    aspect.ratio = aspect_ratio
  )
}





# plot_dist_simple()
#' One-variable distribution: stat_halfeye + stat_interval
#'
#' @param data A numeric vector or a data frame with a column named `data`.
#' @param unit Unit string appended to mean/median in the caption.
#' @param plot_title Optional title (also used as x-axis label).

plot_distr_simple <- function(data,
                             unit,
                             round_level = 0,
                             plot_title = NA,
                             nudge = 0.02) {

  # Coerce to data frame
  if (is.vector(data)) {
    dataset <- data.frame(data = data)
  } else if (is.data.frame(data) && "data" %in% names(data)) {
    dataset <- data
  } else {
    stop(paste0("`data` must be a numeric vector or a data frame ",
                "with a 'data' column."))
  }

  dataset <- dataset[!is.na(dataset$data), , drop = FALSE]

  mean_val <- mean(dataset$data)
  median_val <- median(dataset$data)
  n_val <- nrow(dataset)

  ggplot(dataset, aes(x = data)) +
    stat_halfeye(
      fill_type = "segments",
      fill = "#acbfbd",
      point_interval = NULL,
      point_color = "white",
      interval_color = NA, # hide halfeye's own interval line
      position = position_nudge(y = nudge) # push halfeye slightly up
    ) +
    stat_interval(.width = c(0.20, 0.50, 0.80, 0.90),
                  position = position_nudge(y = -1 * nudge)) + # push down
    annotate(geom = "point",
             x = mean_val,
             y = -0.015,
             color = "white",
             size  = 1.5) +
    coord_cartesian(
      ylim = c(0, 1),
      xlim = c(0, quantile(dataset$data, 0.995))
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    stat_interval_color_scale() +
    guides(
      color = guide_legend(
        override.aes = list(linewidth = 4, linetype = 1)
      )
    ) +
    labs(
      title = paste0(plot_title, "<br>",
                     "*(n = ", n_val, ")*"),
      caption = paste0(
        "Mean: ", round(mean_val, round_level), " ", unit,
        "<br>Median: ", round(median_val, round_level), " ", unit),
      x = NULL,
      y = NULL
    ) +
    theme_distr() +
    theme(axis.text.y = element_blank())
}


# plot_dist_by_group()
#' Distribution by group: stat_halfeye + stat_interval, groups on y-axis
#'
#' @param data Data frame with columns `response` and `group`.
#' @param response_name Label for the response variable (supports markdown).
#' @param group_name Label for the grouping variable (supports markdown).
#' @param x_label x-axis label.
#' @param x_min,x_max x-axis limits.
#' @param src_survey_name Optional subtitle string.
#' @param caption Optional caption string.
#' @param aspect_ratio Passed to `.theme_distr()`.
#' @param col_panel,col_background,col_front  Colour overrides.

plot_distr_by_group <- function(data,
                                response_name,
                                group_name,
                                plot_title = NA,
                                x_min = NULL,
                                x_max = NULL,
                                caption = NULL,
                                aspect_ratio = 1,
                                col_panel = "white",
                                col_background = NULL,
                                col_front = "black",
                                nudge = 0.05,
                                with_interval = TRUE) {

  dataset <- data %>%
    mutate(group = .data[[group_name]]) %>%
    mutate(response = .data[[response_name]]) %>%
    filter(!is.na(response)) %>%
    filter(!is.na(group) &
             group != "")

  data_count <- dataset %>%
    group_by(group) %>%
    reframe(count = n())

  # dataset <- dataset %>%
  #   left_join(data_count, by = "group") %>%
  #   mutate(group = factor(
  #     paste0("**", group, "** (", count, ")"),
  #     levels = paste0("**", levels(group), "** (",
  #                     data_count$count[match(levels(group),
  #                                            data_count$group)], ")")
  #   ))

  dataset <- dataset %>%
    left_join(data_count, by = "group") %>%
    mutate(group = factor(
      paste0(group, " *(", count, ")*"),
      levels = paste0(levels(group), " *(",
                      data_count$count[match(levels(group),
                                             data_count$group)], ")*")
    ))

  # Bootstrapped mean + 95 % CI
  suppressWarnings({
    data_summary <- rcompanion_groupwiseMean(
      group = "group",
      var = "response",
      data = dataset,
      conf = 0.95,
      digit = 5,
      R = 2000,
      traditional = TRUE,
      bca = FALSE,
      na.rm = TRUE
    ) %>%
      mutate(
        Trad.lower = pmax(Trad.lower, 0),
        type = "Mean + 95 % CI<br>(bootstrapped)"
      )
  })

  x_min <- x_min %||% 0
  x_max <- x_max %||% quantile(dataset$response, 0.995, na.rm = TRUE)

  nudge <- if (with_interval) nudge else (-2.5) * nudge

  ggplot() +
    stat_halfeye(
      data = dataset,
      aes(x = response, y = group),
      fill_type = "segments",
      fill = "#acbfbd",
      point_interval = NULL, # mean dot added separately below
      normalize = "groups",
      scale = 0.55,
      position = position_nudge(y = nudge) # push halfeye slightly up
    ) +
    (if (with_interval) list(
    stat_interval(
      data = dataset,
      aes(x = response, y = group),
      .width = c(0.20, 0.50, 0.80, 0.90),
      position = position_nudge(y = -1 * nudge) # push interval slightly down
    ),
    geom_errorbar(
      data = data_summary,
      aes(y = group, xmin = Trad.lower, xmax = Trad.upper, linetype = type),
      color = "white",
      linewidth = 0.5,
      width = 0,
      position = position_nudge(y = -1 * nudge)
    ),
    geom_point(
      data = data_summary,
      aes(y = group, x = Mean, shape = type),
      color = "white",
      size = 1.2,
      position = position_nudge(y = -1 * nudge)
    ))) +
    coord_cartesian(xlim = c(x_min, x_max), expand = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_discrete(limits = rev) +
    (if (with_interval) list(
    stat_interval_color_scale(),
    scale_linetype_manual(
      values = c("Mean + 95 % CI<br>(bootstrapped)" = "solid"),
      name = ""),
      scale_shape_manual(
        values = c("Mean + 95 % CI<br>(bootstrapped)" = 16),
        name = ""),
      guides(
        color = guide_legend(
          override.aes = list(linewidth = 4, linetype = 1)
        )
      ))) +
    labs(
      title = plot_title,
      x = NULL,
      y = NULL,
      caption = caption
    ) +
    theme_bw() +
    theme_distr(
      aspect_ratio = aspect_ratio,
      col_panel = col_panel,
      col_background = col_background,
      col_front = col_front
    )
}


