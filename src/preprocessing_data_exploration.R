
# Data exploration and visualisation for data pre-processing

# Note: for plotly graphs (which are interactive), you can hover over
# outlier points to see which plots they represent, zoom in, etc




# Data exploration undisturbed samples ----


# Make dataframe in which you can easily check the data

df_layers_undist_check <- df_layers %>%
  filter(wp == "WP2") %>%
  select(wp, institute_sampling, plot_code_simple, site_type, code_layer,
         layer_number, depth_top, depth_bottom, depth_bottom_bedrock,
         vol_ring, count_rings, bulk_den,
         mass_before, mass_after, mass_after_fine_earth,
         P1_coaf_2mm, P1_coaf_50mm, coarse_fragment_vol_ring,
         mass_cf, vol_cf, particle_density, particle_density_source,
         mass_roots, remarks_pretrt,
         contains("dry_to_moist"), mass_after_fine_earth_corr,
         vol_total, vol_fine_earth, contains("bulk_density"),
         # contains("depth_bedrock")
  ) %>%
  left_join(
    df_sampling_points %>%
      group_by(plot_code_simple) %>%
      reframe(
        notes_bulk_den = {
          vals <- na.omit(notes_bulk_den)
          if (length(vals)) paste(vals, collapse = ". ") else NA_character_
        }
      ),
    by = "plot_code_simple") %>%
  relocate(notes_bulk_den, .before = "remarks_pretrt")




df_layers_undist_check2 <- df_layers_undist_check %>%
  filter(grepl("^M", code_layer)) %>%
  # Derived metrics to evaluate consistency
  mutate(
    # mass_lab_type = case_when(
    #   !is.na(mass_after_total_net) ~ "total",
    #   !is.na(mass_after_net) ~ "fine earth",
    #   TRUE ~ NA),
    # mass_lab = coalesce(mass_after_total_net,
    #                     mass_after_net),
    dry_to_moist_central = mass_after / mass_before,
    dry_to_moist = mass_after / bulk_den,
    before_to_moist_field = mass_before / bulk_den,
    # bulk_den_estimate = (1E-3 * mass_lab) / (1E-6 * vol_total),
    bd_field =
      (1E-3 * (bulk_den - coalesce(mass_cf, 0) - coalesce(mass_roots, 0))) /
      (1E-6 * ((vol_total) - coalesce(vol_cf, 0) -
         (coalesce(mass_roots, 0) * 1E3 / 1400))),
    # stones = case_when(
    #   (!is.na(P1_coaf_2mm) & P1_coaf_2mm > 1) |
    #     (!is.na(P1_coaf_50mm) & P1_coaf_50mm > 5) ~ "Ja!",
    #   TRUE ~ "Nee")
    )








# Graphs

# Check mass_after versus mass_before and mass_dist per partner

for (partner_i in unique(df_layers_undist_check2$institute_sampling)) {

  p <- df_layers_undist_check2 %>%
    filter(institute_sampling == partner_i) %>%
    pivot_longer(
      cols = c(bulk_den, mass_before, mass_after),
      names_to = "stage",
      values_to = "mass"
    ) %>%
    filter(!is.na(mass) & mass > 0) %>%
    mutate(stage = factor(stage,
                          levels = c("bulk_den",
                                     "mass_before", "mass_after"))) %>%
    ggplot(
      aes(x = stage,
          y = mass,
          group = code_layer,
          colour = code_layer)
    ) +
    coord_cartesian(ylim = c(0, NA),
                    expand =  c(bottom = FALSE)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ plot_code_simple,
               ncol = 4,
               labeller = labeller(
                 plot_code_simple = function(x) {
                   x %>%
                     str_replace_all("__", "__\n") %>%   # break after "__"
                     str_wrap(width = 18)
                 }
               )
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      strip.text = element_text(hjust = 0, size = 8)
    )

  n_plots <- n_distinct(
    df_ff_check$plot_code_simple[which(
      df_ff_check$institute_sampling == partner_i)])
  ncol <- 4
  nrow <- ceiling(n_plots / ncol)
  height_per_row <- 1

  ggsave(filename = paste0("undisturbed_mass_per_stage_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/undisturbed/",
                       "stages_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 2)      # dynamic height

} # End of for loop along partners





## mass_after versus field mass ----

p <- ggplot(df_layers_undist_check2,
            aes(y = mass_after,
                x = bulk_den,
                colour = code_layer,
                text = paste0(plot_code_simple, "_", code_layer))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")


ggsave(filename = paste0("mass_after_versus_bulk_den",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)


dry_to_moist_field_plaus <- c(0.4, 1)

p <- ggplot(
  df_layers_undist_check2,
  aes(
    x = 1,
    y = dry_to_moist,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = dry_to_moist_field_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")


ggsave(filename = paste0("mass_after_to_bulk_den",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



## mass_before versus field mass ----

p <- ggplot(df_layers_undist_check2,
            aes(y = mass_before,
                x = bulk_den,
                colour = code_layer,
                text = paste0(plot_code_simple, "_", code_layer))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")


ggsave(filename = paste0("mass_before_versus_bulk_den",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)

before_to_moist_field_plaus <- c(0.4, 1.02)

p <- ggplot(
  df_layers_undist_check2,
  aes(
    x = 1,
    y = before_to_moist_field,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = before_to_moist_field_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_before_to_bulk_den",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



## mass_after versus mass_before ----

p <- ggplot(df_layers_undist_check2,
            aes(y = mass_after,
                x = mass_before,
                colour = code_layer,
                text = paste0(plot_code_simple, "_", code_layer))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_versus_mass_before",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



dry_to_moist_lab_plaus <- c(0.5, 1.01)

p <- ggplot(
  df_layers_undist_check2,
  aes(
    x = 1,
    y = dry_to_moist_central,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = dry_to_moist_lab_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_to_mass_before",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



## Bulk density ----

# These are the 5-95 % quantiles of the bulk densities of mineral matrices
# in ICP Forests (systematic Level I) (kg m-3)
# Note: this is for fine earth!

bd_mineral_plaus <- c(720, 1535)

p <- ggplot(
  df_layers_undist_check2,
  aes(
    x = 1,
    y = bulk_density,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = bd_mineral_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("bulk_density",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)


hist(df_layers_undist_check2$bulk_density)

## mass_after_fine_earth versus vol_fine_earth ----

p <- ggplot(df_layers_undist_check2,
            aes(y = mass_after_fine_earth_corr,
                x = vol_fine_earth,
                colour = code_layer,
                # text = paste0(plot_code_simple, "_", code_layer)
            )) + # tooltip
  geom_point(size = 0.8,
             alpha = 0.5) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE
  ) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label), after_stat(rr.label),
                    after_stat(p.value.label), sep = "~~~"),
    ),
    formula = y ~ x - 1,
    parse = TRUE
  )

p

ggsave(filename = paste0("mass_fe_versus_vol_fe",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



## Field bulk density estimate ----

p <- ggplot(
  df_layers_undist_check2,
  aes(
    x = 1,
    y = bd_field,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = bd_mineral_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("bulk_density_moist_field",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)





## Coarse fragments (2-50 mm) lab versus field ----

p <- df_layers_undist_check %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  mutate(
    coaf_2_50mm = (P1_coaf_2mm - P1_coaf_50mm)) %>%
  filter(!is.na(coarse_fragment_vol_ring) &
           !is.na(coaf_2_50mm)) %>%
  ggplot(aes(y = coaf_2_50mm,
                x = coarse_fragment_vol_ring,
                colour = code_layer,
                # text = paste0(plot_code_simple, "_", code_layer)
            )) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE
  ) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label), after_stat(rr.label),
                    after_stat(p.value.label), sep = "~~~"),
    ),
    formula = y ~ x - 1,
    parse = TRUE
  )

p

ggsave(filename = paste0("coaf_2_50mm_lab_versus_field",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)








## Moisture content per partner ----

for (partner_i in unique(df_layers_undist_check$institute_sampling)) {

  p <- df_layers_undist_check %>%
    mutate(depth = ifelse(layer_number == 1, depth_top, depth_top + .1)) %>%
    bind_rows(
      df_layers_undist_check %>%
        mutate(depth = depth_bottom_bedrock)
    ) %>%
    arrange(institute_sampling, plot_code_simple, depth) %>%
    filter(institute_sampling == partner_i) %>%
    filter(!is.na(dry_to_moist)) %>%
    filter(!is.na(depth_bottom_bedrock)) %>%
    ggplot() +
    geom_line(
      aes(y = dry_to_moist,
          x = depth),
      linewidth = .8
    ) +
      # geom_rect(aes(ymin = 0,
      #               ymax = dry_to_moist,
      #               xmin = (1) * depth_bottom_bedrock,
      #               xmax = (1) * depth_top),
      #           color = NA,
      #           linewidth = 1) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0, 0)) +
    scale_x_reverse(breaks = c(0, 10, 30, 60, 100),
                    expand = c(0, 0)) +
    # coord_cartesian(ylim = c(0, NA),
    #                 expand =  c(bottom = FALSE)) +
    facet_wrap(~ plot_code_simple,
               ncol = 4,
               labeller = labeller(
                 plot_code_simple = function(x) {
                   x %>%
                     str_replace_all("__", "__\n") %>%   # break after "__"
                     str_wrap(width = 18)
                 }
               )
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      strip.text = element_text(hjust = 0, size = 8)
    )

  n_plots <- n_distinct(
    df_ff_check$plot_code_simple[which(
      df_ff_check$institute_sampling == partner_i)])
  ncol <- 4
  nrow <- ceiling(n_plots / ncol)
  height_per_row <- 1

  ggsave(filename = paste0("dry_to_moist_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/undisturbed/",
                       "moisture_contents_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 2)      # dynamic height

} # End of for loop along partners










## Bulk density per partner ----

for (partner_i in unique(df_layers_undist_check$institute_sampling)) {

  p <- df_layers_undist_check %>%
    mutate(depth = ifelse(layer_number == 1, depth_top, depth_top + .1)) %>%
    bind_rows(
      df_layers_undist_check %>%
        mutate(depth = depth_bottom_bedrock)
    ) %>%
    arrange(institute_sampling, plot_code_simple, depth) %>%
    filter(institute_sampling == partner_i) %>%
    filter(!is.na(depth_bottom_bedrock)) %>%
    mutate(bulk_density = coalesce(bulk_density,
                                   bulk_density_dist),
           # bulk_den_estimate = (1E-3 * mass_lab) / (1E-6 * vol_total),
           bd_field =
             (1E-3 * (bulk_den - coalesce(mass_cf, 0) -
                        coalesce(mass_roots, 0))) /
             (1E-6 * ((vol_total) - coalesce(vol_cf, 0) -
                        (coalesce(mass_roots, 0) * 1E3 / 1400))),) %>%
    filter(!is.na(bulk_density)) %>%
    ggplot() +
    geom_hline(yintercept = bd_mineral_plaus, colour = "blue", alpha = 0.3) +
    geom_line(
      aes(y = bulk_density,
          x = depth,
          color = "Bulk density"),
      linewidth = .8
    ) +
    geom_line(
      aes(y = bd_field,
          x = depth,
          color = "Bulk density (field-moist)"),
      linewidth = .8
    ) +
    scale_color_manual(
      values = c(
        "Bulk density" = "black",
        "Bulk density (field-moist)" = "orange"
      ),
      name = NULL
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.05))
                       # expand = c(0, NA)
                       ) +
    scale_x_reverse(breaks = c(0, 10, 30, 60, 100),
                    expand = c(0, 0)) +
    # coord_cartesian(ylim = c(0, NA),
    #                 expand =  c(bottom = FALSE)) +
    facet_wrap(~ plot_code_simple,
               ncol = 4,
               labeller = labeller(
                 plot_code_simple = function(x) {
                   x %>%
                     str_replace_all("__", "__\n") %>%   # break after "__"
                     str_wrap(width = 18)
                 }
               )
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      strip.text = element_text(hjust = 0, size = 8)
    )

  n_plots <- n_distinct(
    df_ff_check$plot_code_simple[which(
      df_ff_check$institute_sampling == partner_i)])
  ncol <- 4
  nrow <- ceiling(n_plots / ncol)
  height_per_row <- 1

  ggsave(filename = paste0("bulk_density_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/undisturbed/",
                       "bulk_density_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 2)      # dynamic height

} # End of for loop along partners












## Coarse fragments per partner ----

for (partner_i in unique(df_layers_undist_check$institute_sampling)) {

  p <- df_layers_undist_check %>%
    mutate(depth = ifelse(layer_number == 1, depth_top, depth_top + .1)) %>%
    bind_rows(
      df_layers_undist_check %>%
        mutate(depth = depth_bottom_bedrock)
    ) %>%
    arrange(institute_sampling, plot_code_simple, depth) %>%
    filter(institute_sampling == partner_i) %>%
    filter(!is.na(depth_bottom_bedrock)) %>%
    filter(!is.na(coarse_fragment_vol_ring) |
             !is.na(P1_coaf_2mm) |
             !is.na(P1_coaf_50mm)) %>%
    mutate(
      coaf_2_50mm = (P1_coaf_2mm - P1_coaf_50mm)) %>%
    ggplot(
      aes(x = depth)
    ) +
    geom_line(
      aes(y = coarse_fragment_vol_ring, color = "CF (2–50 mm) lab"),
      linewidth = .8
    ) +
    geom_line(
      aes(y = coaf_2_50mm, color = "CF (2–50 mm) field"),
      linewidth = .8
    ) +
    geom_line(
      aes(y = P1_coaf_2mm, color = "CF (total) field"),
      linewidth = .8
    ) +
    scale_color_manual(
      values = c(
        "CF (2–50 mm) lab" = "black",
        "CF (2–50 mm) field" = "orange",
        "CF (total) field" = "red"
      ),
      name = NULL
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 100),
                       expand = c(0, 0)
    ) +
    scale_x_reverse(breaks = c(0, 10, 30, 60, 100),
                    expand = c(0, 0)) +
    facet_wrap(~ plot_code_simple,
               ncol = 4,
               labeller = labeller(
                 plot_code_simple = function(x) {
                   x %>%
                     str_replace_all("__", "__\n") %>%   # break after "__"
                     str_wrap(width = 18)
                 }
               )
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      strip.text = element_text(hjust = 0, size = 8)
    )

  n_plots <- n_distinct(
    df_ff_check$plot_code_simple[which(
      df_ff_check$institute_sampling == partner_i)])
  ncol <- 4
  nrow <- ceiling(n_plots / ncol)
  height_per_row <- 1

  ggsave(filename = paste0("coarse_fragments_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/undisturbed/",
                       "coarse_fragments_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 2)      # dynamic height

} # End of for loop along partners







# Add flags

undist_add %>%
  mutate(
    d2mf_flag = case_when(
      dry_to_moist_field < 0.4 |
        dry_to_moist_field > 1 ~ "implaus"),
    b2mf_flag = case_when(
      before_to_moist_field < 0.4 |
        before_to_moist_field > 1.02 ~ "implaus"),
    d2ml_flag = case_when(
      (mass_lab_type == "total" & dry_to_moist_lab < 0.8) |
        dry_to_moist_lab < 0.6 |
        dry_to_moist_lab > 1.01 ~ "implaus"),
    bd_flag = case_when(
      # Less strict flag
      bulk_den_estimate < 480 |
        bulk_den_estimate > 1700 ~ "implaus"),
    bd_field_flag = case_when(
      bd_field < 700 |
        bd_field > 2000 ~ "implaus")) %>%
  View







## Data exploration coarse fragments undist ----

# Before validation

p <- undist_harm %>%
  mutate(
    mass_cf =
      ifelse(!is.na(mass_cf_wet_siev) | !is.na(mass_cf_siev),
             pmax(coalesce(mass_cf_wet_siev, 0) + coalesce(mass_cf_siev, 0),
                  0, na.rm = TRUE),
             NA_real_),
    mass_cf_for_vol =
      ifelse(
        (!is.na(mass_cf_wet_siev) &
           mass_cf_wet_siev > 0 &
           !is.na(mass_cf_siev) &
           mass_cf_siev > 0),
        mass_cf_wet_siev,
        mass_cf),
    vol_cf = case_when(
      !is.na(vol_cf_before) & is.na(vol_cf_after) & vol_cf_before == 0 ~ 0,
      TRUE ~ vol_cf_after - vol_cf_before)
  ) %>%
  filter(!is.na(mass_cf_for_vol) & !is.na(vol_cf)) %>%
  ggplot(
    aes(x = vol_cf, y = mass_cf_for_vol)) +
  geom_point() +
  geom_smooth(method = "lm",
              # Through origin (- 1; equivalent to y ~ 0 + x)
              formula = y ~ x - 1, se = FALSE) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    formula = y ~ x - 1,
    parse = TRUE,
    label.x = "left",
    label.y = "top"
  )

p

ggsave(filename = paste0("mass_versus_vol_cf_before_validation",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)

p <- undist_harm %>%
  mutate(
    mass_cf =
      ifelse(!is.na(mass_cf_wet_siev) | !is.na(mass_cf_siev),
             pmax(coalesce(mass_cf_wet_siev, 0) + coalesce(mass_cf_siev, 0),
                  0, na.rm = TRUE),
             NA_real_),
    mass_cf_for_vol =
      ifelse(
        (!is.na(mass_cf_wet_siev) &
           mass_cf_wet_siev > 0 &
           !is.na(mass_cf_siev) &
           mass_cf_siev > 0),
        mass_cf_wet_siev,
        mass_cf),
    vol_cf = case_when(
      !is.na(vol_cf_before) & is.na(vol_cf_after) & vol_cf_before == 0 ~ 0,
      TRUE ~ vol_cf_after - vol_cf_before)
  ) %>%
  filter(!is.na(mass_cf_for_vol) & !is.na(vol_cf)) %>%
  ggplot(
    aes(x = vol_cf,
        y = mass_cf_for_vol,
        text = paste0(plot_code_simple, "_", code_layer))) +
  geom_point()

ggplotly(p, tooltip = "text")




# part_dens_plaus <- c(1748, 3047) # 90 % quantile (after filtering vol >= 5)

# Plausible range of observed particle densities
# 80 % quantile after filtering for vol_cf > 10
part_dens_plaus <- c(1976.706, 2724.126)

p <- undist_harm %>%
  mutate(
    mass_cf =
      ifelse(!is.na(mass_cf_wet_siev) | !is.na(mass_cf_siev),
             pmax(coalesce(mass_cf_wet_siev, 0) + coalesce(mass_cf_siev, 0),
                  0, na.rm = TRUE),
             NA_real_),
    mass_cf_for_vol =
      ifelse(
        (!is.na(mass_cf_wet_siev) &
           mass_cf_wet_siev > 0 &
           !is.na(mass_cf_siev) &
           mass_cf_siev > 0),
        mass_cf_wet_siev,
        mass_cf),
    vol_cf = case_when(
      !is.na(vol_cf_before) & is.na(vol_cf_after) & vol_cf_before == 0 ~ 0,
      TRUE ~ vol_cf_after - vol_cf_before),
    particle_density = ifelse(
      !is.na(mass_cf_for_vol) & !is.na(vol_cf) &
        mass_cf_for_vol > 0 & vol_cf > 0,
      1E3 * mass_cf_for_vol / vol_cf,
      NA_real_)
  ) %>%
  filter(vol_cf >= 5) %>%
  ggplot(
  aes(
    x = 1,
    y = particle_density,
    text = gsub("_Bulk_Density$", "", sample_id_simple,
                ignore.case = TRUE)
  )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = part_dens_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("particle_density_after_validation",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)







# After validation

p <- ggplot(
  df_layers_undist_check %>%
    filter(!is.na(mass_cf) & !is.na(vol_cf)),
  aes(x = vol_cf, y = mass_cf)
) +
  geom_point() +
  geom_smooth(method = "lm",
              # Through origin (- 1; equivalent to y ~ 0 + x)
              formula = y ~ x - 1, se = FALSE) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    formula = y ~ x - 1,
    parse = TRUE,
    label.x = "left",
    label.y = "top"
  )

p

ggsave(filename = paste0("mass_versus_vol_cf_after_validation",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)


mod <- lm(mass_cf ~ vol_cf - 1, data = undist_harm)

eq <- paste0(
  "y = ", # round(coef(mod)[1], 2), " + ",
  round(coef(mod)[1], 3), "x\n",
  "R² = ", round(summary(mod)$r.squared, 3)
)
cat(eq)



p <- ggplot(df_layers_undist_check %>%
              filter(!is.na(mass_cf) & !is.na(vol_cf)),
            aes(y = mass_cf,
                x = vol_cf,
                text = paste0(plot_code_simple, "_", code_layer))
            ) + # tooltip
  geom_point()

ggplotly(p, tooltip = "text")





p <- ggplot(
  df_layers_undist_check %>%
    filter(!is.na(particle_density)) %>%
    filter(vol_cf >= 10),
  aes(
    x = 1,
    y = particle_density,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = part_dens_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("particle_density_after_validation",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)
















# Data exploration forest floor masses ----


# Make dataframe in which you can easily check the data

df_ff_check <- df_layers %>%
  select(wp, institute_sampling, sample_id_simple,
         plot_code_simple, code_layer, thickness, tamass, dist,
         mass_dry_partner, mass_before, mass_after, tamass_compiled,
         dry_to_moist, dry_to_moist_local, dry_to_moist_central,
         areal_mass, areal_mass_min, areal_mass_max,
         bulk_density_dist,
         surface_frame, count_ff_frames) %>%
  filter(code_layer %in% c("OL", "OFH")) %>%
  filter(wp == "WP2") %>%
  left_join(
    df_sampling_points %>%
      group_by(plot_code_simple) %>%
      reframe(
        remarks_fofloor_sample = {
          vals <- na.omit(remarks_fofloor_sample)
          if (length(vals)) paste(vals, collapse = ". ") else NA_character_
        }
      ),
    by = "plot_code_simple") %>%
  left_join(
    dist_harm %>%
      group_by(plot_code_simple, code_layer) %>%
      reframe(
        remarks_pretrt = {
          vals <- na.omit(remarks_pretrt)
          if (length(vals)) paste(vals, collapse = ". ") else NA_character_
        }
      ) %>%
      select(plot_code_simple, code_layer, remarks_pretrt),
    by = join_by("plot_code_simple", "code_layer")) %>%
  left_join(
    samples %>%
      filter(grepl("_OFH_carbon$", sample_id_simple)) %>%
      select(sample_id_simple, mass_sample_gross),
    by = "sample_id_simple") %>%
  relocate(mass_sample_gross, .after = "mass_dry_partner")

View(df_ff_check)



df_ff_check2 <- df_ff_check %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form,
                     actual_weather, past_weather, soil_condition),
            by = "plot_code_simple") %>%
  mutate(
    mass_dry_partner = case_when(
      institute_sampling %in% c("INBO", "BFNP") ~ mass_before,
      TRUE ~ mass_dry_partner)) %>%
  mutate(
    dry_partner_to_dist = mass_dry_partner / dist,
    before_to_dry_partner = mass_before / mass_dry_partner,
    after_to_before = mass_after / mass_before,
    after_to_dist = mass_after / dist,
    after_to_dry_partner = mass_after / mass_dry_partner
  ) %>%
  mutate(
    actual_weather2 = case_when(
      actual_weather %in% c("rain", "variable, showers") ~ actual_weather,
      TRUE ~ "dry"),
    past_weather2 = case_when(
      past_weather ==
        "heavier rain for some days or rainstorm in the last 24 hours" ~ "wet",
      past_weather %in% c(
        "rainy without heavy rain in the last 24 hours",
        "no rain in the last 24 hours") ~ "rather wet",
      TRUE ~ "dry")
  )

## Thickness to mass per sampling point ----

p <- ggplot(df_sampling_points %>%
              pivot_longer(
                cols = c(ol_thickness, ofh_thickness, ol_tamass, ofh_tamass),
                names_to = c("code_layer", ".value"),
                names_pattern = "(ol|ofh)_(.*)"
              ) %>%
              mutate(
                code_layer = toupper(code_layer)
              ) %>%
              filter(!is.na(tamass) & !is.na(thickness)),
            aes(y = tamass,
                x = thickness,
                colour = code_layer,
                # text = paste0(plot_code_simple, "_", code_layer)
                )) + # tooltip
  geom_point(size = 0.8) +
  # Regression lines per code_layer
  geom_smooth(
    aes(group = code_layer),
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE
  ) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label), after_stat(rr.label),
                    after_stat(p.value.label), sep = "~~~"),
      group = code_layer
    ),
    formula = y ~ x - 1,
    parse = TRUE
  ) +
  coord_cartesian(xlim = c(0, 40),
                  expand = FALSE) +
  ggtitle(paste0("Field-moist mass versus thickness of forest floor layers\n",
                 "across sampling points and WP2 plots")) +
  theme(
    title = element_text(size = 8))
p

ggsave(filename = paste0("ff_mass_versus_thickness_per_point",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


p <- ggplot(df_sampling_points %>%
              pivot_longer(
                cols = c(ol_thickness, ofh_thickness, ol_tamass, ofh_tamass),
                names_to = c("code_layer", ".value"),
                names_pattern = "(ol|ofh)_(.*)"
              ) %>%
              mutate(
                code_layer = toupper(code_layer)
              ) %>%
              filter(!is.na(tamass) & !is.na(thickness)),
            aes(y = tamass,
                x = thickness,
                colour = code_layer,
                text = paste0(plot_code_simple, "_", code_layer)
            )) + # tooltip
  geom_point(size = 0.8,
             alpha = 0.5) +
  # Regression lines per code_layer
  geom_smooth(
    aes(group = code_layer),
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE
  ) +
  coord_cartesian(xlim = c(0, 40),
                  expand = FALSE) +
  ggtitle(paste0("Field-moist mass versus thickness of forest floor layers\n",
                 "across sampling points and WP2 plots")) +
  theme(
    title = element_text(size = 8))

ggplotly(p, tooltip = "text")


# OL:
#    2.5%       5%      10%      90%      95%    97.5%
#  0.0100   0.0100   6.1080 229.8050 361.9940 523.0127

# OFH:
#    2.5%       5%      10%      90%      95%    97.5%
#  0.0300   0.0500  57.3220 567.5630 719.9480 966.6265

p <- df_sampling_points %>%
  pivot_longer(
    cols = c(ol_thickness, ofh_thickness, ol_tamass, ofh_tamass),
    names_to = c("code_layer", ".value"),
    names_pattern = "(ol|ofh)_(.*)"
  ) %>%
  mutate(
    code_layer = toupper(code_layer)
  ) %>%
  filter(!is.na(tamass) & !is.na(thickness)) %>%
  left_join(
    df_layers %>%
      distinct(plot_code_simple, .keep_all = TRUE) %>%
      select(plot_code_simple, surface_frame),
    by = "plot_code_simple") %>%
  filter(thickness > 0) %>%
  mutate(
    density_moist = # kg m-3 (moist)
      round(
        tamass * 1E-3 / # kg field-moist material over sampling frame
          (surface_frame * thickness * 1E-2), 2) # m3 volume
  ) %>%
  ggplot(
    aes(
      x = 1,
      y = density_moist,
      colour = code_layer,
      text = paste0(plot_code_simple, "_", code_layer, "_", p_id)
    )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5)

ggplotly(p, tooltip = "text")


# Outliers for FVA-BW, especially Siedigkopf

ggsave(filename = paste0("ff_moist_density_per_point",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)






## Comparison dist versus tamass ----

p <- ggplot(df_ff_check2 %>%
              filter(!is.na(dist)),
            aes(y = dist,
                x = tamass,
                colour = code_layer,
                text = gsub("_carbon|_flammability$", "", sample_id_simple,
                            ignore.case = TRUE))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5) +
  coord_cartesian(xlim = c(0, 2500),
                  ylim = c(0, 2500),
                  expand = FALSE)

ggplotly(p, tooltip = "text")


ggsave(filename = paste0("dist_versus_tamass",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)



## Comparison dry mass partner versus field-moist subsample ----

# Is mass_dry_partner not higher than dist?

df_ff_check %>%
  filter(mass_dry_partner > 1.0 * dist) %>%
  View

p <- ggplot(df_ff_check %>%
              filter(!is.na(mass_dry_partner)),
            aes(y = mass_dry_partner,
                x = dist,
                colour = code_layer,
                text = gsub("_carbon|_flammability$", "", sample_id_simple,
                            ignore.case = TRUE))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_dry_partner_versus_dist",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


dry_partner_to_dist_plaus <- c(0.2, 1)

p <- ggplot(
  df_ff_check2 %>%
    filter(!is.na(mass_dry_partner)),
  aes(
    x = 1,
    y = dry_partner_to_dist,
    colour = code_layer,
    text = gsub("_carbon|_flammability$", "", sample_id_simple,
                ignore.case = TRUE)
  )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = dry_partner_to_dist_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

# Some issues for BFNP OL (> 1): ask if masses excluded bag
# Some samples < 0.15 for peat samples (VUK__34, WLS__25) so fine

ggsave(filename = paste0("dry_to_moist_local",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)



# OL versus OFH

p <- ggplot(
  df_ff_check2 %>%
    pivot_wider(
      id_cols = c(plot_code_simple, institute_sampling),
      names_from = code_layer,
      values_from = c(dry_partner_to_dist)
    ) %>%
    filter(!is.na(OL) & !is.na(OFH)),
  aes(
    x = OFH,
    y = OL,
    text = plot_code_simple
  )) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  expand = FALSE)

ggplotly(p, tooltip = "text")


p <- ggplot(
  df_ff_check2 %>%
    pivot_wider(
      id_cols = c(plot_code_simple),
      names_from = code_layer,
      values_from = c(dry_partner_to_dist)
    ) %>%
    # Should be <1 in theory
    filter(OL <= 1 & OFH <= 1) %>%
    filter(!is.na(OL) & !is.na(OFH)),
  aes(
    x = OFH,
    y = OL
  )) +
  geom_point(size = 0.8) +
  # Regression lines per code_layer
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE
  ) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0, 1),
                  expand = FALSE) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label), after_stat(rr.label),
                    after_stat(p.value.label), sep = "~~~"),
    ),
    formula = y ~ x - 1,
    parse = TRUE
  )

p

ggsave(filename = paste0("dry_to_moist_local_ol_versus_ofh",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)

# Check mass_dry_partner to dist per partner and per OL/OFH

p <- df_ff_check2 %>%
  filter(!is.na(mass_dry_partner)) %>%
  mutate(
    y_axis = paste(institute_sampling, code_layer, sep = " - "),
    y_axis = factor(
      y_axis,
      levels = rev(unique(paste(institute_sampling, code_layer, sep = " - ")))
    )
  ) %>%
  ggplot(
    aes(
      x = dry_partner_to_dist,
      y = y_axis,
      text = gsub("_carbon$|_flammability$", "", sample_id_simple,
                  ignore.case = TRUE)
    )
  ) +
  geom_jitter(height = 0.2, width = 0)

ggplotly(p, tooltip = "text")








## Is mass_before similar to mass_dry_partner? ----

df_ff_check %>%
  filter(mass_before > 1.05 * mass_dry_partner |
           mass_before < 0.95 * mass_dry_partner) %>%
  View

p <- ggplot(df_ff_check %>%
              filter(institute_sampling != "INBO") %>%
              filter(code_layer == "OFH"),
            aes(y = mass_before,
                x = mass_dry_partner,
                text = gsub("_carbon$", "", sample_id_simple,
                            ignore.case = TRUE))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_before_versus_mass_dry_partner",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


before_to_dry_partner_plaus <- c(0.9, 1.1)

p <- ggplot(
  df_ff_check2 %>%
    filter(institute_sampling != "INBO") %>%
    filter(code_layer == "OFH"),
  aes(
    x = 1,
    y = before_to_dry_partner,
    text = gsub("_carbon$", "", sample_id_simple, ignore.case = TRUE)
  )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = before_to_dry_partner_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_before_to_mass_dry_partner",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)




## Comparison mass_after versus mass_before (central lab) ----

# Is mass_after not higher than mass_before
# (if slightly higher, replace by mass_before)

p <- ggplot(df_ff_check %>%
              filter(!is.na(mass_before) & !is.na(mass_after)) %>%
              filter(institute_sampling != "INBO") %>%
              mutate(remarks = (!is.na(remarks_pretrt))),
            aes(y = mass_after,
                x = mass_before,
                colour = remarks,
                text = gsub("_carbon$", "", sample_id_simple,
                            ignore.case = TRUE))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_versus_mass_before",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


after_to_before_plaus <- c(0.8, 1.01)

p <- ggplot(
  df_ff_check2 %>%
    filter(!is.na(mass_before) & !is.na(mass_after)) %>%
    filter(institute_sampling != "INBO") %>%
    mutate(remarks = (!is.na(remarks_pretrt))),
  aes(
    x = 1,
    y = after_to_before,
    colour = remarks,
    text = gsub("_carbon$", "", sample_id_simple, ignore.case = TRUE)
  )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = after_to_before_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_to_mass_before",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


# Check mass_after to mass_before per partner and per OL/OFH

p <- df_ff_check2 %>%
  filter(!is.na(mass_before) & !is.na(mass_after)) %>%
  filter(institute_sampling != "INBO") %>%
  mutate(remarks = (!is.na(remarks_pretrt))) %>%
  ggplot(
  aes(
    x = after_to_before,
    y = interaction(institute_sampling, code_layer, sep = " - "),
    colour = remarks,
    text = gsub("_carbon$", "", sample_id_simple,
                ignore.case = TRUE)
  )
) +
  geom_jitter(height = 0.2, width = 0)

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_to_mass_before_per_partner",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)





# Check mass_after versus mass_before and mass_dry_partner and dist per partner

for (partner_i in unique(df_ff_check$institute_sampling)) {

  p <- df_ff_check %>%
    filter(institute_sampling == partner_i) %>%
    pivot_longer(
      cols = c(dist, mass_dry_partner, mass_before, mass_after),
      names_to = "stage",
      values_to = "mass"
    ) %>%
    filter(!is.na(mass)) %>%
    mutate(stage = factor(stage,
                          levels = c("dist", "mass_dry_partner",
                                     "mass_before", "mass_after"))) %>%
    ggplot(
      aes(x = stage,
          y = mass,
          group = code_layer,
          colour = code_layer)
    ) +
    coord_cartesian(ylim = c(0, NA),
                    expand =  c(bottom = FALSE)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ plot_code_simple,
               ncol = 4,
               labeller = labeller(
                 plot_code_simple = function(x) {
                   x %>%
                     str_replace_all("__", "__\n") %>%   # break after "__"
                     str_wrap(width = 18)
                 }
               )
               ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      strip.text = element_text(hjust = 0, size = 8)
    )

  n_plots <- n_distinct(
    df_ff_check$plot_code_simple[which(
      df_ff_check$institute_sampling == partner_i)])
  ncol <- 4
  nrow <- ceiling(n_plots / ncol)
  height_per_row <- 1



  ggsave(filename = paste0("forest_floor_mass_per_stage_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/forest_floor/",
                       "stages_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 2)      # dynamic height

} # End of for loop along partners









## Comparison mass_after versus dist ----

p <- ggplot(df_ff_check %>%
              filter(code_layer == "OFH"),
            aes(y = mass_after,
                x = dist,
                text = gsub("_carbon$", "", sample_id_simple,
                            ignore.case = TRUE))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_versus_dist",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


after_to_dist_plaus <- c(0.2, 1)

p <- ggplot(
  df_ff_check2 %>%
    filter(code_layer == "OFH"),
  aes(
    x = 1,
    y = after_to_dist,
    text = gsub("_carbon$", "", sample_id_simple, ignore.case = TRUE)
  )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = after_to_dist_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_to_dist",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)






## Comparison mass_after versus mass_dry_partner ----

p <- ggplot(df_ff_check %>%
              filter(code_layer == "OFH"),
            aes(y = mass_after,
                x = mass_dry_partner,
                text = gsub("_carbon$", "", sample_id_simple,
                            ignore.case = TRUE))) + # tooltip
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  geom_point(size = 0.8,
             alpha = 0.5)

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_versus_mass_dry_partner",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


after_to_dry_partner_plaus <- c(0.8, 1)

p <- ggplot(
  df_ff_check2 %>%
    filter(code_layer == "OFH"),
  aes(
    x = 1,
    y = after_to_dry_partner,
    text = gsub("_carbon$", "", sample_id_simple, ignore.case = TRUE)
  )) +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = after_to_dry_partner_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_to_dist",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)


df_ff_check2 %>%
  filter(code_layer == "OFH") %>%
  mutate(
    b2dp_flag = (!is.na(mass_dry_partner) &
                   (before_to_dry_partner < before_to_dry_partner_plaus[1] |
                      before_to_dry_partner > before_to_dry_partner_plaus[2])),
    a2b_flag = (!institute_sampling %in% c("BFNP", "UNIUD", "INBO") &
                  (after_to_before < after_to_before_plaus[1] |
                     after_to_before > after_to_before_plaus[2]))
  ) %>%
  filter(wp == "WP2" & code_layer == "OFH" & (b2dp_flag) & (!a2b_flag)) %>% View









## Areal mass ----
#  t ha-1

p <- ggplot(df_ff_check %>%
              filter(!is.na(areal_mass)),
            aes(y = areal_mass,
                x = thickness,
                colour = code_layer,
            )) + # tooltip
  geom_point(size = 0.8) +
  # Regression lines per code_layer
  geom_smooth(
    aes(group = code_layer),
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE
  ) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label), after_stat(rr.label),
                    after_stat(p.value.label), sep = "~~~"),
      group = code_layer
    ),
    formula = y ~ x - 1,
    parse = TRUE
  )
p

ggsave(filename = paste0("areal_mass_versus_thickness",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)

# 11 t ha-1 ~ 1.1 kg m-2 (per cm)

p <- ggplot(df_ff_check %>%
              filter(!is.na(areal_mass)),
            aes(y = areal_mass,
                x = thickness,
                colour = code_layer,
                text = paste0(plot_code_simple, "_", code_layer)
            )) + # tooltip
  geom_point(size = 0.8,
             alpha = 0.5) +
  # Regression lines per code_layer
  geom_smooth(
    aes(group = code_layer),
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE
  )

ggplotly(p, tooltip = "text")



# areal_mass as a function of humus_form

p <- df_ff_check2 %>%
  filter(!is.na(areal_mass)) %>%
  mutate(humus_form = factor(humus_form,
                             levels = c("Mull", "Moder", "Mor",
                                        "Tangel", "Amphi",
                                        "Semi-terrestrial"))) %>%
  ggplot(
    aes(
      x = areal_mass,
      y = humus_form,
      text = paste0(plot_code_simple, "_", code_layer)
    )
  ) +
  geom_jitter(width = 0, height = 0.2, alpha = 0.3)

ggplotly(p, tooltip = "text")


ggsave(filename = paste0("areal_mass_versus_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/forest_floor/"),
       dpi = 500)





## dry_to_moist ----
# dry_to_moist as a function of weather / moisture conditions

for (col_i in c("actual_weather", "past_weather", "soil_condition")) {

  p <- df_ff_check2 %>%
    mutate(
      actual_weather = as.factor(actual_weather2),
      past_weather = as.factor(past_weather2),
      soil_condition = as.factor(soil_condition)
    ) %>%
    ggplot(
      aes(
        x = .data[[col_i]],
        y = dry_to_moist
      )
    ) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.6)

  ggsave(filename = paste0("dry_to_moist_versus_", col_i,
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/forest_floor/"),
         dpi = 500)

} # End of for loop







# Data exploration disturbed below-ground masses ----

# Check mass_after versus mass_before and dist per partner
# (to make sure not samples got switched)

for (partner_i in
     unique(df_layers$institute_sampling[which(df_layers$wp == "WP2")])) {

  p <- df_layers %>%
    filter(wp == "WP2") %>%
    filter(institute_sampling == partner_i) %>%
    filter(grepl("^M", code_layer)) %>%
    pivot_longer(
      cols = c(dist, mass_before_dist, mass_after_dist),
      names_to = "stage",
      values_to = "mass"
    ) %>%
    filter(!is.na(mass)) %>%
    filter(mass > 0) %>%
    mutate(stage = factor(stage,
                          levels = c("dist", "mass_before_dist",
                                     "mass_after_dist"))) %>%
    ggplot(
      aes(x = stage,
          y = mass,
          group = code_layer,
          colour = code_layer)
    ) +
    coord_cartesian(ylim = c(0, NA),
                    expand =  c(bottom = FALSE)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ plot_code_simple,
               ncol = 4,
               labeller = labeller(
                 plot_code_simple = function(x) {
                   x %>%
                     str_replace_all("__", "__\n") %>%   # break after "__"
                     str_wrap(width = 18)
                 }
               )
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      strip.text = element_text(hjust = 0, size = 8)
    )

  n_plots <- n_distinct(
    df_ff_check$plot_code_simple[which(
      df_ff_check$institute_sampling == partner_i)])
  ncol <- 4
  nrow <- ceiling(n_plots / ncol)
  height_per_row <- 1



  ggsave(filename = paste0("disturbed_bg_mass_per_stage_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/disturbed_below_ground/",
                       "stages_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 2)      # dynamic height

} # End of for loop along partners








