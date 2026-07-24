
# Data exploration and visualisation for data pre-processing

# Note: for plotly graphs (which are interactive), you can hover over
# outlier points to see which plots they represent, zoom in, etc

# General variables and functions ----

# Dataframe with value for upper and lower depth

df_layers_depth <- df_layers %>%
  filter(wp == "WP2") %>%
  mutate(
    depth = ifelse(
      layer_number == 1,
      depth_top,
      depth_top + .1)) %>%
  bind_rows(
    df_layers %>%
      filter(wp == "WP2") %>%
      mutate(
        depth = depth_bottom_bedrock)
  ) %>%
  arrange(institute_sampling, plot_code_simple, depth)

# Vector with all institutes (partners)

vec_inst <- df_layers %>%
  filter(wp == "WP2") %>%
  distinct(institute_sampling) %>%
  pull(institute_sampling)

# These are the 5-95 % quantiles of the bulk densities of mineral matrices
# in ICP Forests (systematic Level I) (kg m-3)
# Note: this is for fine earth!

bd_mineral_plaus <- c(720, 1535)

# Pars for facet plots as a function of depth

ncol <- 4
height_per_row <- 1






# Data exploration undisturbed samples ----


# Make dataframe in which you can easily check the data

df_layers_undist_check <- df_layers %>%
  filter(wp == "WP2") %>%
  mutate(
    P1_coaf_2mm =
      if (!"P1_coaf_2mm" %in% names(.)) coarse_fragment_vol_p1
    else P1_coaf_2mm,
    P1_coaf_50mm =
      if (!"P1_coaf_50mm" %in% names(.)) coarse_fragment_vol_p1_50mm
    else P1_coaf_50mm,
  ) %>%
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
  mutate(
    # Derived metrics to evaluate consistency
    # Bulk density based on fresh masses
    bd_field =
      (1E-3 * (bulk_den - coalesce(mass_cf, 0) - coalesce(mass_roots, 0))) /
      (1E-6 * ((vol_total) - coalesce(vol_cf, 0) -
                 (coalesce(mass_roots, 0) * 1E3 / 1400)))
  ) %>%
  relocate(notes_bulk_den, .before = "remarks_pretrt")




# Graphs

# Check mass_after versus mass_before and mass_dist per partner

for (partner_i in vec_inst) {

  p <- df_layers_undist_check %>%
    filter(grepl("^M", code_layer)) %>%
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
    df_layers_undist_check$plot_code_simple[which(
      df_layers_undist_check$institute_sampling == partner_i)])
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

p <- ggplot(df_layers_undist_check %>%
              filter(grepl("^M", code_layer)),
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


dry_to_field_moist_plaus <- c(0.3, 1)

p <- ggplot(
  df_layers_undist_check %>%
    filter(grepl("^M", code_layer)),
  aes(
    x = 1,
    y = dry_to_moist,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = dry_to_field_moist_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")


ggsave(filename = paste0("mass_after_to_bulk_den",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



## mass_before versus field mass ----

p <- ggplot(df_layers_undist_check %>%
              filter(grepl("^M", code_layer)),
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

dry_to_moist_local_plaus <- c(0.3, 1.015)

p <- ggplot(
  df_layers_undist_check %>%
    filter(grepl("^M", code_layer)),
  aes(
    x = 1,
    y = dry_to_moist_local,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = dry_to_moist_local_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_before_to_bulk_den",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



## mass_after versus mass_before ----

p <- ggplot(df_layers_undist_check %>%
              filter(grepl("^M", code_layer)),
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



after_to_before_plaus <- c(0.5, 1.01)

p <- ggplot(
  df_layers_undist_check %>%
    filter(grepl("^M", code_layer)),
  aes(
    x = 1,
    y = dry_to_moist_central,
    colour = code_layer,
    text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_hline(yintercept = after_to_before_plaus,
             colour = "red")

ggplotly(p, tooltip = "text")

ggsave(filename = paste0("mass_after_to_mass_before",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)



## Bulk density ----

p <- ggplot(
  df_layers_undist_check %>%
    filter(grepl("^M", code_layer)),
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

df_layers_undist_check %>%
  filter(grepl("^M", code_layer)) %>%
  hist(main = "Bulk density · WILDCARD WP2 (mineral layers)")



## mass_after_fine_earth versus vol_fine_earth ----

p <- ggplot(df_layers_undist_check %>%
              filter(grepl("^M", code_layer)),
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
  df_layers_undist_check %>%
    filter(grepl("^M", code_layer)),
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

ggsave(filename = paste0("bulk_density_field_moist",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)





## Coarse fragments (2-50 mm) lab versus field ----

p <- df_layers_undist_check %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  mutate(
    coaf_2_50mm = (P1_coaf_2mm - P1_coaf_50mm)) %>%
  filter(!is.na(coarse_fragment_vol_ring) & !is.na(coaf_2_50mm)) %>%
  ggplot(aes(
    y = coaf_2_50mm,
    x = coarse_fragment_vol_ring,
    colour = code_layer,
    # text = paste0(plot_code_simple, "_", code_layer)
  )) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linewidth = 0.5) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE,
    linewidth = 0.5
  ) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label),
                    after_stat(rr.label),
                    after_stat(p.value.label),
                    sep = "~~~")
      ),
    formula = y ~ x - 1,
    parse = TRUE,
    lineheight = 1.2
  ) +
  labs(
    title = paste0("Comparison of data sources for vol% coarse fragments ",
                   "(2 - 50 mm)<br>",
                   "(WILDCARD WP2)"),
    x = "Vol% based on **undisturbed samples**",
    y = paste0("Vol% based on<br>",
               "**visual field<br>estimation**")
  ) +
  coord_cartesian(ylim = c(0, 70),
                  xlim = c(0, 70),
                  expand =  c(bottom = FALSE,
                              left = FALSE)) +
  scale_colour_depth() +
  theme_single()

p

ggsave(filename = paste0("coaf_2_50mm_lab_versus_field",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81)






## Coarse fragments lab versus field ----

p <- df_layers %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  filter(!is.na(P1_coaf_2mm) & !is.na(coarse_fragment_vol)) %>%
  ggplot(aes(
    y = P1_coaf_2mm,
    x = coarse_fragment_vol,
    colour = code_layer,
  )) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linewidth = 0.5) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE,
    linewidth = 0.5
  ) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label),
                    after_stat(rr.label),
                    after_stat(p.value.label),
                    sep = "~~~")
    ),
    formula = y ~ x - 1,
    parse = TRUE,
    lineheight = 1.2
  ) +
  labs(
    title = paste0("Comparison of data sources for vol% coarse fragments<br>",
                   "(WILDCARD WP2)"),
    x = paste0("Vol% based on **undisturbed samples** (2-50 mm)<br>+ ",
               "**visual field estimation** (>50 mm)"),
    y = paste0("Vol% based on<br>",
               "**visual field<br>estimation**")
  ) +
  coord_cartesian(ylim = c(0, 100),
                  xlim = c(0, 100),
                  expand =  FALSE) +
  scale_colour_depth() +
  theme_single()

p

ggsave(filename = paste0("coaf_lab_versus_field",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81)










## Moisture content per partner ----

for (partner_i in vec_inst) {

  p <- df_layers_depth %>%
    filter(institute_sampling == partner_i) %>%
    filter(!is.na(dry_to_moist)) %>%
    filter(!is.na(depth_bottom_bedrock)) %>%
    ggplot() +
    geom_line(
      aes(y = dry_to_moist,
          x = depth),
      linewidth = 0.5,
      colour = "red"
    ) +
      # geom_rect(aes(ymin = 0,
      #               ymax = dry_to_moist,
      #               xmin = (1) * depth_bottom_bedrock,
      #               xmax = (1) * depth_top),
      #           color = NA,
      #           linewidth = 1) +
    labs(
      title = paste0("**Moisture content** (dry to fresh ratio)"),
      x = "Depth<br>(cm)"
    ) +
    theme_facet(val_max = 1)

  n_plots <- n_distinct(
    df_layers_depth$plot_code_simple[
      which(df_layers_depth$institute_sampling == partner_i)])
  nrow <- ceiling(n_plots / ncol)

  ggsave(filename = paste0("dry_to_moist_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/undisturbed/",
                       "moisture_contents_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 3)      # dynamic height

} # End of for loop along partners









## Bulk density per partner ----

for (partner_i in vec_inst) {

  df_i <- df_layers_depth %>%
    filter(institute_sampling == partner_i) %>%
    filter(!is.na(depth_bottom_bedrock)) %>%
    mutate(bulk_density = coalesce(bulk_density,
                                   bulk_density_dist),
           # # bulk_den_estimate = (1E-3 * mass_lab) / (1E-6 * vol_total),
           # bd_field =
           #   (1E-3 * (bulk_den - coalesce(mass_cf, 0) -
           #              coalesce(mass_roots, 0))) /
           #   (1E-6 * ((vol_total) - coalesce(vol_cf, 0) -
           #              (coalesce(mass_roots, 0) * 1E3 / 1400))),
           ) %>%
    filter(!is.na(bulk_density))

  p <- df_i %>%
    ggplot() +
    geom_segment(
      data = tibble(
        y    = bd_mineral_plaus,
        yend = bd_mineral_plaus
      ),
      aes(x = 0, xend = Inf,
          y = y, yend = yend,
          colour = "ICP Forests\n(5–95% range)"),
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = bulk_density,
          x = depth,
          colour = "Calculated\n(WILDCARD)"),
      linewidth = 0.5
    ) +
    scale_colour_manual(
      name   = NULL,
      values = c(
        "ICP Forests\n(5–95% range)" = "orange",
        "Calculated\n(WILDCARD)" = "blue"
      )
    ) +
    labs(
      title = paste0("**Bulk density** (kg m<sup>-3</sup>)"),
      x = "Depth<br>(cm)"
    ) +
    theme_facet(val_max = 2000,
                nrow_legend_keys = 2)

  n_plots <- n_distinct(
    df_layers_depth$plot_code_simple[
      which(df_layers_depth$institute_sampling == partner_i)])
  nrow <- ceiling(n_plots / ncol)

  ggsave(filename = paste0("bulk_density_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/undisturbed/",
                       "bulk_density_per_partner/"),
         dpi = 500,
         width = 6.81,
         height = nrow * height_per_row + 3) # dynamic height

  # The "weird" cases with lower field-moist bulk density than real
  # bulk density are not a problem. The final bulk_density is correct,
  # the original field-moist bulk_density was most likely a typo:
  # "INBO__2__Wijnendalebos__NA" M13
  # "BFNP__IP_10__IP_Hohensteingehaenge__NA" M36
  # "BGD-NP__1__Berchtesgaden National Park__F048" M01

} # End of for loop along partners












## Coarse fragments per partner ----

for (partner_i in vec_inst) {

  p <- df_layers_depth %>%
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
      aes(y = coarse_fragment_vol_ring, color = "2–50 mm - lab"),
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = coaf_2_50mm, color = "2–50 mm - field"),
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = P1_coaf_2mm, color = ">2 mm - field"),
      linewidth = 0.5
    ) +
    scale_color_manual(
      values = c(
        "2–50 mm - lab" = "blue",
        "2–50 mm - field" = "orange",
        ">2 mm - field" = "red"
      ),
      name = NULL
    ) +
    labs(
      title = paste0("**Coarse fragments** (vol%)"),
      x = "Depth<br>(cm)"
    ) +
    theme_facet(val_max = 100)


  n_plots <- n_distinct(
    df_layers_depth$plot_code_simple[
      which(df_layers_depth$institute_sampling == partner_i)])
  nrow <- ceiling(n_plots / ncol)

  ggsave(filename = paste0("coarse_fragments_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/undisturbed/",
                       "coarse_fragments_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 3)      # dynamic height

} # End of for loop along partners










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
      is.na(vol_cf_after) ~ NA_real_,
      TRUE ~ vol_cf_after - vol_cf_before)
  ) %>%
  filter(!is.na(mass_cf_for_vol) & !is.na(vol_cf)) %>%
  ggplot(
    aes(x = vol_cf, y = mass_cf_for_vol)) +
  geom_point() +
  geom_smooth(method = "lm",
              # Through origin (- 1; equivalent to y ~ 0 + x)
              formula = y ~ x - 1, se = FALSE,
              colour = "red") +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    formula = y ~ x - 1,
    parse = TRUE,
    label.x = "left",
    label.y = "top"
  ) +
  labs(
    x = "Volume coarse fragments (cm<sup>3</sup>)",
    y = "Mass<br>coarse fragments<br>(g)"
  ) +
  theme_single()

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
      is.na(vol_cf_after) ~ NA_real_,
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

ggsave(filename = paste0("particle_density_before_validation",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500)







# After validation

p <- ggplot(
  undist_harm %>%
    filter(!is.na(mass_cf_for_vol) & !is.na(vol_cf)),
  aes(x = vol_cf, y = mass_cf_for_vol)
) +
  geom_point() +
  geom_smooth(method = "lm",
              # Through origin (- 1; equivalent to y ~ 0 + x)
              formula = y ~ x - 1, se = FALSE,
              colour = "red") +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    formula = y ~ x - 1,
    parse = TRUE,
    label.x = "left",
    label.y = "top"
  ) +
  labs(
    x = "Volume coarse fragments (cm<sup>3</sup>)",
    y = "Mass<br>coarse fragments<br>(g)"
  ) +
  theme_single()

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



p <- ggplot(undist_harm %>%
              filter(!is.na(mass_cf_for_vol) & !is.na(vol_cf)),
            aes(y = mass_cf_for_vol,
                x = vol_cf,
                text = paste0(plot_code_simple, "_", code_layer))
            ) + # tooltip
  geom_point()

ggplotly(p, tooltip = "text")





p <- ggplot(
  df_layers %>%
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
         plot_code_simple, code_layer, thickness,
         tamass, tamass_min, tamass_max, dist,
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
  filter(mass_dry_partner > 1.0 * dist)

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
           mass_before < 0.95 * mass_dry_partner)

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

for (partner_i in vec_inst) {

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

for (partner_i in vec_inst) {

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
         path = paste0("./output/data_quality/disturbed/",
                       "stages_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 2)      # dynamic height

} # End of for loop along partners













# Data exploration coordinates ----



stopifnot(require(leaflet),
          require(htmlwidgets))

source("./src/functions/as_sf.R")

# Create dataframe

his_sf <- his %>%
  rename_with(~ paste0(.x, "_dec"), c(latitude, longitude)) %>%
  # Filter out "extra" plots without HIS coordinates
  filter(!is.na(latitude_dec) & !is.na(longitude_dec)) %>%
  as_sf %>%
  rename_with(~ paste0(.x, "_his"), c(geometry,
                                      latitude_dec, longitude_dec)) %>%
  select(plot_code_simple, ends_with("_his"))

df_plot_sf <- df_plot %>%
  filter(wp == "WP2") %>%
  as_sf %>%
  rename_with(~ paste0(.x, "_app"), c(geometry,
                                      latitude_dec, longitude_dec)) %>%
  select(plot_code_simple, ends_with("_app"))


# Join the two dataframes on plot_code_simple
df_coord <- his %>%
  select(plot_code_simple) %>%
  inner_join(
    his_sf,
    by = "plot_code_simple"
  ) %>%
  inner_join(
    df_plot_sf,
    by = "plot_code_simple"
  ) %>%
  rowwise() %>%
  mutate(
    distance_m =
      round(as.numeric(st_distance(geometry_his, geometry_app)))
  ) %>%
  ungroup()

write.table(df_coord %>%
              select(-contains("geometry")),
            file = "./output/data_quality/coordinates/distances.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

# Build lines correctly in WGS84
lines_sf <- df_coord %>%
  st_drop_geometry() %>%
  select(-geometry_his, -geometry_app) %>%  # drop the geometry list-columns
  rowwise() %>%
  mutate(
    geometry = list(
      st_linestring(matrix(
        c(longitude_dec_his, latitude_dec_his,
          longitude_dec_app, latitude_dec_app),
        ncol = 2, byrow = TRUE
      ))
    )
  ) %>%
  ungroup() %>%
  mutate(geometry = st_sfc(geometry, crs = 4326)) %>%
  st_as_sf()


# Build interactive map

map <- leaflet(df_coord %>%
                 st_drop_geometry() %>%
                 select(-geometry_his, -geometry_app)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  addPolylines(
    data    = lines_sf,
    color   = "black",
    weight  = 1.2,
    opacity = 0.6,
    label   = ~plot_code_simple
  ) %>%

  addCircleMarkers(
    lng         = ~longitude_dec_his,
    lat         = ~latitude_dec_his,
    color       = "blue",
    fillColor   = "blue",
    fillOpacity = 0.85,
    radius      = 3,
    weight      = 1,
    label       = ~paste0(plot_code_simple, " (HIS metadata table)")
  ) %>%

  addCircleMarkers(
    lng         = ~longitude_dec_app,
    lat         = ~latitude_dec_app,
    color       = "orange",
    fillColor   = "orange",
    fillOpacity = 0.85,
    radius      = 3,
    weight      = 1,
    label       = ~paste0(plot_code_simple, " (Survey123 app)")
  )

map



# Save to a self-contained HTML file (all assets embedded - easy to share)

saveWidget(
  widget   = map,
  file     = paste0("./output/data_quality/coordinates/",
                    "coordinates_app_vs_metadata_map.html"),
  selfcontained = TRUE   # single file, no extra folders needed
)







# Final data ----

## Bulk density ----

df_layers_filled %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  filter(!(code_layer == "OL" & depth_top < 0)) %>%
  group_by(bulk_density_source) %>%
  reframe(count = n())

# Distribution of belowground and forest floor

p1 <- df_layers_filled %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  filter(!(grepl("^O", code_layer) & depth_top < 0)) %>%
  filter(!grepl("Pedotransfer", bulk_density_source)) %>%
  pull(bulk_density) %>%
  plot_distr_simple(unit = "kg m<sup>-3</sup>",
                    plot_title = paste0("**Bulk density** (kg m<sup>-3</sup>)",
                                        " - belowground"))

p2 <- df_layers_filled %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  filter(grepl("^O", code_layer) & depth_top < 0) %>%
  filter(!grepl("Pedotransfer", bulk_density_source)) %>%
  pull(bulk_density) %>%
  plot_distr_simple(unit = "kg m<sup>-3</sup>",
                    plot_title = paste0("**Bulk density** (kg m<sup>-3</sup>)",
                                        " - forest floor"))

p <- (p1 | p2) + plot_layout(guides = "collect")

ggsave(filename = paste0("bulk_density",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)

# Distribution per layer

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!(code_layer == "OL")) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  mutate(layer = code_layer_to_factor(code_layer)) %>%
  select(layer, bulk_density) %>%
  plot_distr_by_group(response_name = "bulk_density",
                      group_name = "layer",
                      plot_title = "**Bulk density** (kg m<sup>-3</sup>)",
                      nudge = 0.07)

ggsave(filename = paste0("bulk_density_per_layer",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)



## TOC ----

# Distribution per layer

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!(code_layer == "OL" & depth_top < 0)) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  mutate(layer = code_layer_to_factor(code_layer)) %>%
  select(layer, c_organic_total) %>%
  plot_distr_by_group(response_name = "c_organic_total",
                      group_name = "layer",
                      plot_title = "**Total organic C** (g C kg<sup>-1</sup>)")

ggsave(filename = paste0("c_organic_total_per_layer",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)



## Coarse fragments ----

# Distribution per layer

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!(depth_top < 0)) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  mutate(layer = code_layer_to_factor(code_layer)) %>%
  select(layer, coarse_fragment_vol) %>%
  plot_distr_by_group(response_name = "coarse_fragment_vol",
                      group_name = "layer",
                      plot_title = "**Coarse fragments** (vol%)",
                      x_max = 100,
                      with_interval = FALSE)

ggsave(filename = paste0("coarse_fragment_vol_per_layer",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)





## pH ----

# Distribution M01 per humus form

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form),
            by = "plot_code_simple") %>%
  filter(depth_top == 0) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  select(humus_form, ph_cacl2) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "ph_cacl2",
                      group_name = "humus_form",
                      plot_title = "**pH-CaCl<sub>2</sub>** (0 - 10 cm)",
                      x_min = 2.5,
                      nudge = 0.07)

ggsave(filename = paste0("ph_cacl2_m01_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)



# Distribution OFH per humus form

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form),
            by = "plot_code_simple") %>%
  filter(code_layer == "OFH" &
           depth_bottom == 0) %>%
  # filter(!is.na(depth_bottom_bedrock)) %>%
  select(humus_form, ph_cacl2) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "ph_cacl2",
                      group_name = "humus_form",
                      plot_title = "**pH-CaCl<sub>2</sub>** (OFH)",
                      x_min = 2.5,
                      nudge = 0.07)

ggsave(filename = paste0("ph_cacl2_ofh_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)





# pH of deepest mineral layer

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(grepl("^M", code_layer)) %>%
  select(plot_code_simple, ph_cacl2, depth_bottom) %>%
  filter(!is.na(ph_cacl2)) %>%
  arrange(plot_code_simple, desc(depth_bottom)) %>%
  group_by(plot_code_simple) %>%
  slice_head(n = 1) %>%
  pull(ph_cacl2) %>%
  plot_distr_simple(unit = "",
                    plot_title = paste0("**pH-CaCl<sub>2</sub>** (-) of the ",
                                        "deepest mineral layer"))

ggsave(filename = paste0("ph_cacl2_bottom",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)




# pH versus TIC

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(grepl("^M", code_layer)) %>%
  filter(!is.na(ph_cacl2)) %>%
  mutate(c_inorganic_total = coalesce(c_inorganic_total, 0)) %>%
  ggplot(
    aes(
      x = ph_cacl2,
      y = c_inorganic_total,
      colour = code_layer
    )
  ) +
  geom_vline(xintercept = 5.5, colour = "orange", linewidth = 0.5) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  labs(
    title = paste0("**Total inorg. C** versus **pH-CaCl<sub>2</sub>**<br>",
                   "<span style='color:orange'>",
                   "Threshold for inorg. C analysis: ",
                   "pH-CaCl<sub>2</sub> ≥ 5.5",
                   "</span>"),
    x = "pH-CaCl<sub>2</sub> (-)",
    y = paste0("Total<br>",
               "inorg. C<br>",
               "(g C kg<sup>-1</sup>)")
  ) +
  scale_colour_depth() +
  theme_single(lineheight = 1.4)


ggsave(filename = paste0("c_inorganic_total_versus_ph_cacl2",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.9)









## pH per partner

for (partner_i in vec_inst) {

  p <- df_layers_depth %>%
    filter(institute_sampling == partner_i) %>%
    filter(!is.na(ph_cacl2)) %>%
    filter(!is.na(depth_bottom_bedrock)) %>%
    ggplot() +
    geom_line(
      aes(y = ph_cacl2,
          x = depth),
      linewidth = 0.5,
      colour = "red"
    ) +
    labs(
      title = paste0("**pH-CaCl<sub>2</sub>**"),
      x = "Depth<br>(cm)"
    ) +
    theme_facet(val_min = 2,
                val_max = 8)

  n_plots <- n_distinct(
    df_layers_depth$plot_code_simple[
      which(df_layers_depth$institute_sampling == partner_i)])
  nrow <- ceiling(n_plots / ncol)

  ggsave(filename = paste0("ph_cacl2_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/disturbed/",
                       "ph_cacl2_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 3)      # dynamic height

} # End of for loop along partners













## C:N ratio ----

# Distribution C/N OFH per humus form

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form),
            by = "plot_code_simple") %>%
  filter(code_layer == "OFH" &
           depth_bottom == 0) %>%
  # filter(!is.na(depth_bottom_bedrock)) %>%
  select(humus_form, c_to_n_ratio) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "c_to_n_ratio",
                      group_name = "humus_form",
                      plot_title = "**C:N ratio** (OFH)",
                      x_min = 15,
                      nudge = 0.07)

ggsave(filename = paste0("c_to_n_ratio_ofh_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)


# Distribution C/N M01 per humus form

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form),
            by = "plot_code_simple") %>%
  filter(depth_top == 0) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  select(humus_form, c_to_n_ratio) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "c_to_n_ratio",
                      group_name = "humus_form",
                      plot_title = "**C:N ratio** (0 - 10 cm)",
                      x_min = 10,
                      nudge = 0.07)

ggsave(filename = paste0("c_to_n_ratio_m01_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)




## C:N per partner

for (partner_i in vec_inst) {

  p <- df_layers_depth %>%
    filter(institute_sampling == partner_i) %>%
    filter(!is.na(c_to_n_ratio)) %>%
    filter(!is.na(depth_bottom_bedrock)) %>%
    ggplot() +
    geom_line(
      aes(y = c_to_n_ratio,
          x = depth),
      linewidth = 0.5,
      colour = "red"
    ) +
    labs(
      title = paste0("**C:N ratio**"),
      x = "Depth<br>(cm)"
    ) +
    theme_facet(val_min = 0,
                val_max = 50)

  n_plots <- n_distinct(
    df_layers_depth$plot_code_simple[
      which(df_layers_depth$institute_sampling == partner_i)])
  nrow <- ceiling(n_plots / ncol)

  ggsave(filename = paste0("c_to_n_ratio_over_depth_", tolower(partner_i),
                           ".png"),
         plot = p,
         path = paste0("./output/data_quality/disturbed/",
                       "c_to_n_ratio_per_partner/"),
         dpi = 500,
         width = 6.81,                 # fixed width
         height = nrow * height_per_row + 3)      # dynamic height

} # End of for loop along partners













## Areal mass ----

# Distribution OFH per humus form

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form),
            by = "plot_code_simple") %>%
  filter(code_layer == "OFH" &
           depth_bottom == 0) %>%
  # filter(!is.na(depth_bottom_bedrock)) %>%
  select(humus_form, areal_mass) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "areal_mass",
                      group_name = "humus_form",
                      plot_title = "**Areal mass** (OFH) (t ha<sup>-1</sup>)",
                      # x_min = 15,
                      nudge = 0.07)

ggsave(filename = paste0("areal_mass_ofh_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)





## Thickness ----

# Distribution OFH per humus form

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form),
            by = "plot_code_simple") %>%
  filter(code_layer == "OFH" &
           depth_bottom == 0) %>%
  # filter(!is.na(depth_bottom_bedrock)) %>%
  select(humus_form, thickness) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "thickness",
                      group_name = "humus_form",
                      plot_title = "**Thickness** (OFH) (cm)",
                      # x_min = 15,
                      nudge = 0.07)

ggsave(filename = paste0("thickness_ofh_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/disturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)





## MAT ----

# Distribution per humus form

p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  select(humus_form, mat) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "mat",
                      group_name = "humus_form",
                      plot_title = "**Mean Annual Temperature** (°C)",
                      # x_min = 15,
                      nudge = 0.07)

ggsave(filename = paste0("mat_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/stratifiers/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)











## Texture ----

tex_data <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(depth_top == 0) %>%
  filter(!is.na(clay)) %>%
  select(CLAY = clay, SILT = silt, SAND = sand) %>%
  as.data.frame()

tex_data <- TT.normalise.sum(
  tri.data = tex_data,
  css.names = c("CLAY", "SILT", "SAND")
)



png("./output/data_quality/disturbed/soil_texture_m01.png",
    width  = 18,
    height = 16,
    units = "cm",
    res = 500
    )

# par(lwd = 0.1)  # all lines at half thickness; tune as needed

TT.plot(
  class.sys = "USDA.TT",
  tri.data = tex_data,
  main = "",
  col = adjustcolor("red", alpha.f = 0.5),
  pch = 16,
  cex = 1,
  cex.main = 1.2,
  cex.lab = 0.8,
  cex.axis = 0.8
)

title(main = "Soil texture (0 - 10 cm)", line = -2, cex.main = 1.2)

dev.off()



## depth_bedrock ----

p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  pull(depth_bedrock) %>%
  plot_distr_simple(unit = "cm",
                    plot_title = paste0("**Bedrock depth** (cm)"))

ggsave(filename = paste0("depth_bedrock_vert",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3)


p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  mutate(
    plot_code_simple = reorder(plot_code_simple, depth_bedrock)
  ) %>%
  ggplot() +
  geom_pointrange(
    aes(x = depth_bedrock,
        xmin = depth_bedrock_min,
        xmax = depth_bedrock_max,
        colour = site_type,
        y = plot_code_simple),
    size = 0.02,
    linewidth = 0.2,
  ) +
  theme(axis.text.y = element_blank(),
        aspect.ratio = 1)

suppressMessages({
  ggsave(filename = "bedrock_depth_predictions_vertical.png",
         plot = p,
         path = paste0("./output/data_quality/undisturbed/"),
         dpi = 500,
         width = 6.81)
})




## vol_perc_fine_earth ----


p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  pull(vol_perc_fine_earth) %>%
  plot_distr_simple(unit = "%",
                    plot_title = paste0("**Fine earth** fraction (vol%)",
                                        " in soil profile (0 - 100 cm)"))

ggsave(filename = paste0("vol_perc_fine_earth",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3)





## Slope ----

p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  pull(slope_deg) %>%
  plot_distr_simple(unit = "°",
                    plot_title = paste0("**Slope** of the plot (°)"))

ggsave(filename = paste0("slope",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3)



p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  ggplot(
    aes(
      x = slope_deg,
      y = slope_worldclim
    )
  ) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linewidth = 0.5) +
  geom_smooth(
    method = "lm",
    formula = y ~ x - 1,
    se = FALSE,
    linewidth = 0.5
  ) +
  # Equations per code_layer
  stat_poly_eq(
    aes(
      label = paste(after_stat(eq.label),
                    after_stat(rr.label),
                    after_stat(p.value.label), sep = "~~~"),
    ),
    formula = y ~ x - 1,
    parse = TRUE
  ) +
  labs(
    title = NULL,
    x = "Slope (WILDCARD) (°)",
    y = paste0("Slope<br>",
               "(WorldClim)<br>(°)")
  ) +
  coord_cartesian(ylim = c(0, 55),
                  xlim = c(0, 55),
                  expand = FALSE) +
  theme_single()

ggsave(filename = paste0("slope_versus_worldclim",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3)






## Altitude ----

p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  pull(altitude) %>%
  plot_distr_simple(unit = "m a.s.l.",
                    plot_title = paste0("**Altitude** of the plot (m a.s.l.)"))

ggsave(filename = paste0("altitude",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3)


# Distribution per humus form

p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  select(humus_form, altitude) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = factor(humus_form,
                        levels = c("Tangel", "Amphi", "Mor", "Moder", "Mull"))
  ) %>%
  plot_distr_by_group(response_name = "altitude",
                      group_name = "humus_form",
                      plot_title = "**Altitude** (m a.s.l.)",
                      nudge = 0.07)

ggsave(filename = paste0("altitude_per_humus_form",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)








## SOC stocks ----

path <- paste0("./output/stocks/oc/20260608_oc_stocks/")

### Distribution per EFTC ----

p <- df_stocks %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(stock)) %>%
  left_join(his %>%
              select(plot_code_simple, his_eftc),
            by = "plot_code_simple") %>%
  select(his_eftc, stock) %>%
  filter(his_eftc != "other") %>%
  mutate(
    his_eftc = gsub(" forests$", "", his_eftc),
    his_eftc = gsub(" forest$", "", his_eftc),
    his_eftc = fct_reorder(his_eftc,
                           stock,
                           .fun = mean,
                           na.rm = TRUE,
                           .desc = TRUE)
  ) %>%
  plot_distr_by_group(response_name = "stock",
                      group_name = "his_eftc",
                      plot_title = paste0("**Soil organic C stock** ",
                                          "(t C ha<sup>-1</sup>)<br>",
                                          "(OFH + 0-100 cm)"),
                      nudge = 0.06)

ggsave(filename = paste0("stock_per_eftc",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)




### Distribution per WRB ----

df <- df_stocks %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(stock)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, wrb_ref_soil_group),
            by = "plot_code_simple") %>%
  select(wrb_ref_soil_group, stock)

wrb_most_common <- df %>%
  group_by(wrb_ref_soil_group) %>%
  reframe(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 5) %>%
  pull(wrb_ref_soil_group)

p <- df_stocks %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(stock)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, wrb_ref_soil_group),
            by = "plot_code_simple") %>%
  select(wrb_ref_soil_group, stock) %>%
  filter(wrb_ref_soil_group %in% wrb_most_common) %>%
  mutate(
    wrb_ref_soil_group = fct_reorder(wrb_ref_soil_group,
                                     stock,
                                     .fun = mean,
                                     na.rm = TRUE,
                                     .desc = TRUE)
  ) %>%
  plot_distr_by_group(response_name = "stock",
                      group_name = "wrb_ref_soil_group",
                      plot_title = paste0("**Soil organic C stock** ",
                                          "(t C ha<sup>-1</sup>)<br>",
                                          "(OFH + 0-100 cm)"),
                      nudge = 0.1)

ggsave(filename = paste0("stock_per_wrb_ref_soil_group",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)




### Distribution per humus form ----

p <- df_stocks %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  mutate(
    stock_forest_floor = coalesce(stock_forest_floor,
                                  0)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, humus_form),
            by = "plot_code_simple") %>%
  select(humus_form, stock_forest_floor) %>%
  filter(humus_form != "Semi-terrestrial") %>%
  mutate(
    humus_form = fct_reorder(humus_form,
                             stock_forest_floor,
                             .fun = mean,
                             na.rm = TRUE,
                             .desc = TRUE)
  ) %>%
  plot_distr_by_group(response_name = "stock_forest_floor",
                      group_name = "humus_form",
                      plot_title = paste0("**Forest floor organic C stock** ",
                                          "(t C ha<sup>-1</sup>)<br>",
                                          "(OFH)"),
                      nudge = 0.07)

ggsave(filename = paste0("stock_forest_floor_per_humus_form",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)




### Distribution per parent material ----

p <- df_stocks %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  select(-parent_material) %>%
  left_join(df_plot %>%
              select(plot_code_simple, parent_material),
            by = "plot_code_simple") %>%
  select(parent_material, oc_stock_below_ground) %>%
  mutate(
    oc_stock_below_ground = coalesce(oc_stock_below_ground, 0),
    parent_material = fct_reorder(parent_material,
                                  oc_stock_below_ground,
                                  .fun = mean,
                                  na.rm = TRUE,
                                  .desc = TRUE)
  ) %>%
  plot_distr_by_group(response_name = "oc_stock_below_ground",
                      group_name = "parent_material",
                      plot_title = paste0("**Below-ground soil organic C ",
                                          "stock** ",
                                          "(Mg C ha<sup>-1</sup>)"),
                      nudge = 0.11,
                      aspect_ratio = 1.5)

ggsave(filename = paste0("stock_below_ground_per_parent_material",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)




### Distribution stock per TSA class ----

df <- df_stocks %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  mutate(
    stock_forest_floor = coalesce(stock_forest_floor,
                                  0)) %>%
  left_join(his %>%
              select(plot_code_simple, his_tsaclass),
            by = "plot_code_simple") %>%
  select(his_tsaclass, stock, stock_below_ground_topsoil,
         stock_forest_floor, stock_below_ground_subsoil) %>%
  filter(!is.na(his_tsaclass) & !his_tsaclass %in% c("0")) %>%
  mutate(
    his_tsaclass = his_tsaclass %>%
      recode(">100" = "&gt;100") %>%
      factor(levels = c("0-30", "30-60", "60-100", "&gt;100", "primary"))
  )

p <- df %>%
  filter(!is.na(stock)) %>%
  plot_distr_by_group(response_name = "stock",
                      group_name = "his_tsaclass",
                      plot_title = paste0("**Soil organic C stock** ",
                                          "(t C ha<sup>-1</sup>)<br>",
                                          "(OFH + 0-100 cm)"),
                      nudge = 0.06)

ggsave(filename = paste0("stock_per_tsa",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)



### Distribution stock forest floor per TSA class ----

p <- df %>%
  filter(!is.na(stock_forest_floor)) %>%
  plot_distr_by_group(response_name = "stock_forest_floor",
                      group_name = "his_tsaclass",
                      plot_title = paste0("**Forest floor organic C stock** ",
                                          "(t C ha<sup>-1</sup>)<br>",
                                          "(OFH)"),
                      nudge = 0.06)

ggsave(filename = paste0("stock_forest_floor_per_tsa",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)


### Distribution stock topsoil per TSA class ----

p <- df %>%
  filter(!is.na(stock_below_ground_topsoil)) %>%
  plot_distr_by_group(response_name = "stock_below_ground_topsoil",
                      group_name = "his_tsaclass",
                      plot_title = paste0("**Soil organic C stock** ",
                                          "(t C ha<sup>-1</sup>)<br>",
                                          "(0-30 cm)"),
                      nudge = 0.06)

ggsave(filename = paste0("stock_below_ground_topsoil_per_tsa",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)



### Distribution stock subsoil per TSA class ----

p <- df %>%
  filter(!is.na(stock_below_ground_subsoil)) %>%
  plot_distr_by_group(response_name = "stock_below_ground_subsoil",
                      group_name = "his_tsaclass",
                      plot_title = paste0("**Soil organic C stock** ",
                                          "(t C ha<sup>-1</sup>)<br>",
                                          "(30-100 cm)"),
                      nudge = 0.06)

ggsave(filename = paste0("stock_below_ground_subsoil_per_tsa",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)




## Stock as a function of vol% fine earth ----

p <- df_stocks %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(stock_below_ground)) %>%
  left_join(df_plot %>%
              select(plot_code_simple, vol_perc_fine_earth),
            by = "plot_code_simple") %>%
  ggplot(aes(
    y = stock_below_ground,
    x = vol_perc_fine_earth
  )) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  labs(
    title = NULL,
    x = "Vol% **fine earth** in soil profile",
    y = paste0("**SOC stock**<br>",
               "(0-100 cm)")
  ) +
  coord_cartesian(ylim = c(0, 600),
                  xlim = c(0, 100),
                  expand = FALSE) +
  theme_single(lineheight = 1.5)


ggsave(filename = paste0("stock_versus_fine_earth_vol",
                         ".png"),
       plot = p,
       path = path,
       dpi = 500,
       width = 6.81,
       height = 3.3)




## Bulk density as a function of TOC ----

source("./src/functions/bd_ptf.R")

df <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  filter(!(grepl("^O", code_layer) & depth_top < 0)) %>%
  filter(!is.na(bulk_density_undist) & !is.na(c_organic_total) &
           # values above are peat (Rejviz)
           c_organic_total < 300) %>%
  mutate(
    name = "Coarse fragments<br>(vol%)",
    cf_cat = case_when(
      coarse_fragment_vol < 2 ~ "0-2 vol%", # 1st quantile
      coarse_fragment_vol < 15 ~ "2-15 vol%", # median
      coarse_fragment_vol < 50 ~ "15-50 vol%", # 3rd quantile
      !is.na(coarse_fragment_vol) ~ "50-100 vol%"),
    cf_cat = factor(cf_cat,
                    levels = c("0-2 vol%", "2-15 vol%",
                               "15-50 vol%", "50-100 vol%"))
  )

ptf_df <- tibble(
  c_organic_total = seq(
    min(df$c_organic_total, na.rm = TRUE),
    max(df$c_organic_total, na.rm = TRUE),
    length.out = 200
  )
) %>%
  rowwise() %>%
  mutate(
    pred = list(bd_ptf(c_organic_total))
  ) %>%
  tidyr::unnest_wider(pred)


p <- df %>%
  ggplot(aes(
    y = bulk_density,
    x = c_organic_total,
    colour = cf_cat
  )) +
  geom_ribbon(
    data = ptf_df,
    aes(
      x = c_organic_total,
      ymin = bulk_density_ptf_min,
      ymax = bulk_density_ptf_max
    ),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.3
  ) +
  geom_line(
    data = ptf_df,
    aes(
      x = c_organic_total,
      y = bulk_density_ptf,
      linetype = "Pedotransfer<br>(ICP Forests Level I)"
    ),
    inherit.aes = FALSE,
    colour = "orange",
    linewidth = 0.5
  ) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  labs(
    title = NULL,
    x = "Total **org. C** (g C kg<sup>-1</sup>)",
    y = paste0("**Bulk density**<br>",
               "(kg m<sup>-3</sup>)")
  ) +
  scale_colour_manual(
    name = "Coarse fragments",
    values = c(
      "0-2 vol%" = "#AADC32",
      "2-15 vol%" = "#27AD81",
      "15-50 vol%" = "#2C728E",
      "50-100 vol%" = "#472D7B"
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Pedotransfer<br>(ICP Forests Level I)" = "solid"
    )
  ) +
  coord_cartesian(ylim = c(0, 1800),
                  xlim = c(0, 300),
                  expand = FALSE) +
  theme_single(lineheight = 1.4)


ggsave(filename = paste0("bulk_density_versus_toc",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)




## Bulk density as a function of coarse fragments ----

p <- df_layers %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  filter(!is.na(depth_bottom_bedrock)) %>%
  filter(!(grepl("^O", code_layer) & depth_top < 0)) %>%
  filter(!is.na(bulk_density_undist) & !is.na(coarse_fragment_vol)) %>%
  ggplot(aes(
    y = bulk_density,
    x = coarse_fragment_vol,
    colour = code_layer
  )) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  # Locally weighted regressing line
  geom_smooth(method = "loess", se = TRUE,
              color = "orange", linewidth = 0.5,
              fill = "grey70", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE,
              color = "black", linewidth = 0.5) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~"),
        colour = NULL,
        group = 1),
    formula = y ~ x,
    parse = TRUE,
    lineheight = 1.2,
    size = 3,
    color = "black"
  ) +
  labs(
    title = NULL,
    x = "**Coarse fragments** (vol%)",
    y = paste0("**Bulk density**<br>",
               "(kg m<sup>-3</sup>)")
  ) +
  scale_colour_depth() +
  coord_cartesian(ylim = c(0, 1800),
                  xlim = c(0, 100),
                  expand = FALSE) +
  theme_single(lineheight = 1.4)


ggsave(filename = paste0("bulk_density_versus_coarse_fragment_vol",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)



## Stock as a function of clay ----

# p <- df_stocks %>%
#   filter(!grepl("EXTRA", plot_code_simple)) %>%
#   filter(!is.na(stock_below_ground)) %>%
#   left_join(df_plot %>%
#               select(plot_code_simple, vol_perc_fine_earth),
#             by = "plot_code_simple") %>%
#   ggplot(aes(
#     y = stock_below_ground,
#     x = vol_perc_fine_earth
#   )) +
#   geom_point(size = 0.8,
#              alpha = 0.5) +
#   labs(
#     title = NULL,
#     x = "Vol% **fine earth** in soil profile",
#     y = paste0("**SOC stock**<br>",
#                "(0-100 cm)")
#   ) +
#   coord_cartesian(ylim = c(0, 600),
#                   xlim = c(0, 100),
#                   expand = FALSE) +
#   theme_single(lineheight = 1.5)
#
#
# ggsave(filename = paste0("stock_versus_fine_earth_vol",
#                          ".png"),
#        plot = p,
#        path = path,
#        dpi = 500,
#        width = 6.81,
#        height = 3.3)





## Stock as a function of pH ----

# p <- df_stocks %>%
#   filter(!grepl("EXTRA", plot_code_simple)) %>%
#   filter(!is.na(stock_below_ground)) %>%
#   left_join(df_plot %>%
#               select(plot_code_simple, vol_perc_fine_earth),
#             by = "plot_code_simple") %>%
#   ggplot(aes(
#     y = stock_below_ground,
#     x = vol_perc_fine_earth
#   )) +
#   geom_point(size = 0.8,
#              alpha = 0.5) +
#   labs(
#     title = NULL,
#     x = "Vol% **fine earth** in soil profile",
#     y = paste0("**SOC stock**<br>",
#                "(0-100 cm)")
#   ) +
#   coord_cartesian(ylim = c(0, 600),
#                   xlim = c(0, 100),
#                   expand = FALSE) +
#   theme_single(lineheight = 1.5)
#
#
# ggsave(filename = paste0("stock_versus_fine_earth_vol",
#                          ".png"),
#        plot = p,
#        path = path,
#        dpi = 500,
#        width = 6.81,
#        height = 3.3)





## Vol fine earth as a function of slope ----

p <- df_plot %>%
  filter(!grepl("EXTRA", plot_code_simple)) %>%
  ggplot(aes(
    x = slope_deg,
    y = vol_perc_fine_earth
  )) +
  geom_point(size = 0.8,
             alpha = 0.5) +
  labs(
    title = NULL,
    x = "**Slope** of the plot (°)",
    y = paste0("**Fine earth** fraction<br>",
               "in soil profile (0 - 100 cm)<br>(vol%)")
  ) +
  coord_cartesian(ylim = c(0, 100),
                  expand = FALSE) +
  theme_single(lineheight = 1.4)


ggsave(filename = paste0("fine_earth_frac_versus_slope",
                         ".png"),
       plot = p,
       path = paste0("./output/data_quality/undisturbed/"),
       dpi = 500,
       width = 6.81,
       height = 3.3)

