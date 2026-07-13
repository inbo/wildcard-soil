
summarise_forest_floor_masses <- function(df_sampling_points_ff,
                                          iter = 2000) {

  # Areal mass uncertainty estimation using Bayesian Gamma GLM
  # Input:  df_sampling_points_ff — one row per sampling frame per plot x layer
  #         tamass_mean = field-moist mass (g) for one sampling frame
  # Output: posterior mean areal mass (kg m-2) + 95% credible interval per
  #         plot x layer, for use as central estimate in SOC stock calculations

  # One model per plot x layer combination (OL and OFH fitted separately,
  # as they represent structurally different organic layers with different
  # mass distributions — no shrinkage across plots needed)

  # Template model (iter = 4 just to compile Stan code without real sampling)
  # Gamma(link = "log"): appropriate for positive continuous right-skewed mass
  # data. The log link means the intercept is on log scale internally, but
  # epred_draws back-transforms to the original kg m-2 scale automatically —
  # no manual exp() needed, unlike a log-normal approach.


  # From the posterior, we extract:
  #  · posterior mean (or median) as the representative thickness
  #  · 95% credible interval (qi) as the uncertainty range



  # Assert that the required columns are present
  assert_that("plot_code_simple" %in% names(df_sampling_points_ff))
  assert_that("code_layer" %in% names(df_sampling_points_ff))
  assert_that("sampling_point" %in% names(df_sampling_points_ff))
  assert_that(
    # Make sure there are five sampling points are present (long format)
    identical(5L,
              df_sampling_points_ff %>%
                group_by(plot_code_simple, code_layer) %>%
                reframe(count = n()) %>%
                distinct(count) %>%
                pull(count)))
  assert_that("tamass" %in% names(df_sampling_points_ff))

  # Check if everything is installed

  if (!require("brms", quietly = TRUE) ||
      !require("cmdstanr", quietly = TRUE)) {

    cat(paste0("\nInstallation instructions:\n",
               "(Running this in a fresh R session or starting your current ",
               "session)\n",
               "install.packages('brms')\n",
               "install.packages('cmdstanr', ",
               "repos = c('https://stan-dev.r-universe.dev', ",
               "getOption('repos')))\n",
               "Install external tool 'cmdstan' (not an R package): ",
               "cmdstanr::install_cmdstan()\n\n"))
  }

  stopifnot(require("brms"),
            require("cmdstanr"),
            require("tidybayes"))


  # Create directory to save models (.RDS) if needed

  model_dir <- here::here("models", "forest_floor_mass_brms")
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }

  # Template model - compiled once, reused via update() for each plot x layer.
  # Purpose of the template model is to compile the Stan C++ code so that
  # subsequent update() calls skip recompilation. The 4-iteration fit is
  # intentionally thrown away - you never extract predictions from it -
  # so no worries about any warning messages related to divergence or E-BFMI
  # not being computed.

  # Gamma(link = "log"): positive continuous right-skewed mass data.
  # Intercept-only: we estimate the expected areal mass for each plot x layer.
  # epred_draws will back-transform from log scale automatically.

  forest_floor_mass <- brm(
    formula = bf(tamass ~ 1),
    data = df_sampling_points_ff %>%
      filter(!is.na(tamass)) %>%
      head(5), # minimal real data to compile
    family = Gamma(link = "log"),
    backend = "cmdstanr",
    iter = 4,
    file = here::here("models", "forest_floor_mass_brms",
                      "forest_floor_mass_template")
  )

  # Helper: fit the template model to a new plot x layer subset
  # y is used only for the cache filename (plot_code x layer combination)

  fit_gamma <- function(x, y, model = forest_floor_mass, iter = 2000) {
    y <- gsub("/", "_", y)  # sanitise filename (avoid slashes from plot codes)

    m <- update(
      model,
      . ~ .,
      newdata = x,
      family = Gamma(link = "log"),
      backend = "cmdstanr",
      iter = iter,
      file = here::here("models", "forest_floor_mass_brms",
                        paste0("forest_floor_mass_", y)),
      file_refit = "on_change"  # only refit if data or model spec changed
    )

    return(m)
  }



  # Helper: extract posterior mean and 95 % credible interval from a fitted
  # model
  # epred_draws gives draws from the expected value of the posterior predictive
  # distribution (i.e. the mean of the Gamma, back-transformed from log scale).
  # mean_qi summarises to posterior mean + 95% CI (.lower, .upper).
  # No log transformation needed: epred_draws handles the link function
  # inversion.

  calc_predictions <- function(x) {
    df <- epred_draws(x, newdata = data.frame(tamass = NA)) %>%
      median_qi(.epred)  # posterior median and 95% credible interval
    # (Median is much better in case of clear skews, and we will anyway
    #  use the arithmetic mean)

    return(df)
  }


  # Track how many frames had presence vs absence

  df_sampling_points_ff <- df_sampling_points_ff %>%
    group_by(plot_code_simple, code_layer) %>%
    mutate(
      # count non-zero, non-NA frames per plot x layer
      n_frames_present = sum(!is.na(tamass) & tamass > 0),
      # count all non-NA frames per plot x layer
      n_frames_total = sum(!is.na(tamass))
    ) %>%
    ungroup


  # Fit one model per plot x layer combination
  # Nesting by both plot_code_simple and code_layer ensures OL and OFH
  # are treated as independent estimation problems with their own likelihoods

  mass_models <- df_sampling_points_ff %>%
    # Plots with no frame data cannot be fitted
    filter(!is.na(tamass)) %>%
    # Exclude structural zeros before modelling
    # (Else, this gives problems with the model. Later, we will multiply
    #  posteriors with the fraction of frames for which tamass > 0 to
    #  correct for this)
    filter(tamass > 0) %>%
    # Long format: one row per frame measurement per plot x layer
    # (areal_mass here is the per-frame value, not the plot mean)
    nest(.by = c(plot_code_simple, code_layer)) %>%
    filter(purrr::map_int(data, ~ nrow(.x)) >= 1) %>% # safety check
    mutate(
      # Concatenate plot code and layer for unique cache filenames
      model_id = paste0(plot_code_simple, "_", code_layer),
      model = map2(
        data,
        model_id,
        \(x, y) fit_gamma(x, y, iter = iter)
      )
    )

  # Extract posterior summaries
  # For single-frame plots (n = 1), the posterior will be wide and
  # prior-dominated — this is honest: we genuinely have little information.
  # The credible interval will reflect that, rather than collapsing to a point.

  mass_models <- mass_models %>%
    mutate(
      pred = map(model, calc_predictions)
    ) %>%
    mutate(
      shape_mean = map_dbl(model, ~ {
        draws <- as_draws_df(.x)
        mean(draws$shape)
      })
    )

  mass_predictions <- bind_rows(
    # For n > 1
    mass_models %>%
      filter(purrr::map_int(data, ~ nrow(.x)) > 1) %>%
      mutate(pred = map(model, calc_predictions)) %>%
      select(-data, -model) %>%
      unnest(pred) %>%
      mutate(across(where(is.numeric),
                    ~ as.numeric(format(.x, scientific = FALSE)))) %>%
      # Correct for the number of sampling points with tamass = 0
      left_join(
        df_sampling_points_ff %>%
          select(
            plot_code_simple, code_layer,
            n_frames_present, n_frames_total) %>%
          distinct(plot_code_simple, code_layer, .keep_all = TRUE),
        by = join_by("plot_code_simple", "code_layer")) %>%
      mutate(
        across(c(.epred, .lower, .upper),
               ~ .x * (n_frames_present / n_frames_total))
      ) %>%
      mutate(
        bayes_remark = case_when(
          n_frames_present < n_frames_total & shape_mean < 1 ~
            "any_zero_with_skew",
          n_frames_present < n_frames_total ~ "any_zero",
          shape_mean < 0.5 ~ "high_skew",
          shape_mean < 1 ~ "skew")
        ) %>%
      select(-starts_with("n_frames_"), -starts_with("shape_")),
    # For n = 1: no uncertainty can be estimated from the data alone.
    # Report the single observation as the point estimate, CI as NA.
    # Uncertainty for these plots should be flagged and handled separately
    mass_models %>%
      filter(purrr::map_int(data, ~ nrow(.x)) == 1) %>%
      mutate(
        .epred = NA_real_,
        .lower = NA_real_,
        .upper = NA_real_,
        .width = 0.95,
        .point = "mean",
        .interval = "qi"
      ) %>%
      select(-data, -model, -pred) %>%
      mutate(
        bayes_remark = "single_frame"))


  # Write output

  write.table(
    mass_predictions,
    file = here::here("data", "areal_mass_predictions.csv"),
    row.names = FALSE,
    na = "",
    sep = ";",
    dec = "."
  )

  # Diagnostic plot: pointrange sorted by posterior mean, coloured by layer
  # Wide intervals flag plots with high spatial variability or single frames

  p <- mass_predictions %>%
    filter(!is.na(.epred)) %>%
    mutate(plot_code_simple = reorder(plot_code_simple, .epred)) %>%
    ggplot() +
    geom_pointrange(
      aes(
        x = .epred,
        xmin = .lower,
        xmax = .upper,
        y = plot_code_simple,
        colour = code_layer  # OL vs OFH distinguished by colour
      ),
      size = 0.02,
      linewidth = 0.2
    ) +
    labs(x = "Field-moist mass for one sampling point (g)", colour = "Layer") +
    theme(
      axis.text.y  = element_blank(),
      aspect.ratio = 1
    )

  suppressMessages({
    ggsave(
      filename = "areal_mass_predictions.png",
      plot     = p,
      path     = here::here("models", "forest_floor_mass_brms"),
      dpi      = 500,
      width    = 6.81
    )
  })

  # Chain convergence diagnostics
  # Rhat <= 1.01 is ideal; up to 1.03 acceptable for this application.
  # Values above 1.03 suggest chains haven't mixed — increase iter.
  # With n = 1 plots the intercept-only Gamma model rarely has mixing issues,
  # but worth checking especially for the high-variance cases (e.g. 26,44,121,1593).

  rhats <- map_dfr(mass_models$model, rhat)
  assert_that(
    round(quantile(rhats$Intercept, 0.95), 2) <= 1.03,
    msg = "Rhat > 1.03 for some models — consider increasing iter"
  )

  return(mass_predictions)

}

