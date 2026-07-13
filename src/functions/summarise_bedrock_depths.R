
summarise_bedrock_depths <- function(df_bedrock,
                                     iter = 2000,
                                     correction_status = "with_corrections") {

  # SAMPLING DESIGN ---

  # Each plot has 9 sampling points (P1-P9), covered until a depth of 100 cm
  # or shallower. If bedrock is encountered within the covered depth range,
  # it is recorded (in theory); else, it is NA. Hereby, partners measured
  # following one of the following protocols:

  # Standard protocol:
  #   P[1-5]: sampled to 100 cm depth. NA = bedrock not reached within 100 cm.
  #   P[6-9]: sampled to 30 cm depth. NA = bedrock not reached within 30 cm.

  # Stony site protocol:
  #   P1:     sampled to 100 cm (profile pit until 80 cm and further augering
  #           until 100 cm).
  #   P[2-9]: sampled to 30 cm. NA = bedrock not reached within 30 cm.
  #           Non-NA values are usually <= 30 cm, but occasionally deeper if the
  #           field team continued past 30 cm (valid measurements).

  # NA REPLACEMENT FOR C-STOCK CALCULATION ---

  # Goal: integrate SOC stocks over the upper 100 cm (or until bedrock).
  # As we are only interested in SOC located in the upper 100 cm, the exact
  # depth of bedrock beyond 100 cm is irrelevant: whether bedrock is at 101 cm
  # or 200 cm, the integration depth is capped at 100 cm.

  # BRMS APPROACH ---
  # (Thanks to INBO statistician)

  # To obtain a plot-level summary of bedrock depth (with uncertainty), we use a
  # Bayesian censored regression model (brms), treating bedrock depth as a
  # censored continuous variable. Right-censoring reflects that we know a lower
  # bound but not the true value. By using this approach, we are able to use all
  # nine sampling points for each plot, including those where the partner did
  # not go down to a depth of 100 cm.

  # The following censoring bounds are used:

  # · NA at P[6-9] (standard protocol) or NA at P[2-9] (stony protocol):
  #   right-censored above 30 cm

  # · NA at points that are supposed to be covered until 100 cm, but for which
  #   sampling is known to be stopped more shallowly due to stoniness:
  #   right-censored above x cm, for which x is the deepest depth reached.

  # A censored regression model is fitted per plot using brms:

  #   formula <- bf(bedrock_depth_m | cens(censoring) ~ 1)

  # where:
  #   bedrock_depth_m  = observed depth (in metres) or lower censoring bound
  #   censoring        = 'none' or 'right'
  #   ~ 1              = The predictor: intercept only. No covariates. This
  # means the model estimates a single underlying mean bedrock depth for all
  # observations going into this formula (i.e., all sampling points within one
  # plot, if you fit per plot). The posterior of this intercept is what you
  # extract as the plot-level representative bedrock depth. If you later fit
  # a hierarchical model across all plots simultaneously, you could replace 1
  # with plot-level covariates or add a random effect, e.g., ~ (1 | plot_id) to
  # get partial pooling across plots.

  # A beta distribution (after rescaling depths to [0, 1]) is used given the
  # bounded nature of depths.

  # From the posterior, we extract:
  #  · posterior mean (or median) as the plot-level representative bedrock depth
  #  · 95% credible interval (qi) as the uncertainty range

  # Note: the assumption is that all depths are ortogonal to the local slope
  # gradient


  # Assert that the required columns are present
  assert_that("plot_code_simple" %in% names(df_bedrock))
  assert_that("site_type" %in% names(df_bedrock))
  assert_that(all(na.omit(unique(df_bedrock$site_type)) %in%
                                c("Standard site", "Stony site")))
  assert_that("p_id" %in% names(df_bedrock))
  assert_that(
    # Make sure there are nine sampling points are present (long format)
    identical(9L,
              df_bedrock %>%
                group_by(plot_code_simple) %>%
                reframe(count = n()) %>%
                distinct(count) %>%
                pull(count)))
  assert_that("depth_ort_m" %in% names(df_bedrock))
  assert_that(all(!is.na(df_bedrock$depth_ort_m)) &&
                min(df_bedrock$depth_ort_m, na.rm = TRUE) >= .005 &&
                max(df_bedrock$depth_ort_m, na.rm = TRUE) <= .995)
  assert_that("censoring" %in% names(df_bedrock))
  assert_that(all(unique(df_bedrock$censoring) %in% c("none", "right")))


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

  model_dir <- here::here("models", "bedrock_brms", correction_status)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }

  # The converted to meter values in the continuous (0, 1) range
  # can be modelled using a beta distribution
  # accounting for censoring if needed

  # Fit empty model
  # This will compile the model
  # We can reuse it to fit it to newdata (subset for each plot)
  m_depth <- brm(
    formula = bf(
      depth_ort_m | cens(censoring) ~ 1
    ),
    data = df_bedrock,
    cores = 4,
    iter = 4, # setting to low number for empty model
    family = Beta(),
    backend = "cmdstanr",
    file = here::here("models", "bedrock_brms", correction_status, "m_depth"),
    file_refit = "on_change"
  )

  # helper function to iteratively fit the model to new data
  fit_beta <- function(x, y, model = m_depth, iter = 2000) {
    y <- gsub("/", "_", y) # avoid / slash in filename
    m <- update(
      model,
      . ~ .,
      newdata = x,
      # Should probably increase iter to 2000 (the default)
      # but to speed up computation lowered it to 1000
      iter = iter,
      cores = 4, #
      family = Beta(),
      backend = "cmdstanr",
      file = here::here("models", "bedrock_brms", correction_status,
                        paste0("m_depth_", y)),
      file_refit = "on_change"
    )
    return(m)
  }

  calc_predictions <- function(x) {

    # Calculate predictions and confidence intervals
    df <- epred_draws(x, newdata = data.frame(intercept = NA)) %>%
      # Summarise to get the posterior mean and 95% credible intervals
      # (.lower, .upper)
      mean_qi(.epred)

    return(df)
  }


  # Fitting the 208 models takes approx 1 hour for iter = 2000

  bedrock_models <- df_bedrock %>%
    nest(.by = c(plot_code_simple, site_type)) %>%
    mutate(
      model = map2(
        data,
        plot_code_simple,
        \(x, y) fit_beta(x, y, iter = iter)
      )
    )

  bedrock_models <- bedrock_models %>%
    mutate(
      pred = map(
        .x = model,
        .f = calc_predictions
      )
    )

  bedrock_predictions <- bedrock_models %>%
    select(-data, -model) %>%
    unnest(pred)

  write.table(bedrock_predictions,
              file = here::here("data", "bedrock_depth_predictions.csv"),
              row.names = FALSE,
              na = "",
              sep = ";",
              dec = ".")


  p <- bedrock_predictions %>%
    mutate(
      plot_code_simple = reorder(plot_code_simple, .epred)
    ) %>%
    ggplot() +
    geom_pointrange(
      aes(x = .epred, xmin = .lower, xmax = .upper, colour = site_type,
          y = plot_code_simple),
      size = 0.02,
      linewidth = 0.2,
    ) +
    theme(axis.text.y = element_blank(),
          aspect.ratio = 1)

  suppressMessages({
     ggsave(filename = "bedrock_depth_predictions.png",
         plot = p,
         path = here::here("models", "bedrock_brms", correction_status),
         dpi = 500,
         width = 6.81)
  })


  # Check if Rhat values (diagnostic for chains mixing) are fine
  # Should be <= 1.01, but a little higher (1.03) is still acceptable for
  # this purpose
  # Increase iter argument to have smaller Rhat

  # Note: several cases with a high Rhat, usually for plots with right-censored
  # points only. Therefore, it makes sense that the model has difficulties
  # to find a good estimate, and the outcome is sufficient for this study,
  # considering that we take the uncertainty into account.

  rhats <- bedrock_models %>%
    mutate(
      rhat_intercept = map_dbl(
        model,
        ~ rhat(.x)[["Intercept"]]
      )
    )

  assert_that(round(quantile(rhats$rhat_intercept, c(0.90)), 3) <= 1.03)

  return(bedrock_predictions)

}
