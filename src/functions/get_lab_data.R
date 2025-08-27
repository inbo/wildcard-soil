
get_lab_data <- function() {

  # You must either be in office or have the INBO VPN switched on!

  cat("Make sure that the VPN of INBO is switched on.\n")

  # Check: https://github.com/inbo/inbolims

  # Make connection with the database
  connection <- lims_connect()

  # We need the rf_overview table
  source("./src/functions/get_rf.R")

  rfs <- unique(rf_overview$rf)

  # This way, you can see the samples for a certain RF:
  # lims_sample_information(connection, project = c("V-24V006-01"))

  # LOQs

  loq_tc <- 0.5
  loq_tic <- 0.1
  loq_tn <- 0.1

  harmonise_below_loqs <- function(var, tc = NULL, tic = NULL, tn = NULL,
                                   loq_tc = 0.5, # g C kg-1 DS
                                   loq_tic = 0.1, # g C kg-1 DS
                                   loq_tn = 0.1) { # g N kg -1 DS

    if (var == "TOC") {

      assertthat::assert_that(!is.null(tc))

      if (is.null(tic)) {
        tic <- 0
      }

      toc_harm <- case_when(
        # TIC is 0, so TOC ≈ TC
        tic == 0 & tc < loq_tc ~ 0.5 * loq_tc,
        # General case when TIC is not 0
        (tc - tic) < (loq_tc - loq_tic) ~ 0.5 * (loq_tc - loq_tic),
        # Above LOQ
        TRUE ~ (tc - tic))

      return(toc_harm)
    }

    if (var == "TIC") {

      if (is.null(tic)) {
        tic <- 0
      }

      tic_harm = case_when(
        tic == 0 ~ 0,
        tic < loq_tic ~ 0.5 * loq_tic,
        TRUE ~ tic)

      return(tic_harm)
    }


    if (var %in% c("TN", "TC")) {

      tn  <- if (is.null(tn))  NA_real_ else tn
      tc  <- if (is.null(tc))  NA_real_ else tc

      value <- case_when(
        var == "TN" ~ tn,
        var == "TC" ~ tc)

      loq_value <- case_when(
        var == "TN" ~ loq_tn,
        var == "TC" ~ loq_tc)

      assertthat::assert_that(!is.null(value))

      value_harm = case_when(
        value < loq_value ~ 0.5 * loq_value,
        TRUE ~ value)

      return(value_harm)
    }
  }



  # Lab measurement uncertainty

  compute_uncertainty <- function(var, tc = NULL, tic = NULL, tn = NULL,
                                  urel_tc = 0.09,
                                  urel_tic = 0.106,
                                  urel_tn = 0.06) {
    # var: "TOC", "TIC", "TC" or "TN"
    # tc, tic, tn: numeric vectors from your dataframe
    # urel_* : relative expanded uncertainties (fractions). Defaults are
    # uncertainty in lab analyses ("Meetonzekerheid") calculated by INBO BMK.

    # Returns ABSOLUTE uncertainty!

    if (var == "TOC") {

      assertthat::assert_that(!is.null(tc))

      if (is.null(tic)) {
        tic <- 0
      }

      # convert to standard uncertainties
      u_tc  <- (urel_tc  * tc)
      u_tic <- (urel_tic * tic)

      # combined
      u_toc <- sqrt(u_tc^2 + u_tic^2) # Assuming independence

      return(u_toc)

    }

    if (var %in% c("TN", "TC", "TIC")) {

      tn  <- if (is.null(tn))  NA_real_ else tn
      tc  <- if (is.null(tc))  NA_real_ else tc
      tic <- if (is.null(tic)) NA_real_ else tic

      value <- case_when(
        var == "TN" ~ tn,
        var == "TC" ~ tc,
        var == "TIC" ~ tic)

      urel <- case_when(
        var == "TN" ~ urel_tn,
        var == "TC" ~ urel_tc,
        var == "TIC" ~ urel_tic)

      assertthat::assert_that(!is.null(value))

      u_var <- (urel * value)

      return(u_var)
    }
  }


  # Final uncertainty ranges lab

  add_lab_uncertainty <- function(var, tc = NULL, tic = NULL, tn = NULL,
                                  loq_tc = 0.5, # g C kg-1 DS
                                  loq_tic = 0.1, # g C kg-1 DS
                                  loq_tn = 0.1) {

    # This function returns the minimum and the maximum of the uncertainty
    # interval, taking the LOQ into account (uncertainty range for below-LOQ
    # values goes from 0 to LOQ)

    var_long <- case_when(
      var == "TOC" ~ "c_organic_total",
      var == "TC" ~ "c_total",
      var == "TIC" ~ "c_inorganic_total",
      var == "TN" ~ "n_total")

    u_var <- compute_uncertainty(var = var,
                                 tc = tc,
                                 tic = tic,
                                 tn = tn)

    if (var == "TOC") {

      assertthat::assert_that(!is.null(tc))

      if (is.null(tic)) {
        tic <- 0
      }

      var_min <- case_when(
        # TIC is 0, so TOC ≈ TC
        tic == 0 & tc < loq_tc ~ 0,
        # General case when TIC is not 0
        (tc - tic) < (loq_tc - loq_tic) ~ 0,
        # Above LOQ
        TRUE ~ max(c((tc - tic) - u_var, 0)))

      var_max <- case_when(
        # TIC is 0, so TOC ≈ TC
        tic == 0 & tc < loq_tc ~ loq_tc,
        # General case when TIC is not 0
        (tc - tic) < (loq_tc - loq_tic) ~ (loq_tc - loq_tic),
        # Above LOQ
        TRUE ~ min(c((tc - tic) + u_var,
                     1E3))) # Values cannot be higher than 1E3 since reported
                            # in g kg-1
    }

    if (var == "TIC") {

      if (is.null(tic)) {
        tic <- 0
      }

      var_min = case_when(
        tic == 0 ~ 0,
        tic < loq_tic ~ 0,
        TRUE ~ max(c(tic - u_var, 0)))

      var_max = case_when(
        tic == 0 ~ 0,
        tic < loq_tic ~ loq_tic,
        TRUE ~ min(c(tic + u_var, 1E3)))
    }


    if (var %in% c("TN", "TC")) {

      tn  <- if (is.null(tn))  NA_real_ else tn
      tc  <- if (is.null(tc))  NA_real_ else tc

      value <- case_when(
        var == "TN" ~ tn,
        var == "TC" ~ tc)

      loq_value <- case_when(
        var == "TN" ~ loq_tn,
        var == "TC" ~ loq_tc)

      assertthat::assert_that(!is.null(value))

      var_min = case_when(
        value < loq_value ~ 0,
        TRUE ~ max(c(value - u_var, 0)))

      var_max = case_when(
        value < loq_value ~ loq_value,
        TRUE ~ min(c(value + u_var, 1E3)))

    }

    var_min <- round(var_min, 2)
    var_max <- round(var_max, 2)

    return(
      setNames(
        list(var_min, var_max),
        c(paste0(var_long, "_min"), paste0(var_long, "_max"))))
  }



  # Compile all RFs ----

  data_lab <- NULL

  for (rf_i in rfs) {

    # Lab data in long format:

    # Samples from test sampling last year
    # data_lab_i <- read_lims_data(connection = connection,
    #                              project = c("V-24V006-01"),
    #                              sql_template = "default",
    #                              show_query = FALSE)

    data_lab_i <- read_lims_data(connection = connection,
                                 project = c(rf_i),
                                 sql_template = "default",
                                 show_query = FALSE)

    # Remove rows from duplicate samples (10 % of the samples is measured
    # twice for internal quality control of the analytical lab)

    samples_dup <- data_lab_i %>%
      filter(grepl("DUP", ProductGrade)) %>%
      distinct(OrigineelStaal) %>%
      pull(OrigineelStaal)

    datax_lab_i <- lims_report_xtab(data_lab_i)

    datax_lab_i <- datax_lab_i %>%
      # Automatically detect and convert columns that contain only
      # numeric-convertible values to numeric class
      mutate(across(
        where(~ all(suppressWarnings(!is.na(as.numeric(.)) | is.na(.)))),
        as.numeric)) %>%
      # Remove samples from duplicate analysis
      filter(!OrigineelStaal %in% samples_dup) %>%
      # Rename the column names in order to ignore the lab's test and result
      # replicates in the column names
      # (i.e., remove trailing patterns of "__<number>__<number>")
      rename_with(~ gsub("__\\d+__\\d+$", "", .x)) %>%
      mutate(
        # if TIC col doesn't exist, create it and fill with 0
        "C_TIC_ANALYSER__T.IC.DS" =
        if (!"C_TIC_ANALYSER__T.IC.DS" %in% names(.)) 0 else
          C_TIC_ANALYSER__T.IC.DS) %>%
      # Add the uncertainty ranges of all variables first
      rowwise() %>%
      mutate(toc_unc =
               list(add_lab_uncertainty("TOC",
                                              tc = C_N_ANAL_V__T.C.DS,
                                              tic = C_TIC_ANALYSER__T.IC.DS)),
             tn_unc =
               list(add_lab_uncertainty("TN",
                                              tn = C_N_ANAL_V__T.N.DS)),
             tic_unc =
               list(add_lab_uncertainty("TIC",
                                              tic = C_TIC_ANALYSER__T.IC.DS))
             ) %>%
      ungroup() %>%
      unnest_wider(toc_unc) %>%
      unnest_wider(tn_unc) %>%
      unnest_wider(tic_unc) %>%
      mutate(
        c_organic_total =
          round(harmonise_below_loqs("TOC",
                                     tc = C_N_ANAL_V__T.C.DS,
                                     tic = C_TIC_ANALYSER__T.IC.DS), 2),
        n_total = round(harmonise_below_loqs("TN",
                                             tn = C_N_ANAL_V__T.N.DS), 2),
        c_inorganic_total =
          round(harmonise_below_loqs("TIC",
                                     tic = C_TIC_ANALYSER__T.IC.DS), 2),
        ph_cacl2 = round(PHCACL2_VOL_SP2000_V__pH.CaCl2.20, 2),
        # IMPORTANT: the boundaries for clay, silt, sand according to laser
        # diffraction possibly need to be revised!!!
        clay = round(TEXTUUR_LD_LS13320_V__KLEI.04.6µm, 1),
        silt = round(TEXTUUR_LD_LS13320_V__LEEM.6.63µm, 1),
        sand = round(TEXTUUR_LD_LS13320_V__ZAND.63.2000µm, 1)) %>%
      rename(sample_id_harm = ExternSampleID) %>%
      select(
        sample_id_harm,
        c_organic_total, c_organic_total_min, c_organic_total_max,
        n_total, n_total_min, n_total_max,
        c_inorganic_total, c_inorganic_total_min, c_inorganic_total_max,
        ph_cacl2,
        clay,
        silt,
        sand)

    data_lab <- bind_rows(data_lab,
                          datax_lab_i)

  }

return(as_tibble(data_lab))

}
