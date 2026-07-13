
get_lab_data <- function(vec_rf) {

  # You must either be in office or have the INBO VPN switched on!

  cat("Make sure that the VPN of INBO is switched on.\n")

  # Troubleshooting:
  # If you get an error with a message like:
     # ! ODBC failed with error 08S01 from [Microsoft][ODBC SQL Server Driver].
     # ✖ Communicatiekoppelingsfout
  # Try again to switch off and switch on the VPN,
  # and rerun "connection <- lims_connect"


  # Check: https://github.com/inbo/inbolims

  # Make connection with the database
  if (!exists("connection", envir = globalenv())) {
    connection <- lims_connect()
  }



  # This way, you can see the samples for a certain RF:
  # lims_sample_information(connection, project = c("V-24V006-01"))

  # LOQs

  loq_tc <- 0.5
  loq_tic <- 0.1
  loq_tn <- 0.1
  loq_hwc <- 50

  # Create function harmonise_below_loqs

  harmonise_below_loqs <- function(var,
                                   tc = NULL, tic = NULL, tn = NULL, hwc = NULL,
                                   loq_tc = 0.5, # g C kg-1 DS
                                   loq_tic = 0.1, # g C kg-1 DS
                                   loq_tn = 0.1, # g N kg -1 DS
                                   loq_hwc = 50 # mg C kg-1 DS
                                   ) {

    # This function converts below-LOQ values to 50 % of the LOQ

    # var: "TOC", "TIC", "TC" or "TN"
    # tc, tic, tn: numeric vectors with total C, total inorganic C or
    # total N values
    # loq_* : Limits of quantification. Defaults are values for analytical
    # lab INBO 2025


    if (var == "TOC") {

      assertthat::assert_that(
        !is.null(tc),
        msg = "Argument 'tc' must be provided when var = 'TOC'")

      if (is.null(tic) || is.na(tic)) {
        tic <- 0
      }

      toc_harm <- case_when(
        is.na(tc) ~ NA_real_,
        # TIC is 0, so TOC ≈ TC
        tic == 0 & tc < loq_tc ~ 0.5 * loq_tc,
        # General case when TIC is not 0
        (tc - tic) < (loq_tc - loq_tic) ~ 0.5 * (loq_tc - loq_tic),
        # Above LOQ
        TRUE ~ (tc - tic))

      return(toc_harm)

    } else if (var == "TIC") {

      assertthat::assert_that(
        !is.null(tic),
        msg = "Argument 'tic' must be provided when var = 'TIC'")

      tic_harm <- case_when(
        is.na(tic) ~ NA_real_,
        tic == 0 ~ 0,
        tic < loq_tic ~ 0.5 * loq_tic,
        TRUE ~ tic)

      return(tic_harm)

    } else if (var %in% c("TN", "TC", "HWC")) {

      if (var == "TN") {

        assertthat::assert_that(
          !is.null(tn),
          msg = "Argument 'tn' must be provided when var = 'TN'")

        value <- tn
        loq_value <- loq_tn

      } else if (var == "TC") {

        assertthat::assert_that(
          !is.null(tc),
          msg = "Argument 'tc' must be provided when var = 'TC'")

        value <- tc
        loq_value <- loq_tc

      } else if (var == "HWC") {

        assertthat::assert_that(
          !is.null(hwc),
          msg = "Argument 'hwc' must be provided when var = 'HWC'")

        value <- hwc
        loq_value <- loq_hwc

      }

      assertthat::assert_that(!is.null(value))

      value_harm <- case_when(
        is.na(value) ~ NA_real_,
        value < loq_value ~ 0.5 * loq_value,
        TRUE ~ value)

      return(value_harm)
    }
  }



  # Lab measurement uncertainty

  # Create function compute_uncertainty

  compute_uncertainty <- function(var, tc = NULL, tic = NULL, tn = NULL,
                                  urel_tc = 0.09,
                                  urel_tic = 0.106,
                                  urel_tn = 0.06) {

    # This function calculates the absolute uncertainty of a certain
    # parameter value

    # var: "TOC", "TIC", "TC" or "TN"
    # tc, tic, tn: numeric vectors with total C, total inorganic C or
    # total N values
    # urel_* : relative expanded uncertainties (fractions). Defaults are
    # uncertainty in lab analyses ("Meetonzekerheid") calculated by INBO BMK.

    if (var == "TOC") {

      assertthat::assert_that(
        !is.null(tc),
        msg = "Argument 'tc' must be provided when var = 'TOC'")

      if (is.null(tic) || is.na(tic)) {
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

      if (var == "TN") {

        assertthat::assert_that(
          !is.null(tn),
          msg = "Argument 'tn' must be provided when var = 'TN'")

        value <- tn
        urel <- urel_tn

      } else if (var == "TC") {

        assertthat::assert_that(
          !is.null(tc),
          msg = "Argument 'tc' must be provided when var = 'TC'")

        value <- tc
        urel <- urel_tc

      } else if (var == "TIC") {

        assertthat::assert_that(
          !is.null(tic),
          msg = "Argument 'tic' must be provided when var = 'TIC'")

        value <- tic
        urel <- urel_tic

      }

      u_var <- (urel * value)

      return(u_var)
    }
  }


  # Final uncertainty ranges lab

  # Create function add_lab_uncertainty

  add_lab_uncertainty <- function(var, tc = NULL, tic = NULL, tn = NULL,
                                  loq_tc = 0.5, # g C kg-1 DS
                                  loq_tic = 0.1, # g C kg-1 DS
                                  loq_tn = 0.1) { # g N kg-1 DS

    # This function returns the minimum and the maximum of the uncertainty
    # interval, taking the LOQ into account (uncertainty range for below-LOQ
    # values goes from 0 to LOQ)

    # var: "TOC", "TIC", "TC" or "TN"
    # tc, tic, tn: numeric vectors with total C, total inorganic C or
    # total N values
    # loq_* : Limits of quantification. Defaults are values for analytical
    # lab INBO 2025

    var_long <- switch(var,
                       TOC = "c_organic_total",
                       TC  = "c_total",
                       TIC = "c_inorganic_total",
                       TN  = "n_total")

    u_var <- compute_uncertainty(var = var,
                                 tc = tc,
                                 tic = tic,
                                 tn = tn)

    if (var == "TOC") {

      assertthat::assert_that(
        !is.null(tc),
        msg = "Argument 'tc' must be provided when var = 'TOC'")

      if (is.null(tic) || is.na(tic)) {
        tic <- 0
      }

      var_min <- case_when(
        is.na(tc) ~ NA_real_,
        # TIC is 0, so TOC ≈ TC
        tic == 0 & tc < loq_tc ~ 0,
        # General case when TIC is not 0
        (tc - tic) < (loq_tc - loq_tic) ~ 0,
        # Above LOQ
        TRUE ~ max(c((tc - tic) - u_var, 0)))

      var_max <- case_when(
        is.na(tc) ~ NA_real_,
        # Below-LOQ for TIC is 0, so TOC ≈ TC
        tic == 0 & tc < loq_tc ~ loq_tc,
        # Below-LOQ for general case when TIC is not 0
        (tc - tic) < (loq_tc - loq_tic) ~ (loq_tc - loq_tic),
        # Above LOQ
        TRUE ~ min(c((tc - tic) + u_var,
                     1E3))) # Values cannot be higher than 1E3 since reported
                            # in g kg-1

      # End of "TOC"

    } else if (var == "TIC") {

      assertthat::assert_that(
        !is.null(tic),
        msg = "Argument 'tic' must be provided when var = 'TIC'")

      var_min <- case_when(
        is.na(tic) ~ NA_real_,
        tic == 0 ~ 0,
        tic < loq_tic ~ 0,
        TRUE ~ pmax(tic - u_var, 0))

      var_max <- case_when(
        is.na(tic) ~ NA_real_,
        tic == 0 ~ 0,
        tic < loq_tic ~ loq_tic,
        TRUE ~ pmin(tic + u_var, 1E3))

      # End of "TIC"

    } else if (var %in% c("TN", "TC")) {

      if (var == "TN") {

        assertthat::assert_that(
          !is.null(tn),
          msg = "Argument 'tn' must be provided when var = 'TN'")

        value <- tn
        loq_value <- loq_tn
      }

      if (var == "TC") {

        assertthat::assert_that(
          !is.null(tc),
          msg = "Argument 'tc' must be provided when var = 'TC'")

        value <- tc
        loq_value <- loq_tc
      }

      var_min <- case_when(
        is.na(value) ~ NA_real_,
        value < loq_value ~ 0,
        TRUE ~ max(c(value - u_var, 0)))

      var_max <- case_when(
        is.na(value) ~ NA_real_,
        value < loq_value ~ loq_value,
        TRUE ~ min(c(value + u_var, 1E3)))

    } # End of "TN" or "TC"

    var_min <- round(var_min, 2)
    var_max <- round(var_max, 2)

    return(
      setNames(
        list(var_min, var_max),
        c(paste0(var_long, "_min"), paste0(var_long, "_max"))))
  }








  # Compile all RFs ----

  data_lab <- NULL
  rfs_no_results <- NULL

  for (rf_i in vec_rf) {

    # Lab data in long format:

    # Samples from test sampling 2024
    # data_lab_i <- read_lims_data(connection = connection,
    #                              project = c("V-24V006-01"),
    #                              sql_template = "default",
    #                              show_query = FALSE)

    data_lab_i <- suppressMessages(
      read_lims_data(connection = connection,
                     project = c(rf_i),
                     sql_template = "default",
                     show_query = FALSE))

    if (nrow(data_lab_i) == 0 ||
        # Or if TC has not been reported yet
        !any(grepl("C_N_ANAL_V__T.C.DS", data_lab_i$Sleutel))) {

      rfs_no_results <- c(rfs_no_results, rf_i)

      next
    }

    # Did any extra analyses take place?

    analyses_done <- unique(data_lab_i$LimsAnalyseNaam)

    analyses_expected <- c(
      "TEXTUUR_LD_LS13320_V",
      "TEXTUUR_DEST_SP10_V",
      "DS_OVEN_105_ALQ",
      "DS_OVEN_105",
      "C_TIC_ANALYSER",
      "C_N_ANAL_V",
      "PHCACL2_VOL_SP2000_V",
      "C_HWC_V" # Hot water carbon - at first only for Belgian mineral samples
      )

    extra_analyses <-
      analyses_done[which(!analyses_done %in% analyses_expected)]

    if (!identical(extra_analyses, character(0))) {
      cat(paste0("Extra analyses for RF '", rf_i, "': ",
                 paste(extra_analyses, collapse = ", "), "\n"))
    }

    # Remove rows from duplicate samples (10 % of the samples is measured
    # twice for internal quality control of the analytical lab)

    samples_dup <- data_lab_i %>%
      filter(grepl("DUP", ProductGrade)) %>%
      distinct(OrigineelStaal) %>%
      pull(OrigineelStaal)

    datax_lab_i <- lims_report_xtab(data_lab_i)

    datax_lab_i <- datax_lab_i %>%
      # Convert dots in columns that look like numbers to commas.
      # Automatically detect and convert columns that contain only
      # numeric-convertible values to numeric class
      mutate(across(
        where(~ is.character(.) &&
                all(
                  is.na(.) |
                    grepl("^[0-9.,eE+-]+$", .)
                )),
        ~ as.numeric(gsub(",", ".", .))
      )) %>%
      # Remove samples from duplicate analysis
      filter(!OrigineelStaal %in% samples_dup)

    # Sometimes, there are multiple columns for a certain parameter,
    # which are differentiated by the suffixes "__x__y".
    # x: test replicate
    # y: result replicate
    # If multiple valid values are present for the same parameter,
    # the value from the highest test replicate (first number = x)
    # is in principle retained. Normally, only one result is reported per test,
    # so the result replicate (second number in "__x__y") should usually be
    # the same (0 or 1).

    # Merge columns:

    # Original column names without replicate suffixes like "__1__1"
    col_names <- str_remove(names(datax_lab_i), "__[0-9]+__[0-9]+$")

    # Parameters with multiple columns
    dup_pars <- unique(col_names[duplicated(col_names)])

    # Original names of the columns that are duplicated
    cols_dup_orig <- names(datax_lab_i)[which(col_names %in% dup_pars)]

    # If there are any duplicated columns
    if (!identical(dup_pars, character(0))) {

      alerts <- list()

      for (par in dup_pars) {

        # Columns belonging to this parameter
        cols_par <- names(datax_lab_i)[which(col_names == par)]

        # Extract subset
        tmp <- datax_lab_i %>%
          select(all_of(cols_par))

        # Rowwise unique non-NA values
        vals_unique <- apply(tmp, 1, function(x) {
          unique(na.omit(x))
        })

        # Detect inconsistent rows
        inconsistent <- lengths(vals_unique) > 1

        if (any(inconsistent)) {

          alerts[[par]] <- which(inconsistent)

          message(
            "Inconsistent values detected for parameter: '",
            par, "'\nin RF: '", rf_i,
            "'\nin sample(s):\n",
            paste(datax_lab_i$ExternSampleID[which(inconsistent)],
                  collapse = ",\n")
          )
        }

        # Collapse to one value per row
        datax_lab_i[[par]] <- apply(tmp, 1, function(x) {

          # Keep non-NA values
          keep <- !is.na(x)
          vals <- x[keep]

          # No values
          if (length(vals) == 0) {
            return(NA_real_)
          }

          # Only one unique value
          if (length(unique(vals)) == 1) {
            return(vals[1])
          }

          # If multiple different non-NA values for the given parameter x sample
          # "__x__y":
          # → use value for the highest x
          # → if x is tied, use value for the highest y

          # Corresponding column names
          cols_keep <- names(x)[keep]

          # Extract x and y from "__x__y"
          suffixes <- str_match(
            cols_keep,
            "__([0-9]+)__([0-9]+)$"
          )

          # Convert to numeric
          x_rank <- as.numeric(suffixes[, 2])
          y_rank <- as.numeric(suffixes[, 3])

          # Order by highest x, then highest y
          ord <- order(x_rank, y_rank, decreasing = TRUE)

          # Return preferred value
          vals[ord[1]]

        })

      } # End of loop across duplicated parameters

      # Remove original replicated columns
      datax_lab_i <- datax_lab_i %>%
        select(
          -any_of(cols_dup_orig))

    } # End of "if there are any duplicated columns"


    # Continue data preparations

    datax_lab_i <- datax_lab_i %>%
      # Rename the (non-duplicated) column names in order to ignore the
      # lab's test and result replicates in the column names
      # (i.e., remove trailing patterns of "__<number>__<number>")
      rename_with(~ gsub("__\\d+__\\d+$", "", .x)) %>%
      mutate(
        # if TIC col doesn't exist, create it and fill
        "C_TIC_ANALYSER__T.IC.DS" =
        if (!"C_TIC_ANALYSER__T.IC.DS" %in% names(.)) NA_real_ else
          C_TIC_ANALYSER__T.IC.DS,
        # Same for texture columns (missing for forest floor)
        "TEXTUUR_LD_LS13320_V__KLEI.04.6µm" =
          if (!"TEXTUUR_LD_LS13320_V__KLEI.04.6µm" %in% names(.)) NA_real_ else
            TEXTUUR_LD_LS13320_V__KLEI.04.6µm,
        "TEXTUUR_LD_LS13320_V__LEEM.6.63µm" =
          if (!"TEXTUUR_LD_LS13320_V__LEEM.6.63µm" %in% names(.)) NA_real_ else
            TEXTUUR_LD_LS13320_V__LEEM.6.63µm,
        "TEXTUUR_LD_LS13320_V__ZAND.63.2000µm" =
          if (
            !"TEXTUUR_LD_LS13320_V__ZAND.63.2000µm" %in% names(.)) NA_real_ else
              TEXTUUR_LD_LS13320_V__ZAND.63.2000µm,
        # Hot water carbon
        "C_HWC_V__HWC.OC.DS" =
          if (!"C_HWC_V__HWC.OC.DS" %in% names(.)) NA_real_ else
            C_HWC_V__HWC.OC.DS,
        ) %>%
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
      rowwise() %>%
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
        c_hwc =
          round(harmonise_below_loqs("HWC",
                                     hwc = C_HWC_V__HWC.OC.DS), 2)
        ) %>%
      ungroup() %>%
      mutate(
        ph_cacl2 = round(PHCACL2_VOL_SP2000_V__pH.CaCl2.20, 2),
        # IMPORTANT: the boundaries for clay, silt, sand according to laser
        # diffraction possibly need to be revised!!!
        clay = round(TEXTUUR_LD_LS13320_V__KLEI.04.6µm, 1),
        silt = round(TEXTUUR_LD_LS13320_V__LEEM.6.63µm, 1),
        sand = round(TEXTUUR_LD_LS13320_V__ZAND.63.2000µm, 1)) %>%
      rename(sample_id = ExternSampleID) %>%
      mutate(rf_id = rf_i) %>%
      # Sometimes there are separate texture fractions, but we do not need
      # those:
      # "TEXTUUR_LD_LS13320_V__FRAC.04.2µm"
      # "TEXTUUR_LD_LS13320_V__FRAC.2.6µm"
      # "TEXTUUR_LD_LS13320_V__FRAC.6.50µm"
      # "TEXTUUR_LD_LS13320_V__FRAC.50.63µm"
      # "TEXTUUR_LD_LS13320_V__FRAC.63.2000µm"
      select(
        sample_id, rf_id,
        c_organic_total, c_organic_total_min, c_organic_total_max,
        n_total, n_total_min, n_total_max,
        c_inorganic_total, c_inorganic_total_min, c_inorganic_total_max,
        ph_cacl2,
        clay,
        silt,
        sand,
        c_hwc)

    data_lab <- bind_rows(data_lab,
                          datax_lab_i)

  } # End of loop across RFs


  if (!is.null(rfs_no_results)) {

    cat("\n\nNone of the lab results are ready for the following RFs:\n")
    cat(rfs_no_results)
    cat("\n\n")

  }

return(as_tibble(data_lab))

}
