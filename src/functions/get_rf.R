

# Overview of RFs with their scope and IDs for Google Drive

rf_overview <- data.frame(
  rf = c("V-25V006-01",
         "V-25V006-02",
         "V-25V006-03",
         "V-25V006-04",
         "V-25V006-05",
         "V-25V006-06",
         "V-25V006-07",
         "V-25V006-08",
         "V-25V006-09",
         "V-25V006-10",
         "V-25V006-11",
         "V-25V006-12",
         "V-25V006-13",
         "V-25V006-14",
         "V-25V006-15",
         "V-25V006-16"
         ),
  id = c(id_rf01,
         id_rf02,
         id_rf03,
         id_rf04,
         id_rf05,
         id_rf06,
         id_rf07,
         id_rf08,
         id_rf09,
         id_rf10,
         id_rf11,
         id_rf12,
         id_rf13,
         id_rf14,
         id_rf15,
         id_rf16
         ),
  partners = c("INBO",
               "INBO",
               "UNIUD|BFNP|LWF",
               "UNIUD|BFNP|LWF",
               "WSL",
               "WSL",
               "FVA-BW|NWFVA",
               "FVA-BW|NWFVA",
               "DISAFA - UNITO|IBER-BAS|AlberIT - UNIRC|UCPH|WR|BGD-NP",
               "DISAFA - UNITO|IBER-BAS|AlberIT - UNIRC|UCPH|WR|BGD-NP",
               "CULS",
               "CULS",
               "UL|UNITBV|INCDS|URK|WULS",
               "UL|UNITBV|INCDS|URK|WULS",
               "VUK",
               "VUK"
               ),
  samples = c("M01|M13|M36|M61",
              "OFH",
              "M01|M13|M36|M61",
              "OFH",
              "M01|M13|M36|M61",
              "OFH",
              "M01|M13|M36|M61",
              "OFH",
              "M01|M13|M36|M61",
              "OFH",
              "M01|M13|M36|M61",
              "OFH",
              "M01|M13|M36|M61",
              "OFH",
              "M01|M13|M36|M61",
              "OFH"
              ))

assign("rf_overview", rf_overview)



get_rf <- function() {

  # Get the RF tables in order to obtain the LIMS sample IDs

  stopifnot(require("tidyverse"),
            require("openxlsx"),
            require("parsedate"),
            require("googlesheets4"),
            require("googledrive"),
            require("assertthat"),
            require("inbolims"),
            require("ggpmisc"),
            require("patchwork"),
            require("parsedate"))

  # Function to read and clean one file

  read_rf <- function(file) {

    # STAALFORMULIER

    rf_staal <- suppressMessages(read_sheet(as_id(file$id),
                                 sheet = "STAALFORMULIER",
                                 skip = 2))[-1, ] %>%
      rename(
        lims_project_code = Project,
        lims_sample_id = `Labo-ID`,
        sample_id = `Veld-ID`) %>%
      mutate(
        institute_sampling = case_when(
          grepl("^BE-INBO-", sample_id) ~ "INBO",
          TRUE ~ `Uitvoerder bemonstering`)) %>%
      filter(sample_id != "VeldID?") %>%
      select(lims_project_code, lims_sample_id, sample_id, institute_sampling)

    # LABELFORMULIER

    rf_label <- suppressMessages(read_sheet(as_id(file$id),
                                            sheet = "LABELFORMULIER",
                                            skip = 1))[-1, ] %>%
      mutate(
        # Create column only if missing
        `Korte code` =
          if (!"Korte code" %in% names(.)) NA_character_
        else `Korte code`,
        `korte code2` =
          if (!"korte code" %in% names(.)) NA_character_
        else `korte code`,
        `...10` =
          if (!"...10" %in% names(.)) NA_character_
        else `...10`,
        `Veld-ID kort` =
          if (!"Veld-ID kort" %in% names(.)) NA_character_
        else `Veld-ID kort`) %>%
      mutate(
        lims_label_short = coalesce(`Korte code`,
                                    `korte code2`,
                                    `...10`,
                                    `Veld-ID kort`)) %>%
      rename(
        sample_id_labelformulier = `Veld-ID`,
        lims_sample_id = `Labo-ID`,
        lims_sample_id_2 = `...5` # Labocode 2 (ball-milled sample for CN)
        ) %>%
      filter(sample_id_labelformulier != "VeldID?") %>%
      select(sample_id_labelformulier, lims_sample_id,
             lims_sample_id_2, lims_label_short)

    assertthat::assert_that(nrow(rf_staal) == nrow(rf_label),
                            msg = paste0("Different nrow staalformulier vs ",
                                         "labelformulier: '", file$rf, "'"))

    assertthat::assert_that(
      all(rf_label$lims_sample_id %in% rf_staal$lims_sample_id) &&
        all(rf_staal$lims_sample_id %in% rf_label$lims_sample_id))

    rf <- rf_staal %>%
      left_join(rf_label,
                by = "lims_sample_id")

    assertthat::assert_that(
      # A small mistake in this RF
      file$rf == "V-25V006-10" ||
      all(rf$sample_id == rf$sample_id_labelformulier))

    rf <- rf %>%
      select(-sample_id_labelformulier)

    return(rf)
  }



  # Read and combine all files

  rf_all <- map_dfr(seq_len(nrow(rf_overview)), function(i) {
    read_rf(rf_overview[i, ])
  })




  return(rf_all)

}
