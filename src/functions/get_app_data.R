
get_app_data <- function(path = NULL) {

  # Compiles the data from the survey123 app into one dataframe
  # (horizontal format), after downloading them into your local repository.
  # It also harmonises/adds plot identification fields (e.g.
  # composed_site_id)
  # It is also the purpose that any corrections in the Survey123 app data are
  # implemented in this script.


  stopifnot(require("tidyverse"),
          require("googledrive"),
          require("googlesheets4"),
          require("readxl"),
          require("purrr"),
          require("parsedate"))

  # First step before running the code:
  # Download the data from the WILDCARD SharePoint to your local folders


  # Root folder of Survey123 app data ----

  path_app_data <- "./data/raw_data/app_data/Field-data/"

  if (is.null(path)) {

    assertthat::assert_that(
      file.exists(path_app_data),
      msg = paste0("Default path (Field-data) with Survey123 app data does ",
                   "not exist. ",
                   "Specify root folder path as input."))

    dirs <- list.dirs(path_app_data, recursive = FALSE, full.names = TRUE)

    if (any( "./data/raw_data/app_data/Field-data/Field-data" %in% dirs)) {
      path_app_data <-  "./data/raw_data/app_data/Field-data/Field-data"
    }

  } else {

    assertthat::assert_that(
      file.exists(path),
      msg = "Path does not exist.")

    path_app_data <- path

  }


  # List all Excel files in the folder

  excel_files <- list.files(
    path = path_app_data,
    pattern = "\\.xlsx?$",
    recursive = TRUE,
    full.names = TRUE)

  # Overview table ----
  # This is the overview table where the link of each Survey123 app submission
  # with the WPs and composed_site_id is manually established

  # TO DO:
  # Add VUK WP3 data sampled in 2024 to the table

  identification_df_path <-
    excel_files[which(grepl("identification_sites", excel_files))]

  assertthat::assert_that(file.exists(identification_df_path))

  identification_df <-
    suppressMessages(read_excel(identification_df_path)) %>%
    # Remove the empty column
    {
      df <- .
      if ("...10" %in% names(df) && all(is.na(df$`...10`))) {
        select(df, -`...10`)
      } else {
        df
      }
    } %>%
    rename(wp = `WP2 or WP3 site`) %>%
    # GlobalID sometimes has curly brackets and capital letters, sometimes not
    # To make the link with the identification_df, remove curly brackets
    # and capital letters
    mutate(GlobalID = str_remove_all(GlobalID, "[\\{\\}]") %>%
             str_to_lower())


  # Import and combine the other files ----

  app_files <- excel_files[which(!grepl("identification_sites", excel_files))]

  # Any other xlsx files that are manually checked and that are not output data
  # from the Survey123 app (e.g. forest floor data)

  files_to_exclude <- c(
    paste0("./data/raw_data/app_data/Field-data/Field-data/",
           "VUK-Czechia-Hungary/WP3_sites/Beskydy-Mts/",
           "WILDCARD-WP3-CZ-VUK-Beskydy soils.xlsx"),
    paste0("./data/raw_data/app_data/Field-data/Field-data/",
           "VUK-Czechia-Hungary/WP3_sites/Bohemian-Karst/",
           "WILDCARD-WP3-CZ-VUK-Cesky Kras (Bohemian Karst) soils.xlsx"),
    paste0("./data/raw_data/app_data/Field-data/Field-data/",
           "VUK-Czechia-Hungary/WP3_sites/Hungary-Kiskunsag/",
           "WILDCARD-WP3-HU-VUK-Kiskusag soils.xlsx"),
    paste0("./data/raw_data/app_data/Field-data/Field-data/",
           "VUK-Czechia-Hungary/WP3_sites/Hungary-Tokaj/",
           "WILDCARD-WP3-HU-VUK-Tokaj soils.xlsx"),
    paste0("./data/raw_data/app_data/Field-data/Field-data/",
           "VUK-Czechia-Hungary/WP3_sites/Sumava-Dry/",
           "WILDCARD-WP3-CZ-VUK-Sumava_dry_soils.xlsx"),
    paste0("./data/raw_data/app_data/Field-data/Field-data/",
           "VUK-Czechia-Hungary/WP3_sites/Sumava-Wet/",
           "WILDCARD-WP3-CZ-VUK-Sumava-wet-soils.xlsx")
  )

  app_files <- app_files[which(!app_files %in% files_to_exclude)]


  app_data <- map_dfr(seq_along(app_files), function(i) {

    suppressMessages(read_excel(app_files[i])) %>%
      rename(
        country_code = country,
        institute = team,
        res_id_inst = region,
        wrb_ref_soil_group = ref_soil_grp,
        wrb_qualifier_1 = any_of(c("principal_q_RG", "principal_q",
                                   "principal_q_LV", "principal_q_GL")),
        wrb_qualifier_suppl = any_of(c("suppl_q", "suppl_q_GL",
                                       "suppl_q_LV")),
        humus_form = any_of(c("P1_humus_form", "humus_form")),
        date_time = any_of(c("date_time (UTC)", "date_time")),
        x = any_of(c("x", "X_WGS", "...241")),
        y = any_of(c("y", "Y_WGS", "...242"))) %>%
      mutate(
        # Create column only if missing
        air_temperature =
          if (!"air_temperature" %in% names(.)) NA_real_
          else air_temperature,
        x =
          if (!"x" %in% names(.)) NA_real_
        else x,
        y =
          if (!"y" %in% names(.)) NA_real_
        else y) %>%
      # UTC time zone (Coordinated Universal Time)
      mutate(
        date_time =
          parsedate::parse_iso_8601(parsedate::parse_date(date_time)),
        start_survey =
          parsedate::parse_iso_8601(parsedate::parse_date(start_survey)),
        end_survey =
          parsedate::parse_iso_8601(parsedate::parse_date(end_survey)),
        CreationDate =
          parsedate::parse_iso_8601(parsedate::parse_date(CreationDate)),
        EditDate =
          parsedate::parse_iso_8601(parsedate::parse_date(EditDate))) %>%
      mutate(
        latitude = coalesce(as.character(latitude),
                            as.character(y)),
        longitude = coalesce(as.character(longitude),
                             as.character(x)),
        latitude = as.numeric(str_replace(latitude, ",", ".")),
        longitude = as.numeric(str_replace(longitude, ",", ".")),
        hor_accuracy = coalesce(hor_accuracy_aut,
                                hor_accuracy_man)) %>%
      select(-hor_accuracy_aut, -hor_accuracy_man) %>%
      mutate(survey_date = as.Date(parse_date(date_time)),
             survey_year = as.integer(format(survey_date, "%Y"))) %>%
      mutate(across(c(
        plot_id, site_id, res_id_inst, surface_frame, latitude, longitude,
        gen_observtn, x, y, hor_accuracy, slope_deg,
        matches("(_thickness$|m\\d{2}dist$|_tamass$|_flamm$|_carbon$)"),
        matches("(_eDNA1$|_eDNA2$|_back_up$|_bulk_den$)"),
        matches("(No_point_for_eDNA$|remarks)")),
        as.character
      ))
  })


  # TO DO: At this point, you should first manually correct any inconsistencies,
  # e.g., after receiving corrections from the partner

  # TO DO: use additional meteodata and eDNA sample masses for the additional
  # eDNA samples that were collected by NW-FVA (after their freezer broke down)


  # PIR (inconsistency) ----

  rule <- "Data expected to be numeric should be numeric."

  name_export <- "inconsistencies_app_numeric"

  source("./src/functions/get_attribute_catalogue_app.R")
  attribute_catalogue <- get_attribute_catalogue_app()

  # These columns should be numeric:
  columns_to_check <- c(
    "latitude", "longitude", "hor_accuracy",
    "surface_frame", "vol_ring", "air_temperature", "depth_cultivation_cm",
    "slope_deg",
    grep("(_thickness$|_tamass$)", names(app_data), value = TRUE),
    grep("(coaf_2mm$|coaf_50mm$|_depth_bedrock$)", names(app_data),
         value = TRUE),
    grep("(_flamm$|_carbon$)", names(app_data), value = TRUE),
    grep("(_eDNA1$|_eDNA2$|_edna1$|_edna2$|_back_up$|^m\\d{2}_bulk_den$)",
         names(app_data), value = TRUE))

  # Dataframe to collect problems
  inconsistencies_app_numeric <- data.frame(
    team = character(0),
    plot_code_app = character(0),
    res_id_inst = character(0),
    globalid = character(0),
    parameter = character(0),
    sample_code = character(0),
    value = character(0),
    inconsistency_reason = character(0))

  # Loop through rows and columns
  for (i in seq_len(nrow(app_data))) {
    for (col in columns_to_check) {
      val <- app_data[[col]][i]

      if (!is.na(val)) {
        # Convert comma decimal to dot
        val_clean <- gsub(",", ".", as.character(val))

        # Try converting to numeric
        # Returns NULL if it doesn't give problems
        result <- tryCatch({
          as.numeric(val_clean)
          NULL  # no error
        }, warning = function(w) {
          return(val_clean)
        }, error = function(e) {
          return(val_clean)
        })

        if (!is.null(result)) {
          inconsistencies_app_numeric <-
            rbind(inconsistencies_app_numeric, data.frame(
              team = app_data$institute[i],
              plot_code_app = app_data$code[i],
              res_id_inst = app_data$res_id_inst[i],
              globalid = app_data$globalid[i],
              sample_code = NA,
              parameter = col,
              value = val,
              inconsistency_reason = rule,
              stringsAsFactors = FALSE
            ))
        }
      }
    }
  }

  inconsistencies_app_numeric <- inconsistencies_app_numeric %>%
    left_join(attribute_catalogue %>%
                rename(parameter_description = descr,
                       parameter_unit = unit),
              by = join_by("parameter" == "column_name")) %>%
    relocate(parameter_description, parameter_unit, .after = parameter)

  assign(name_export, inconsistencies_app_numeric)
  cat(paste0("Object '", name_export, "' is imported in global environment.\n"))




  # Improve columns, data classes and harmonised plot keys ----

  layers <- c("OL", "OFH", "M01", "M13", "M36", "M61")

  app_data <- app_data %>%
    select(-x, -y, ) %>%
    relocate(hor_accuracy, .after = longitude) %>%
    relocate(wrb_qualifier_1, wrb_qualifier_suppl,
             .after = wrb_ref_soil_group) %>%
    # Manually fix one inconsistency that was detected on 2025-07-31
    # TO DO: update this with the correct value from the partner (WSL)
    # Or actually, better implement this earlier in this script (right after
    # app_data is first imported)
    mutate(
      P3_ofh_thickness = case_when(
        code == "CH-WSL-30-NA-KF 2" ~ "2",
        TRUE ~ P3_ofh_thickness)) %>%
    # Convert columns that should be numeric to numerics
    mutate(across(c(
      latitude, longitude, hor_accuracy,
      surface_frame, vol_ring, air_temperature, depth_cultivation_cm,
      slope_deg,
      matches("(_thickness$|_tamass$)"),
      matches("(coaf_2mm$|coaf_50mm$|_depth_bedrock$)"),
      matches("(_flamm$|_carbon$)"),
      matches("(_eDNA1$|_eDNA2$|_edna1$|_edna2$)"),
      matches("(_back_up$|^m\\d{2}_bulk_den$)")),
      ~ as.numeric(gsub(",", ".", .x))
    )) %>%
    # Make sure that all layer codes ("M01", "m01" etc) are capitalised
    # (in the column names)
    # (only column names starting with this code_layer or
    # containing "_[code_layer]", i.e., "vol_ring" shouldn't be converted)
    rename_with(
      ~ str_replace_all(.,
                        paste0("(?<=^|_)", paste0(tolower(layers),
                                                  collapse = "|")),
                        toupper),
      matches(paste0("(^|_)", paste0(layers, collapse = "|"))))

    empty_cols <- names(app_data)[colSums(!is.na(app_data)) == 0]

    app_data <- app_data %>%
      select(-any_of(empty_cols)) %>%
      # GlobalID should not have curly brackets and capital letters
      # To make the link with the identification_df
      mutate(globalid = str_remove_all(globalid, "[\\{\\}]") %>%
               str_to_lower())







  app_data <- app_data %>%
    left_join(
      # Add harmonised plot identifiers
      identification_df %>%
        select(GlobalID,
               wp,
               contains("_harmonized")),
      by = join_by("globalid" == "GlobalID")) %>%
    mutate(
      composed_site_id = paste(
        team_harmonized,
        res_id_harmonized,
        region_harmonized,
        site_id_harmonized,
        sep = "__"),
      plot_code = paste(
        team_harmonized,
        res_id_harmonized,
        site_id_harmonized,
        plot_id_harmonized,
        sep = "__")) %>%
    relocate(
      composed_site_id,
      plot_code,
      wp,
      team_harmonized,
      res_id_harmonized,
      region_harmonized,
      site_id_harmonized,
      plot_id_harmonized,
      .before = objectid) %>%
    mutate(
      # Institute that does the soil sampling
      institute_sampling = case_when(
        team_harmonized %in% c("BFNP_EXTRA", "NPS") ~ "BFNP",
        team_harmonized == "LWF" &
          region_harmonized   == "Hoellbachgespreng" ~ "BFNP",
        team_harmonized == "DISAFA - UNITO" ~ "UNITO",
        team_harmonized == "FVA-BW" ~ "NWFVA",
        team_harmonized == "INCDS" ~ "UNITBV",
        team_harmonized %in% c("UNIUD/HSI", "UNIUD/HSI_EXTRA",
                               "UNIUD_EXTRA") ~ "UNIUD",
        team_harmonized %in% c("URK", "WULS") ~ "IBL",
        TRUE ~ team_harmonized)) %>%
    relocate(institute_sampling, .after = team_harmonized)


  return(app_data)



}
