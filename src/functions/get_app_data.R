
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
      path_app_data <-  "./data/raw_data/app_data/Field-data/Field-data/"
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
             str_to_lower()) %>%
    filter(!is.na(GlobalID)) %>%
    # Filter out test survey from INBO 2024
    filter(!(team == "INBO" & grepl("2024", date_time)))


  # Import and combine the other files ----

  app_files <- excel_files[which(!grepl("identification_sites", excel_files))]

  # Any other xlsx files that are manually checked and that are not output data
  # from the Survey123 app (e.g. forest floor data)

  files_to_exclude <- c(
    paste0(path_app_data,
           "VUK-Czechia-Hungary/WP3_sites/Beskydy-Mts/",
           "WILDCARD-WP3-CZ-VUK-Beskydy soils.xlsx"),
    paste0(path_app_data,
           "VUK-Czechia-Hungary/WP3_sites/Bohemian-Karst/",
           "WILDCARD-WP3-CZ-VUK-Cesky Kras (Bohemian Karst) soils.xlsx"),
    paste0(path_app_data,
           "VUK-Czechia-Hungary/WP3_sites/Hungary-Kiskunsag/",
           "WILDCARD-WP3-HU-VUK-Kiskusag soils.xlsx"),
    paste0(path_app_data,
           "VUK-Czechia-Hungary/WP3_sites/Hungary-Tokaj/",
           "WILDCARD-WP3-HU-VUK-Tokaj soils.xlsx"),
    paste0(path_app_data,
           "VUK-Czechia-Hungary/WP3_sites/Sumava-Dry/",
           "WILDCARD-WP3-CZ-VUK-Sumava_dry_soils.xlsx"),
    paste0(path_app_data,
           "VUK-Czechia-Hungary/WP3_sites/Sumava-Wet/",
           "WILDCARD-WP3-CZ-VUK-Sumava-wet-soils.xlsx"))

  app_files <- app_files[which(!app_files %in% files_to_exclude)]


  app_data_wide <- map_dfr(seq_along(app_files), function(i) {

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
        y = any_of(c("y", "Y_WGS", "...242")),
        globalid = any_of(c("globalid", "GlobalID"))) %>%
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
        else y,
        humus_form =
          if (!"humus_form" %in% names(.)) NA_character_
        else humus_form,
        humus_form1 =
          if (!"humus_form1" %in% names(.)) NA_character_
        else humus_form1,
        humus_form2 =
          if (!"humus_form2" %in% names(.)) NA_character_
        else humus_form2,
        surface_frame =
          if (!"surface_frame" %in% names(.)) NA_real_ # .0625
        else surface_frame,
        surface_ff_frame =
          if (!"Surface area of forest floor frame used (m2)" %in%
              names(.)) NA_real_
        else `Surface area of forest floor frame used (m2)`,
        hor_accuracy_man =
          if (!"hor_accuracy_man" %in% names(.)) NA_real_
        else hor_accuracy_man,
        no_undist_m36 =
          if (!"Number of taken undisturbed samples from M36 layer" %in%
              names(.)) NA_real_
        else `Number of taken undisturbed samples from M36 layer`,
        no_undist_m61 =
          if (!"Number of taken undisturbed samples from M61 layer" %in%
              names(.)) NA_real_
        else `Number of taken undisturbed samples from M61 layer`,
        P1_num_M36_undist_samples =
          if (!"P1_num_M36_undist_samples" %in%
              names(.)) NA_real_
        else P1_num_M36_undist_samples,
        P1_num_M61_undist_samples =
          if (!"P1_num_M61_undist_samples" %in%
              names(.)) NA_real_
        else P1_num_M61_undist_samples
        ) %>%
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
        hor_accuracy = coalesce(as.numeric(hor_accuracy_aut),
                                as.numeric(hor_accuracy_man)),
        humus_form = coalesce(humus_form,
                              humus_form1,
                              humus_form2),
        P1_num_M36_undist_samples = coalesce(P1_num_M36_undist_samples,
                                             no_undist_m36),
        P1_num_M61_undist_samples = coalesce(P1_num_M36_undist_samples,
                                             no_undist_m61),
        surface_frame = coalesce(as.numeric(surface_frame),
                                 as.numeric(surface_ff_frame))
        ) %>%
      select(-any_of(c("hor_accuracy_aut", "hor_accuracy_man",
                       "no_undist_m36", "no_undist_m61",
                       "Number of taken undisturbed samples from M36 layer",
                       "Number of taken undisturbed samples from M61 layer",
                       "surface_ff_frame",
                       "Surface area of forest floor frame used (m2)"))
             ) %>%
      # GlobalID should not have curly brackets and capital letters
      # To make the link with the identification_df
      mutate(globalid = str_remove_all(globalid, "[\\{\\}]") %>%
               str_to_lower()) %>%
      mutate(survey_date = as.Date(parse_date(date_time)),
             survey_month = as.integer(format(survey_date, "%M")),
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


  # INCONSISTENCY 1 ----

  rule <- "Data expected to be numeric should be numeric."
  rule_id <- "1"

  name_export <- "inconsistencies_app_numeric"

  source("./src/functions/get_attribute_catalogue_app.R")
  attribute_catalogue <- get_attribute_catalogue_app()

  # These columns should be numeric:
  columns_to_check <- c(
    "latitude", "longitude", "hor_accuracy",
    "surface_frame", "vol_ring", "air_temperature", "depth_cultivation_cm",
    "slope_deg",
    grep("(_thickness$|_tamass$)", names(app_data_wide), value = TRUE),
    grep("(coaf_2mm$|coaf_50mm$|_depth_bedrock$)", names(app_data_wide),
         value = TRUE),
    grep("(_flamm$|_carbon$)", names(app_data_wide), value = TRUE),
    grep("(_eDNA1$|_eDNA2$|_edna1$|_edna2$|_back_up$|^m\\d{2}_bulk_den$)",
         names(app_data_wide), value = TRUE))

  # Dataframe to collect problems
  inconsistencies_app_numeric <- data.frame(
    source = character(0),
    team = character(0),
    plot_code_app = character(0),
    res_id_inst = character(0),
    globalid = character(0),
    parameter = character(0),
    sample_code = character(0),
    value = character(0),
    inconsistency_reason = character(0),
    unique_inconsistency = logical(0),
    rule_id = character(0))

  # Loop through rows and columns
  for (i in seq_len(nrow(app_data_wide))) {
    for (col in columns_to_check) {
      val <- app_data_wide[[col]][i]

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
              source = "Survey123 app",
              team = app_data_wide$institute[i],
              plot_code_app = app_data_wide$code[i],
              res_id_inst = app_data_wide$res_id_inst[i],
              globalid = app_data_wide$globalid[i],
              sample_code = NA,
              parameter = col,
              value = as.character(val),
              inconsistency_reason = rule,
              unique_inconsistency = TRUE,
              rule_id = rule_id))
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

  assign(name_export, inconsistencies_app_numeric, envir = .GlobalEnv)
  cat(paste0("Object '", name_export, "' is imported in global environment.\n"))




  # Improve columns, data classes and harmonised plot keys ----

  layers <- c("OL", "OFH", "M01", "M13", "M36", "M61")

  app_data_wide <- app_data_wide %>%
    select(-x, -y, ) %>%
    relocate(hor_accuracy, .after = longitude) %>%
    relocate(wrb_qualifier_1, wrb_qualifier_suppl,
             .after = wrb_ref_soil_group) %>%
    # Manually fix one inconsistency following feedback WSL
    mutate(
      P3_ofh_thickness = case_when(
        code == "CH-WSL-30-NA-KF 2" ~ "2.5",
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
    ))

    empty_cols <- names(app_data_wide)[colSums(!is.na(app_data_wide)) == 0]

    app_data_wide <- app_data_wide %>%
      select(-any_of(empty_cols))







  app_data_wide <- app_data_wide %>%
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
        sep = "__"),
      # Simple version of a plot code (unique code per plot across WILDCARD)
      # by taking composed_site_id and only adding the plot_id for
      # Bialowieza
      plot_code_simple = ifelse(
        composed_site_id == "WULS__1__Bialowieza National Park__NA",
        case_when(
          grepl("Transect V", plot_id_harmonized, ignore.case = TRUE) ~
            paste(composed_site_id, "Transect V", sep = "__"),
          grepl("Transect IV", plot_id_harmonized, ignore.case = TRUE) ~
            paste(composed_site_id, "Transect IV", sep = "__"),
          grepl("Transect III", plot_id_harmonized, ignore.case = TRUE) ~
            paste(composed_site_id, "Transect III", sep = "__"),
          grepl("Transect II", plot_id_harmonized, ignore.case = TRUE) ~
            paste(composed_site_id, "Transect II", sep = "__")),
        composed_site_id)) %>%
    relocate(
      composed_site_id,
      plot_code,
      plot_code_simple,
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


  # Deduplicate the app data
  # (in the past, some plots were submitted multiple times in the app data.
  #  Note: duplicated records were removed in advancefrom identification_df
  #  following instructions by the partners, and should therefore be absent)

  vec_dupl <- app_data_wide$plot_code_simple[
    duplicated(app_data_wide$plot_code_simple)]

  if (!identical(vec_dupl, character(0))) {

    message("Duplicated plots (records) present in app data!\n")

    app_data_wide <- bind_rows(
      # Records that are fine (not duplicated)
      app_data_wide %>%
        filter(!plot_code_simple %in% vec_dupl),
      # Duplicated records
      app_data_wide %>%
        # restrict to duplicates only
        filter(plot_code_simple %in% vec_dupl) %>%
        group_by(plot_code_simple) %>%
        # Filter for the most recent end_survey (time on which the survey was
        # submitted)
        slice_max(end_survey, with_ties = TRUE) %>%
        # If this still leads to duplicated records:
        group_modify(~ {
          df <- .x
          # Rule 1: if only one row after filtering by max end_survey
          if (nrow(df) == 1) return(df)
          # Rule 2a: if all rows identical, keep first
          if (nrow(unique(df)) == 1) return(df[1, , drop = FALSE])

          # Rule 2b: choose row with max number of non-NA values
          non_na_counts <- rowSums(!is.na(df))
          df <- df[non_na_counts == max(non_na_counts), drop = FALSE]

          df
        }) %>%
        ungroup())

  }



  return(app_data_wide)



}
