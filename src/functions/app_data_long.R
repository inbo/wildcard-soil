
#' Convert Survey123 app data from wide to long format
#'
#' Converts field data exported from the field Survey123 app from a wide format
#' (one record per plot with repeated columns for layers) to a long format
#' containing one record per soil layer.
#'
#' During the conversion, plot-level and layer-level datasets suitable for
#' further combination with other data sources are created and data corrections
#' can optionally be performed. Inconsistencies in the (corrected) data are
#' detected and compiled.
#'
#' @param app_data_wide A data frame containing the raw Survey123 data
#'   in wide format (typically one record per plot).
#'
#' @param apply_corrections Logical. If `TRUE` (default), predefined
#'   corrections to the raw data are applied during processing. If `FALSE`,
#'   the input data are converted without applying these corrections.
#'
#' @param create_inconsistency_report Logical. If `TRUE` (default), a
#'   report listing detected inconsistencies in the submitted data is
#'   created and returned. If `FALSE`, the conversion is performed silently
#'   and no report is generated.
#'
#' @return
#' A named list containing the processed datasets:
#'
#' \itemize{
#' \item `df_layers` – A data frame containing soil layer data in long format
#'       (one record per layer), including variables such as recorded masses,
#'       coarse fragment content, and other layer-specific data
#'
#' \item `df_plot` – A data frame containing plot-level information such as
#'       coordinates and site descriptors.
#'
#' \item `df_sampling_points` – A data frame containing sampling point-specific
#'       information.
#'
#' \item `inconsistencies` – *(optional)* A data frame listing detected
#'       inconsistencies or potential issues in the submitted data. This
#'       element is only included when `create_inconsistency_report = TRUE`.
#' }
#'
#' The returned objects can be accessed using `$`, e.g.:
#'
#' \preformatted{
#' res <- app_data_long(app_data_wide)
#'
#' df_layers <- res$df_layers
#' df_plot   <- res$df_plot
#' df_sampling_points = res$df_sampling_points
#' inconsistencies_app_data_long <- res$inconsistencies
#' }
#'
#' @examples
#' \dontrun{
#' res_app_data_long <- app_data_long(app_data_wide)
#' }
#'
#' @export

app_data_long <- function(app_data_wide,
                          apply_corrections = TRUE,
                          create_inconsistency_report = TRUE) {

  # This function converts app data from the wide format to the long format
  # (different records per layer)

  layers <- c("OL", "OFH", "M01", "M13", "M36", "M61")

  app_data <- app_data_wide %>%
    # Make sure that all layer codes ("M01", "m01" etc) are capitalised
    # (in the column names)
    # (only column names starting with this code_layer or
    # containing "_[code_layer]",
    # in other words, column "vol_ring", which contains "ol", shouldn't be
    # converted)
    rename_with(
      ~ str_replace_all(.,
                        paste0("(?<=^|_)", paste0(tolower(layers),
                                                  collapse = "|")),
                        toupper),
      matches(paste0("(^|_)", paste0(layers, collapse = "|"))))




  # 1. Summarise properties from disturbed samples across sampling points ----

  d_properties <- read.csv(paste0("./data/additional_data/",
                                  "dictionary_disturbed_properties.csv"),
                           sep = ";")

  d_gen_properties <- read.csv(paste0("./data/additional_data/",
                                      "dictionary_general_properties.csv"),
                               sep = ";")




  ## Layer-specific properties ----

  df_properties <- app_data %>%
    select(code,
           (contains("M01") | contains("M13") | contains("M36") |
              contains("M61")) &
             starts_with("P") &
             contains("dist") &
             # These columns do not contain properties
             !ends_with("_undist_samples")
             ) %>%
    mutate(across(matches("^P\\d+_M\\d+"), as.character)) %>%
    pivot_longer(
      cols = matches("^P\\d+_M\\d+"),
      names_to = c("sampling_point", "code_layer", "variable"),
      names_pattern = "^(P\\d+)_?(M\\d+)([a-z0-9_]+)$",
      values_to = "value"
    ) %>%
    # There is a problem with some codes (probably due to Excel)
    # They are expected in the format "10,60,30", but some values show
    # "10.6" (which should probably be "10,60")
    # Correct
    mutate(
      # First correct some very long values like "40.299999999999997"
      # (should be "40,30")
      value = if_else(
        str_length(value) > 15 & str_count(value, "\\.") == 1,
        suppressWarnings(str_replace(sprintf("%.2f", as.numeric(value)),
                                     "\\.", ",")),
        value),
      value = case_when(
        # If there's a dot with only 1 digit after it, add 0 and replace dot
        # with comma
        str_detect(value, "\\.\\d$") ~ str_replace(value, "\\.(\\d)$", ",\\10"),
        # If there's a dot with 2 digits after it, just replace dot with comma
        str_detect(value, "\\.\\d{2}$") ~ str_replace(value, "\\.", ","),
        # str_length(value) > 15 & str_count(value, "\\.") == 1 ~
        #   str_replace(sprintf("%.2f", as.numeric(value)), "\\.", ","),
        # Otherwise leave as is
        TRUE ~ value)) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    mutate(dist = strsplit(as.character(dist), ",")) %>%
    unnest(dist) %>%  # Unnest the list into individual rows
    mutate(dist = na_if(dist, "99")) %>%
    left_join(d_properties %>%
                rename(properties = property) %>%
                mutate(code = as.character(code)),
              by = join_by("dist" == "code")) %>%
    # Summarise all the properties (reported under the disturbed sampling)
    # across all sampling points
    # (more frequently reported data are first)
    group_by(code, code_layer) %>%
    reframe(
      # All sampling points ---
      dist = if (all(is.na(properties))) {
        NA_character_
      } else {
        tab <- table(na.omit(properties))
        tab <- sort(tab, decreasing = TRUE)
        paste0(names(tab), " (", as.integer(tab), ")") %>%
          paste(collapse = ", ")
      },
      # Only P1–P5 ---
      properties_five_pts = {
        props_five <- properties[sampling_point %in% paste0("P", 1:5)]
        if (length(props_five) == 0 || all(is.na(props_five))) {
          NA_character_
        } else {
          tab <- table(na.omit(props_five))
          tab <- sort(tab, decreasing = TRUE)
          paste0(names(tab), " (", as.integer(tab), ")") %>%
            paste(collapse = ", ")
        }
      }
    ) %>%
    ungroup() %>%
    rename(properties = dist)



  ## Plot-specific properties ----

  df_properties_plot <-
    # gen_observtn (convert code first)
    app_data %>%
      select(code, gen_observtn) %>%
      rename(value = gen_observtn) %>%
      # There is a problem with some codes (probably due to Excel)
      # They are expected in the format "10,60,30", but some values show
      # "10.6" (which should probably be "10,60")
      # Correct
      mutate(
        # First correct some very long values like "40.299999999999997"
        # (should be "40,30")
        value = if_else(
          str_length(value) > 15 & str_count(value, "\\.") == 1,
          suppressWarnings(str_replace(sprintf("%.2f", as.numeric(value)),
                                       "\\.", ",")),
          value),
        value = case_when(
          # If there's a dot with only 1 digit after it, add 0 and replace dot
          # with comma
          str_detect(value, "\\.\\d$") ~
            str_replace(value, "\\.(\\d)$", ",\\10"),
          # If there's a dot with 2 digits after it, just replace dot with comma
          str_detect(value, "\\.\\d{2}$") ~ str_replace(value, "\\.", ","),
          # Otherwise leave as is
          TRUE ~ value)) %>%
      mutate(gen_obs = strsplit(as.character(value), ",")) %>%
      select(-value) %>%
      unnest(gen_obs) %>%  # Unnest the list into individual rows
      mutate(gen_obs = na_if(gen_obs, "90")) %>%
      left_join(d_gen_properties %>%
                  rename(properties = property) %>%
                  mutate(code = as.character(code)),
                by = join_by("gen_obs" == "code")) %>%
      # Summarise all the properties (reported under the disturbed sampling)
      # across all sampling points
      # (more frequently reported data are first)
      group_by(code) %>%
      reframe(
        gen_observtn = if (all(is.na(properties))) {
          NA_character_
        } else {
          properties %>%
            na.omit() %>%
            table() %>%
            sort(decreasing = TRUE) %>%
            names() %>%
            paste(collapse = ", ")
        }
      ) %>%
      ungroup()







  # 2. Reshape sample checklist into one record per code_layer per plot ----

  df_layers <- app_data %>%
    select(-ends_with("dist"), -contains("thickness"), -contains("tamass"),
           -vol_ring) %>%
    rename_with(~ str_replace_all(., "eDNA", "edna")) %>%
    rename_with(~ str_replace_all(., "(flamm|carbon)", "dist")) %>%
    rename_with(~ str_replace_all(., "No_point", "no_point")) %>%
    select(code, plot_code_simple, team_harmonized, globalid, res_id_harmonized,
           wp, institute_sampling,
           contains("OL"), contains("OFH"),
           contains("M01"), contains("M13"), contains("M36"),
           contains("M61")) %>%
    mutate(across(-code, as.character)) %>%
    pivot_longer(
      cols = contains("OL") | contains("OFH") |
        contains("M01") | contains("M13") | contains("M36") | contains("M61"),
      names_to = "full_name",
      values_to = "value"
    ) %>%
    mutate(
      full_name = full_name %>%
        str_replace_all(paste0("(",
                               paste0(layers, collapse = "|"), ")"),
                        "_\\1_") %>%
        str_replace_all("__", "_") %>%
        str_replace_all("^_|_$", ""),
      code_layer = str_extract(full_name, paste0(layers, collapse = "|")),
      variable = str_remove(full_name,
                            paste0("_?", paste0(layers, collapse = "|"), "_?")),
      variable = str_replace_all(variable, "__", "_"),
      variable = str_replace_all(variable, "^_|_$", "")
    ) %>%
    select(code, plot_code_simple, team_harmonized, globalid, res_id_harmonized,
           wp, institute_sampling,
           code_layer, variable, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value) %>%
    rename(remarks_sample_dist = remarks_sample) %>%
    # Automatically detect and convert columns that contain only
    # numeric-convertible values to numeric class
    mutate(across(
      where(~ all(suppressWarnings(!is.na(as.numeric(.)) | is.na(.)))),
      as.numeric)) %>%
    mutate(
      dist_check = case_when(
        code_layer == "OL" & plot_code_simple ==
          "CULS__159__Slovakia_Pilsko_spruce__NA" ~ "Sample collected",
        code_layer == "OFH" & plot_code_simple %in% c(
          "NPS__21__Spicnik__2") ~ "Sample collected",
        code_layer == "M01" & plot_code_simple %in% c(
          "WSL__26__Bannhalde__NA",
          # Sample retrieved from eDNA2 (remainder after eDNA extraction)
          "WSL__36__Steibruchhau__NA") ~ "Sample collected",
        code_layer == "M13" & plot_code_simple %in% c(
          "NWFVA__03-082__Bockmer_Holz__NA") ~ "Sample collected",
        code_layer == "M61" & plot_code_simple %in% c(
          "WSL__24__Leihubelwald__NA",
          # Sample retrieved from undisturbed sample (respective masses known)
          "WSL__53__Murgtal__NA") ~ "Sample collected",
        code_layer == "M01" & plot_code_simple %in% c(
          "WSL__40__La Niva__NA") ~ "Sample not collected",
        code_layer == "M36" & plot_code_simple %in% c(
          "BGD-NP__1__Berchtesgaden National Park__F059",
          "FVA-BW__15__Waldmoor-Torfstich__1125") ~ "Sample not collected",
        code_layer == "M61" & plot_code_simple %in% c(
          "NWFVA__03-019__Stoeberhai__NA",
          "NWFVA__03-044__Limker Strang__NA",
          "FVA-BW__15__Waldmoor-Torfstich__1125",
          "NWFVA__06-014__Kniebrecht__NA") ~ "Sample not collected",
        dist_check == "1" ~ "Sample collected",
        dist_check == "0" ~ "Sample not collected",
        dist_check == "Sample don't collected" ~ "Sample not collected",
        TRUE ~ dist_check
      ),
      bulk_den_check = case_when(
        code_layer == "M13" & plot_code_simple %in% c(
          "WR__53__Bunderbos__NA") ~ "Sample collected",
        code_layer == "M36" & plot_code_simple %in% c(
          "WSL__39__Combe Biosse__NA") ~ "Sample collected",
        code_layer == "M36" & plot_code_simple %in% c(
          "NWFVA__03-011__Walbecker Warte__NA") ~ "Sample not collected",
        bulk_den_check == "1" ~ "Sample collected",
        bulk_den_check == "0" ~ "Sample not collected",
        bulk_den_check == "Sample don't collected" ~ "Sample not collected",
        TRUE ~ bulk_den_check
      ))

















  # 3. Compile all plot-specific data ----

  plot_columns <- c(
    "survey_date",
    "humus_form", "wrb_ref_soil_group", "wrb_qualifier_1", "wrb_qualifier_supp",
    "latitude", "longitude", "coordinates", "hor_accuracy",
    "air_temperature", "actual_weather", "past_weather",
    "soil_condition", "soil_dist", "macrorelief", "slope_type", "slope_deg",
    "moisture", "site_type", "scheme", "num_photo_humus", "management_type",
    "start_survey", "end_survey", "survey_month", "survey_year")

  d_soil_group <- read.csv("./data/additional_data/d_soil_group.csv",
                           sep = ";")
  d_soil_adjective <- read.csv("./data/additional_data/d_soil_adjective.csv",
                               sep = ";")


  df_plot <- app_data %>%
    select(code, globalid,
           composed_site_id, plot_code, plot_code_simple, res_id_harmonized,
           team_harmonized, institute_sampling, wp,
           any_of(plot_columns),
           # Columns with remarks
           other_remarks,
           other_observtn,
           ends_with("other_remarks_fragment"),
           ends_with("remarks_bulk_den_sample")
           ) %>%
    left_join(
      df_properties_plot,
      by = "code") %>%
    rename(code_wrb_ref_soil_group = wrb_ref_soil_group,
           code_wrb_qualifier_1 = wrb_qualifier_1) %>%
    mutate(
      # Extract the part before the first comma if any.
      # Note: not sure if this sequence follows the theoretical sequence
      # of decreasing importance according to WRB...
      code_wrb_qualifier_1_first = sub(",.*", "", code_wrb_qualifier_1)
    ) %>%
    left_join(
      d_soil_group %>%
        select(code, description) %>%
        rename(wrb_ref_soil_group = description),
      by = join_by("code_wrb_ref_soil_group" == "code")) %>%
    left_join(
      d_soil_adjective %>%
        select(code, description) %>%
        rename(wrb_qualifier_1 = description),
      by = join_by("code_wrb_qualifier_1_first" == "code")) %>%
    select(-code_wrb_qualifier_1_first) %>%
    relocate(wrb_ref_soil_group, wrb_qualifier_1,
             .before = code_wrb_ref_soil_group) %>%
    mutate(
      humus_form = str_to_title(humus_form),
      actual_weather = case_when(
        actual_weather == "RA" ~ "rain",
        actual_weather == "PC" ~ "partly cloudy",
        actual_weather == "SU" ~ "sunny/clear",
        TRUE ~ actual_weather),
      past_weather = case_when(
        past_weather == "WC3" ~ "no rain in the last 24 hours",
        past_weather == "WC4" ~
          "rainy without heavy rain in the last 24 hours",
        TRUE ~ past_weather),
      soil_condition = case_when(
        soil_condition == "NO" ~ "normal",
        TRUE ~ soil_condition),
      slope_type = case_when(
        slope_type == "()" ~ NA_character_,
        TRUE ~ slope_type),
      soil_dist = case_when(
        soil_dist == "GR" ~ "grazing",
        soil_dist == "ND" ~ "natural disturbances",
        # (e.g., wind throw, burrows, earth holes...)
        soil_dist == "DI" ~ "digging/excavation",
        # (e.g., ditching, raising beds)
        soil_dist == "IN" ~ "injured", # (e.g., litter removal)
        soil_dist == "CU" ~ "cultivated",
        soil_dist == "TR" ~ "trodden",
        soil_dist == "NO" ~ "none",
        soil_dist == "TR,GR" ~ "trodden, grazing",
        soil_dist == "DI,ND" ~ "digging/excavation, natural disturbances")
      ) %>%
    rename_with(~ paste0(.x, "_dec"), c(latitude, longitude))








  ### INCONSISTENCY 11 ----

  if (create_inconsistency_report) {

    rule <- paste0("Reported slope is high and may have been accidentally ",
                   "reported as a percentage instead of decimal degrees. ",
                   "Please confirm the unit.")

    rule_id <- "11"

    source("./src/functions/get_attribute_catalogue_app.R")
    attribute_catalogue <- get_attribute_catalogue_app()

    # Dataframe to collect problems
    inconsistencies <- data.frame(
      source = character(0),
      team = character(0),
      plot_code_app = character(0),
      res_id_inst = character(0),
      globalid = character(0),
      parameter = character(0),
      sample_code = character(0),
      value = character(0), # Must be character always!
      inconsistency_reason = character(0),
      unique_inconsistency = logical(0),
      rule_id = character(0))


    slope_plaus_max <- 30

    # Records with a problem

    recs_problem <- df_plot %>%
      filter(slope_deg > slope_plaus_max)

    inc <- recs_problem %>%
      mutate(
        inconsistency_reason = rule,
        value = slope_deg,
        parameter = "slope_deg") %>%
      left_join(attribute_catalogue %>%
                  rename(parameter_description = descr,
                         parameter_unit = unit),
                by = join_by("parameter" == "column_name")) %>%
      relocate(parameter_description, parameter_unit, .after = parameter) %>%
      mutate(
        # mark unique_inconsistency = TRUE
        # only for the first parameter per record
        unique_inconsistency = TRUE,
        source = "Survey123 app",
        team = team_harmonized,
        plot_code_app = code,
        res_id_inst = res_id_harmonized,
        sample_code = NA_character_,
        rule_id = rule_id) %>%
      mutate(value = as.character(value)) %>%
      select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
             parameter, parameter_description, parameter_unit, value,
             inconsistency_reason, unique_inconsistency, rule_id)

    inconsistencies <- bind_rows(
      inconsistencies,
      inc)

  } # End of "if create_inconsistency_report"





  if (apply_corrections) {

    df_stratifiers <- suppressMessages(
      read_sheet(as_id(id_stratifiers)) %>%
      select(plot_code_simple, wrb_ref_soil_group_corr, humus_form_corr))

    df_plot <- df_plot %>%
      left_join(df_stratifiers,
                by = "plot_code_simple") %>%
      mutate(
        # Add any plots with slopes reported as percentages
        slope_deg = case_when(
          # Convert slope from percent to degrees for some sites of WSL
          plot_code_simple %in% c("WSL__32__Josenwald__NA") ~
            round(atan(slope_deg / 100) * 180 / pi),
          # Correction requested by partner
          plot_code_simple == "WSL__24__Leihubelwald__NA" ~ 25,
          # Cap slopes bigger than 40 ° to a value of 40 ° because steeper
          # slopes would have been difficult to take soil samples
          # This is the case for:
          # "LWF__x__Hoellbachgespreng__NA" (original: 43.4 °)
          # "WSL__31__Weidel__NA" (original: 52 °)
          # Note: at the moment, we decided to trust the slope data
          # (which are confirmed by the partner)
          # However, it would be relevant to check the sensitivity of the
          # calculated stocks to the selected slope (e.g. 40 ° vs 52 °)
          # slope_deg > 40 ~ 40,
          TRUE ~ coalesce(slope_deg, 0)
        ),
        humus_form = coalesce(humus_form_corr,
                              humus_form),
        wrb_ref_soil_group = coalesce(wrb_ref_soil_group_corr,
                                      wrb_ref_soil_group),
        # WRB 1st qualifier has not been validated. Therefore,
        # original qualifiers are most likely no longer applicable for plots
        # for which the Reference Soil Group was corrected
        wrb_qualifier_1 = ifelse(!is.na(wrb_ref_soil_group_corr),
                                 NA_character_,
                                 wrb_qualifier_1),
        latitude_dec = case_when(
          # Correction requested by partner
          plot_code_simple == "NWFVA__03-046__Koenigsbuche__NA" ~ 51.582915000,
          TRUE ~ latitude_dec
        ),
        longitude_dec = case_when(
          # Correction requested by partner
          plot_code_simple == "NWFVA__03-046__Koenigsbuche__NA" ~ 10.339716667,
          # Typo detected relative to HIS metadata table (original: 9.0938)
          plot_code_simple == "NWFVA__03-012__Grosser Freeden__NA" ~ 8.0938,
          TRUE ~ longitude_dec
        ),
        # eDNA samples were recollected for three German plots (due to freezer
        # that got unfrosted). Correct the date, weather and soil metadata for
        # the new sampling event, as those metadata are more important for
        # eDNA than for other parameters.
        # Note: "actual_weather", "past_weather" and "soil_condition" were
        # he same for the two sampling events for all three plots:
        # actual_weather: "partly cloudy" (without rain)
        # past_weather: "rainy without heavy rain in the last 24 hours"
        # soil_condition: "normal"
        survey_date = case_when(
          plot_code_simple == "NWFVA__03-003__Herrenholz__NA" ~
            as.Date(parse_date("2025-07-15")), # Original survey: 2025-05-26
          plot_code_simple == "NWFVA__03-002__Franzhorn__a" ~
            as.Date(parse_date("2025-07-14")), # Original survey: 2025-06-04
          plot_code_simple == "NWFVA__03-002__Franzhorn__b" ~
            as.Date(parse_date("2025-07-14")), # Original survey: 2025-06-05
          TRUE ~ survey_date
        ),
        survey_month = case_when(
          plot_code_simple == "NWFVA__03-003__Herrenholz__NA" ~ 7,
          plot_code_simple == "NWFVA__03-002__Franzhorn__a" ~ 7,
          plot_code_simple == "NWFVA__03-002__Franzhorn__b" ~ 7,
          TRUE ~ survey_month
        ),
        air_temperature = case_when(
          plot_code_simple == "NWFVA__03-003__Herrenholz__NA" ~
            20, # Original survey: 15 °C
          plot_code_simple == "NWFVA__03-002__Franzhorn__a" ~
            22, # Original survey: 15 °C
          plot_code_simple == "NWFVA__03-002__Franzhorn__b" ~
            22, # Original survey: 16 °C
          TRUE ~ air_temperature
        )) %>%
      select(-wrb_ref_soil_group_corr, -humus_form_corr)

  } # End of "if apply_corrections"














  # 4. Forest floor ----

  ## Compile mass and thickness of forest floor sampling point ----

  # First create a dataset per sampling point

  df_sampling_points_ff <- app_data %>%
    select(
      code, team_harmonized, globalid, res_id_harmonized, plot_code_simple, wp,
      institute_sampling, surface_frame,
      matches(paste0("(^|_)", paste0(c("OL", "OFH"), collapse = "|"))) &
        starts_with("P") &
        (contains("thickness") | contains("tamass"))
      ) %>%
    mutate(across(matches("^P\\d+_M\\d+"), as.character))

  if (apply_corrections) {

    df_sampling_points_ff <- df_sampling_points_ff %>%
      # ---
      # Any MANUAL CORRECTIONS for thickness/tamass per point should go here!
      # (manual corrections are based on comments in app, field photos,
      #  feedback by partners, expert assessment)
      # ---
      mutate(
        P1_OL_thickness = case_when(
          # Correction suggested by partner based on detected inconsistency
          plot_code_simple == "LWF__119_a__Stachel__NA" ~ 0.3,
          TRUE ~ P1_OL_thickness
        ),
        P1_OL_tamass = case_when(
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            30.7,
          TRUE ~ P1_OL_tamass
        ),
        P1_OFH_thickness = case_when(
          # Correction suggested by partner based on detected inconsistency
          # (instead of 0.5)
          plot_code_simple == "UNITBV__2__Cozia__NA" ~ 1.5,
          # Probably a typo (3.9 instead of 39, based on other sampling
          # points and based on photo humus at P1)
          plot_code_simple == "NWFVA__03-049__Haringer Berg__NA" ~ 3.9,
          TRUE ~ P1_OFH_thickness
        ),
        P1_OFH_tamass = case_when(
          # Correction suggested by partner based on detected inconsistency
          plot_code_simple == "LWF__65_a__Friedergries__NA" ~ 2614,
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            100,
          TRUE ~ P1_OFH_tamass
        ),
        P2_OL_thickness = case_when(
          # Uncertain (see explanation P2_OL_tamass)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F059" ~
            NA_real_,
          TRUE ~ P2_OL_thickness
        ),
        P2_OL_tamass = case_when(
          # Original value: 391 g. Partner asked to subtract the mass of the
          # bucket (288 g) after we pointed out that this sampling point is
          # heavy as compared to the thickness (i.e., 103 g).
          # However, that means the sum across sampling points becomes lower
          # than the mixed subsample in the field (even though the net fresh
          # mass upon arrival in the lab corresponds with the latter). The
          # partner thinks this sampling point should probably be ignored for
          # OL, because of this uncertainty.
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F059" ~
            NA_real_,
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            67,
          TRUE ~ P2_OL_tamass
        ),
        P2_OFH_tamass = case_when(
          # Correction suggested by partner based on detected inconsistency
          plot_code_simple == "LWF__65_a__Friedergries__NA" ~ 5440,
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            411,
          # Correction suggested by partner
          # (typo in app but correct in field form)
          # Original: 1120 g
          plot_code_simple == "NWFVA__01_020__Butterberg_SH__NA" ~ 120,
          # Correction proposed by partner (based on field form)
          # Original: 39.1 g
          plot_code_simple == "URK__6__Debina reserve__NA" ~ 139.1,
          TRUE ~ P2_OFH_tamass
        ),
        P3_OL_thickness = case_when(
          # Typo
          plot_code_simple == "CULS__181__Slovakia_Zadna Ticha_spruce__NA" ~
            1.5,
          TRUE ~ P3_OL_thickness
        ),
        P3_OL_tamass = case_when(
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            39,
          TRUE ~ P3_OL_tamass
        ),
        P3_OFH_tamass = case_when(
          # Typo reported by partner, formerly 471 which is value for P2
          plot_code_simple == "FVA-BW__588__Altlochkar-Rotwasser__NA" ~ 1385,
          # Correction suggested by partner based on detected inconsistency
          plot_code_simple == "LWF__65_a__Friedergries__NA" ~ 2333,
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            556,
          TRUE ~ P3_OFH_tamass
        ),
        P4_OL_thickness = case_when(
          # Correction suggested by partner based on detected inconsistency
          plot_code_simple == "LWF__119_a__Stachel__NA" ~ 0.3,
          TRUE ~ P4_OL_thickness
        ),
        P4_OL_tamass = case_when(
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            38,
          TRUE ~ P4_OL_tamass
        ),
        P4_OFH_tamass = case_when(
          # Correction suggested by partner based on detected inconsistency
          plot_code_simple == "LWF__65_a__Friedergries__NA" ~ 5481,
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            412,
          TRUE ~ P4_OFH_tamass
        ),
        P5_OL_tamass = case_when(
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            66.9,
          TRUE ~ P5_OL_tamass
        ),
        P5_OFH_tamass = case_when(
          # Correction suggested by partner based on detected inconsistency
          plot_code_simple == "LWF__65_a__Friedergries__NA" ~ 1506,
          # Mistake in mass buckets
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension" ~
            759.9,
          # Correction proposed by partner (based on field form)
          # Original: 76.4 g
          plot_code_simple == "URK__6__Debina reserve__NA" ~ 101.5,
          TRUE ~ P5_OFH_tamass
        ),
        # Replace all P2-P5 forest floor data by NA for Seeliwald
        # (since only P1 was sampled)
        across(
          matches("^P[2-5]_(OL|OFH)_(thickness|tamass)$"),
          ~ ifelse(plot_code_simple == "WSL__25__Seeliwald__NA",
                   NA_real_,
                   .x)
        ),
        # Note: thicknesses for LWF 65_a OFH were kept from the first survey
        # as they were not recorded for the second, but considered similar
        # by the partner
      ) %>%
      # ---
      # Any MANUAL CORRECTIONS for surface_frame should go here!
      # (manual corrections are based on comments in app, field photos,
      #  feedback by partners, expert assessment)
      # ---
      mutate(
        surface_frame = case_when(
          # Must have been a typo based on other plots from partner
          plot_code_simple == "UL__9__Gorjanci__NA" &
            surface_frame == 0.625 ~ 0.0625,
          # Feedback from partner
          institute_sampling == "NWFVA" & surface_frame == 0.0599 ~ 0.059,
          institute_sampling == "NWFVA" & surface_frame == 0.5990 ~ 0.059,
          institute_sampling == "NWFVA" & surface_frame == 590 ~ 0.059,
          institute_sampling == "NWFVA" & surface_frame == 1180 ~ 0.118,
          # Note: forest floor sampling frame of UNIUD was smaller
          # than requested. They said that they did this because of
          # the big amount of material per sampling point.
          TRUE ~ surface_frame
        )
      )

  } # End of "if apply_corrections"




  df_sampling_points_ff <- df_sampling_points_ff %>%
    pivot_longer(
      cols = matches("^P\\d+_"),
      names_to = c("sampling_point", "code_layer", "variable"),
      names_pattern = "^(P\\d+)_([A-Za-z]+)_([a-z0-9_]+)$",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    left_join(
      df_layers %>%
        select(plot_code_simple, code_layer, dist_check),
      by = join_by("plot_code_simple", "code_layer")) %>%
    select(code, plot_code_simple, code_layer, sampling_point, everything()) %>%
    # Filter out rows from sampling points P6-P9 if tamass is NA
    filter(
      !(sampling_point %in% c("P6", "P7", "P8", "P9")) | !is.na(tamass)) %>%
    # Filter out samples that were not collected
    filter(!(dist_check == "Sample not collected")) %>%
    arrange(code, desc(code_layer))


  ### INCONSISTENCY 2 ----

  if (create_inconsistency_report) {

    rule <- paste0("Forest floor mass should be plausible in comparison with ",
                   "thickness and surface area frame")
    rule_id <- "2"

    # These are the 5-95 % quantiles of the bulk densities of organic matrices
    # in ICP Forests (systematic Level I) (kg m-3)
    # Note: this is dry while values from the field are moist!

    mc_wet_by_dry_min <- 1
    mc_wet_by_dry_max <- 3 # Estimated based on Sevendonk which was quite wet

    bd_ofh_plaus <- c(55 * mc_wet_by_dry_min, 190 * mc_wet_by_dry_max)
    bd_ol_plaus <- c(15 * mc_wet_by_dry_min, 136 * mc_wet_by_dry_max)
    bd_peat_plaus <- c(51 * mc_wet_by_dry_min, 221 * mc_wet_by_dry_max)

    # Records with a problem

    recs_problem <- df_sampling_points_ff %>%
      mutate(
        # tamass in gram
        tamass_est_min = case_when(
          code_layer == "OFH" ~
            (thickness * 1E-2 * surface_frame) * bd_ofh_plaus[1] * 1E3,
          code_layer == "OL" ~
            (thickness * 1E-2 * surface_frame) * bd_ol_plaus[1] * 1E3),
        tamass_est_max = case_when(
          code_layer == "OFH" ~
            (thickness * 1E-2 * surface_frame) * bd_ofh_plaus[2] * 1E3,
          code_layer == "OL" ~
            (thickness * 1E-2 * surface_frame) * bd_ol_plaus[2] * 1E3),
        # standardised deviation
        tamass_dev = (abs(tamass - (tamass_est_min + tamass_est_max) / 2)) /
          ((tamass_est_max - tamass_est_min) / 2)) %>%
      filter(thickness > 0 &
               (tamass < tamass_est_min |
                  tamass > tamass_est_max)) %>%
      # Since there is quite some uncertainty in measurement of layer
      # thicknesses we can filter for a relative deviation of at least X
      filter(tamass_dev > 1.5)

    inc <- recs_problem %>%
      mutate(
        inconsistency_reason =
          paste0(rule,
                 " [min: ", round(tamass_est_min, 1),
                 " g, max: ", round(tamass_est_max, 1), " g]")) %>%
      # pivot longer to get one row per parameter
      # (thickness, tamass, surface_frame)
      pivot_longer(
        cols = c(tamass, thickness, surface_frame),
        names_to = "parameter",
        values_to = "value"
      ) %>%
      mutate(
        value = case_when(
          parameter %in% c("thickness", "tamass") ~
            as.character(round(value, 1)),
          TRUE ~ as.character(round(value, 4))),
        parameter = case_when(
          parameter %in% c("thickness", "tamass") ~
            paste(sampling_point, tolower(code_layer), parameter, sep = "_"),
          TRUE ~ parameter)) %>%
      left_join(attribute_catalogue %>%
                  rename(parameter_description = descr,
                         parameter_unit = unit),
                by = join_by("parameter" == "column_name")) %>%
      relocate(parameter_description, parameter_unit, .after = parameter) %>%
      mutate(
        # mark unique_inconsistency = TRUE
        # only for the first parameter per record
        unique_inconsistency = (grepl("tamass", parameter)),
        source = "Survey123 app",
        team = team_harmonized,
        plot_code_app = code,
        res_id_inst = res_id_harmonized,
        sample_code = code_layer,
        rule_id = rule_id) %>%
      mutate(value = as.character(value)) %>%
      select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
             parameter, parameter_description, parameter_unit, value,
             inconsistency_reason, unique_inconsistency, rule_id)

    inconsistencies <- bind_rows(
      inconsistencies,
      inc)

  } # End of "if create_inconsistency_report"






  ## Summarise forest floor across sampling points ----

  # Summarise forest floor masses using BRMS for the uncertainty range
  # Remove files in './models/forest_floor_mass_brms/' if needed,
  # as changing the iter value does not always trigger refitting
  # of the brms model.

  source("./src/functions/summarise_forest_floor_masses.R")
  df_ff_bayes <- summarise_forest_floor_masses(
    df_sampling_points_ff = df_sampling_points_ff %>%
      filter(wp == "WP2"),
    iter = 2000)




  df_ff_summ <- df_sampling_points_ff %>%
    # Corrections for slope in the forest floor
    # 1. All depths (thickness) should be reported plumb-vertically since
    #    below-ground depths are recorded in a plumb-vertical way
    # 2. Surface area of the forest floor sampling frame should be multiplied
    #    with cos(slope) for a horizontal projection (i.e., surface area
    #    "from the sky"). In other words, the areal mass will increase by
    #    a factor 1/cos(slope)
    left_join(
      df_plot %>%
        select(plot_code_simple, slope_deg),
      by = "plot_code_simple"
    ) %>%
    mutate(
      thickness = thickness / cos(slope_deg * pi / 180),
      surface_frame = surface_frame * cos(slope_deg * pi / 180)
    ) %>%
    # Summarise
    group_by(code, institute_sampling,
             plot_code_simple, code_layer, surface_frame) %>%
    reframe(
      count_ff_frames = sum(!is.na(tamass)),
      thickness_min = ifelse(
        any(!is.na(thickness)),
        round(
          mean(as.numeric(thickness), na.rm = TRUE) - sd(as.numeric(thickness),
                                                         na.rm = TRUE), 1),
        NA_real_),
      thickness_max = ifelse(
        any(!is.na(thickness)),
        round(
          mean(as.numeric(thickness), na.rm = TRUE) + sd(as.numeric(thickness),
                                                         na.rm = TRUE), 1),
        NA_real_),
      thickness = ifelse(
        any(!is.na(thickness)),
        round(mean(as.numeric(thickness), na.rm = TRUE), 1),
        NA_real_),
      tamass_compiled = paste(as.character(round(tamass)), collapse = ","),
      tamass_mean = ifelse(
        any(!is.na(tamass)),
        round(mean(as.numeric(tamass), na.rm = TRUE), 1),
        NA_real_),
      tamass = ifelse(
        any(!is.na(tamass)),
        sum(as.numeric(tamass), na.rm = TRUE),
        NA_real_)) %>%
    # Add summarised bedrock depth (posterior mean and 95% credible interval)
    left_join(
      df_ff_bayes %>%
        rename(tamass_median_bayes = .epred,
               tamass_min = .lower,
               tamass_max = .upper) %>%
        select(plot_code_simple, code_layer, starts_with("tamass_")),
      by = join_by("plot_code_simple", "code_layer")) %>%
    relocate(tamass_median_bayes, .after = tamass_max)












  # 5. Potential to take undisturbed samples ----

  d_kopecky_depths <- tibble(
    code = c(10, 11, 12, 13),
    code_layer = c("M01", "M13", "M36", "M61"))

  # Small helper function which we will use later to update the column
  # undist_sampling_points. This column lists all sampling points where
  # undisturbed samples (Kopeckys) were taken. The function makes sure that
  # the number of "P1" entries matches the reported number of P1 undisturbed
  # samples.
  update_points <- function(points, n) {
    if (is.na(n) || n <= 1) return(points)
    pts <- str_split(points, ",")[[1]]
    other <- pts[pts != "P1"]
    updated <- c(rep("P1", n), other)
    paste(updated, collapse = ",")
  }


  # There are two formats of bulk density data:
  # - data collected by VUK in 2024 have "10,11,12,13" in "P*_bulk_density"
  #   and NA in "P*_bulk_density_lyr"
  # - data collected in 2025 have "YES"/"NO" in "P*_bulk_density"
  #   and "M01,M13,M36,M61" in "P*_bulk_density_lyr"

  df_rings <- bind_rows(
    # 2024 records
    app_data %>%
      filter(!P1_bulk_density %in% c("YES", "NO")) %>%
      select(code, team_harmonized, globalid, res_id_harmonized,
             plot_code_simple, site_type,
             # Volume of kopecky ring
             # (typically one ring per depth x sampling point)
             vol_ring,
             P1_num_M36_undist_samples, P1_num_M61_undist_samples,
             starts_with("P") & ends_with("_bulk_density")) %>%
      pivot_longer(cols = matches("^P[1-5]_bulk_density$"),
                   names_to = "sampling_point",
                   values_to = "code_depths") %>%
      mutate(
        # Keep only "P1", "P2", etc.
        sampling_point = str_extract(sampling_point, "^P\\d+")) %>%
      separate_rows(code_depths, sep = ",") %>%
      mutate(code_depths = as.integer(str_trim(code_depths))) %>%
      left_join(d_kopecky_depths,
                by = c("code_depths" = "code")) %>%
      filter(!is.na(code_layer)) %>%
      group_by(code, team_harmonized, globalid, res_id_harmonized,
               plot_code_simple, site_type, code_layer, vol_ring,
               P1_num_M36_undist_samples, P1_num_M61_undist_samples) %>%
      reframe(
        undist_sampling_points = paste(sort(unique(sampling_point)),
                                       collapse = ","),
        count_rings = n()),
    # 2025 records
    app_data %>%
      select(code, team_harmonized, globalid, res_id_harmonized,
             plot_code_simple, site_type,
             # Volume of kopecky ring
             # (typically multiple rings per depth across sampling points or
             # per sampling point)
             vol_ring,
             P1_num_M36_undist_samples, P1_num_M61_undist_samples,
             starts_with("P") & ends_with("_bulk_density_lyr")) %>%
      pivot_longer(cols = matches("^P[1-5]_bulk_density_lyr$"),
                   names_to = "sampling_point",
                   values_to = "code_depths") %>%
      mutate(
        # Keep only "P1", "P2", etc.
        sampling_point = str_extract(sampling_point, "^P\\d+")) %>%
      separate_rows(code_depths, sep = ",") %>%
      rename(code_layer = code_depths) %>%
      arrange(code, code_layer) %>%
      group_by(code, team_harmonized, globalid, res_id_harmonized,
               plot_code_simple, site_type, code_layer, vol_ring,
               P1_num_M36_undist_samples, P1_num_M61_undist_samples) %>%
      reframe(
        undist_sampling_points = paste(sort(unique(sampling_point)),
                                       collapse = ","),
        count_rings = n())
    ) %>%
    mutate(
      count_rings_orig = count_rings)



  if (apply_corrections) {

    df_rings <- df_rings %>%
      # ---
      # Any MANUAL CORRECTIONS should happen here!
      # Part 1: P1_num_M36_undist_samples and P1_num_M36_undist_samples
      # (count_rings, undist_sampling_points, vol_ring follow below)
      # (manual corrections are based on comments in app, field photos,
      #  feedback by partners, expert assessment)
      # ---
      mutate(
        P1_num_M36_undist_samples = case_when(
          plot_code_simple == "AlberIT - UNIRC__24__Macchia dell'Orso__NA" ~ 3,
          plot_code_simple == "BFNP__2122__Laerchenberg__NA" ~ 4,
          plot_code_simple == "FVA-BW__17__Schnapsried__1127" ~ NA,
          plot_code_simple == "FVA-BW__543__Schnepfenmoos__NA" ~ NA,
          plot_code_simple == "FVA-BW__602__Riedis__NA" ~ NA,
          plot_code_simple == "FVA-BW__62__Feldseewald__NA" ~ NA,
          plot_code_simple == "FVA-BW__621__Siedigkopf__NA" ~ NA,
          plot_code_simple == "FVA-BW__9__Napf__1109" ~ NA,
          plot_code_simple == "LWF__119_a__Stachel__NA" ~ 1,
          plot_code_simple == "NWFVA__03-060__Meinsberg__NA" ~ 5,
          plot_code_simple == "WSL__26__Bannhalde__NA" ~ 5,
          TRUE ~ P1_num_M36_undist_samples
        ),
        P1_num_M61_undist_samples = case_when(
          plot_code_simple == "AlberIT - UNIRC__22__Monte Tranquillo__NA" ~ 3,
          plot_code_simple == "AlberIT - UNIRC__24__Macchia dell'Orso__NA" ~ 4,
          plot_code_simple == "BFNP__137__Mittelsteighuette__NA" ~ 5,
          plot_code_simple == "BFNP__2122__Laerchenberg__NA" ~ 5,
          plot_code_simple == "BFNP_EXTRA__59__Recherau__NA" ~ 5,
          plot_code_simple == "FVA-BW__17__Schnapsried__1127" ~ NA,
          plot_code_simple == "FVA-BW__543__Schnepfenmoos__NA" ~ NA,
          plot_code_simple == "FVA-BW__602__Riedis__NA" ~ NA,
          plot_code_simple == "FVA-BW__62__Feldseewald__NA" ~ NA,
          plot_code_simple == "FVA-BW__621__Siedigkopf__NA" ~ NA,
          plot_code_simple == "FVA-BW__9__Napf__1109" ~ NA,
          plot_code_simple == "LWF__119_a__Stachel__NA" ~ NA,
          plot_code_simple == "NWFVA__03-019__Stoeberhai__NA" ~ NA,
          plot_code_simple == "NWFVA__03-044__Limker Strang__NA" ~ NA,
          plot_code_simple == "NWFVA__03-046__Koenigsbuche__NA" ~ NA,
          plot_code_simple == "NWFVA__06-014__Kniebrecht__NA" ~ NA,
          plot_code_simple == "NWFVA__06-006__Niddahaenge__a" ~ NA,
          plot_code_simple == "WSL__18__Fuerstenhalde__NA" ~ 3,
          plot_code_simple == "WSL__26__Bannhalde__NA" ~ 4,
          plot_code_simple == "NWFVA__03-083__Saubrink_Oberberg__NA" ~ 5,
          TRUE ~ P1_num_M61_undist_samples
        ))

  } # End of "if apply_corrections"


  df_rings <- df_rings %>%
    # For some plots (e.g. BFNP__2122__Laerchenberg__NA),
    # partners were unable to collect undisturbed samples at the
    # deepest layers (M36, M61) across all sampling points due to stoniness, and
    # instead took multiple samples from the central pit (P1). The number of P1
    # samples taken is recorded in P1_num_M36_undist_samples and
    # P1_num_M61_undist_samples.
    # However, partners sometimes forgot to indicate P1 for those depths,
    # which means that P1 is not in undist_sampling_points and count_rings,
    # or a record for the given depth is at this point lacking.
    # The code below corrects this:
    # - if no record exists for M36/M61, a new row is added with P1 as the
    #   sampling point;
    # - if a record exists but P1 is missing, P1 is prepended to
    #   undist_sampling_points and count_rings is incremented by 1.
    group_by(code) %>%
    group_modify(~ {
      df <- .x

      for (layer in c("M36", "M61")) {

        p1_col <- if (layer == "M36") "P1_num_M36_undist_samples" else
          "P1_num_M61_undist_samples"

        p1_num <- df[[p1_col]][1]  # same value across rows for the plot

        # Only act if P1_num is non-NA and >= 1 (i.e., they did take a sample)
        if (is.na(p1_num) || p1_num < 1) next

        row_exists <- any(df$code_layer == layer)

        if (!row_exists) {
          # Case 1: no record for this depth → add a new row ---
          new_row <- df[1, ]  # copy metadata from first row
          new_row$code_layer <- layer
          new_row$undist_sampling_points <- "P1"
          new_row$count_rings <- 1L
          df <- bind_rows(df, new_row)

        } else {
          # Case 2: record exists but P1 not listed in
          #         undist_sampling_points ---
          idx <- which(df$code_layer == layer)
          current_pts <- df$undist_sampling_points[idx]

          # Only patch if non-NA and P1 not already present
          if (!is.na(current_pts) && !grepl("P1", current_pts)) {
            df$undist_sampling_points[idx] <- paste0("P1,", current_pts)
            df$count_rings[idx] <- df$count_rings[idx] + 1L
          }
        }
      }
      df
    }) %>%
    ungroup() %>%
    # For plots in which all or some M36 and M61 rings were taken from the deep
    # central pit, correct the number of rings based on the columns
    # P1_num_M36_undist_samples, P1_num_M61_undist_samples
    mutate(
      count_rings = case_when(
        # 1. M36
        # If P1_num > 1 and count_rings == 1 → use value in P1_num
        code_layer == "M36" & !is.na(P1_num_M36_undist_samples) &
          (P1_num_M36_undist_samples > 1) & (count_rings == 1) ~
          P1_num_M36_undist_samples,
        # If (P1_num + count_rings) > 6 → a bit suspicious, probably these
        # were 5 rings in total
        code_layer == "M36" & !is.na(P1_num_M36_undist_samples) &
          (P1_num_M36_undist_samples > 1) &
          ((P1_num_M36_undist_samples + count_rings) > 6) ~ 5,
        # If (P1_num + count_rings) <= 6 → count rings is the sum of both
        # (minus 1 because P1 is counted in both P1_num and count_rings)
        code_layer == "M36" & !is.na(P1_num_M36_undist_samples) &
          (P1_num_M36_undist_samples > 1) &
          ((P1_num_M36_undist_samples + count_rings) <= 6) ~
          (P1_num_M36_undist_samples + count_rings - 1),
        # 2. M61
        # If P1_num > 1 and count_rings == 1 → use value in P1_num
        code_layer == "M61" & !is.na(P1_num_M61_undist_samples) &
          (P1_num_M61_undist_samples > 1) & (count_rings == 1) ~
          P1_num_M61_undist_samples,
        # If (P1_num + count_rings) > 6 → a bit suspicious, probably these
        # were 5 rings in total
        code_layer == "M61" & !is.na(P1_num_M61_undist_samples) &
          (P1_num_M61_undist_samples > 1) &
          ((P1_num_M61_undist_samples + count_rings) > 6) ~ 5,
        # If (P1_num + count_rings) <= 6 → count rings is the sum of both
        # (minus 1 because P1 is counted in both P1_num and count_rings)
        code_layer == "M61" & !is.na(P1_num_M61_undist_samples) &
          (P1_num_M61_undist_samples > 1) &
          ((P1_num_M61_undist_samples + count_rings) <= 6) ~
          (P1_num_M61_undist_samples + count_rings - 1),
        # ELSE, ignore P1_num_M36_undist_samples and
        # P1_num_M61_undist_samples
        TRUE ~ count_rings)) %>%
    rowwise() %>%
    mutate(
      # Update undist_sampling_points also
      # For example, if: undist_sampling_points is "P1,P4,P5" and
      # P1_num_M36_undist_samples is 5,
      # I want undist_sampling_points to become "P1,P1,P1,P1,P1,P4,P5"
      undist_sampling_points = case_when(
        # M36
        code_layer == "M36" & !is.na(P1_num_M36_undist_samples) &
          (P1_num_M36_undist_samples > 1)  ~
          update_points(undist_sampling_points, P1_num_M36_undist_samples),
        # M61
        code_layer == "M61" & !is.na(P1_num_M61_undist_samples) &
          (P1_num_M61_undist_samples > 1)  ~
          update_points(undist_sampling_points, P1_num_M61_undist_samples),
        TRUE ~ undist_sampling_points)) %>%
    ungroup() %>%
    left_join(
      df_layers %>%
        select(plot_code_simple, code_layer,
               bulk_den, bulk_den_check),
      by = join_by("plot_code_simple", "code_layer")) %>%
    # Filter out records for which no sample was taken
    # Consistent with "bulk_den == 0" for WP2
    filter(bulk_den_check == "Sample collected")




  if (apply_corrections) {

    df_rings <- df_rings %>%
      # ---
      # Any MANUAL CORRECTIONS should happen here!
      # Part 2: count_rings, undist_sampling_points, vol_ring
      # (P1_num_M36_undist_samples and P1_num_M36_undist_samples were
      #  earlier in the script)
      # (manual corrections are based on comments in app, field photos,
      #  feedback by partners, expert assessment)
      # ---
      mutate(
        count_rings = case_when(
          # Four rings (as confirmed by partner)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F048" &
            code_layer == "M01" ~ 4,
          # Probably one ring instead of five (as the partner also suspects)
          # This is plausible based mass of undisturbed WBL
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F066" &
            code_layer == "M01" ~ 1,
          # Probably one ring instead of five (as the partner also suspects)
          # This is plausible based on mass of undisturbed WBL
          # (in fact, volume still seems high as compared to mass)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F099" &
            code_layer == "M01" ~ 1,
          # Based on communication with the partner
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" &
            code_layer == "M36" ~ 2,
          # One ring based on communication with the partner
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" &
            code_layer == "M61" ~ 1,
          # Based on communication with the partner
          plot_code_simple == "CULS__123__Slovakia_Sutovska_beech__NA" &
            code_layer == "M01" ~ 4,
          # Based on communication with the partner
          plot_code_simple == "CULS__153__Slovakia_Koprova dolina_spruce__NA" &
            code_layer == "M01" ~ 3,
          # Three rings instead of five (second sampling event)
          plot_code_simple == "LWF__119_a__Stachel__NA" &
            code_layer == "M36" ~ 3,
          # Three rings instead of two (second sampling event)
          plot_code_simple == "LWF__65_a__Friedergries__NA" &
            code_layer == "M01" ~ 3,
          # Five rings
          plot_code_simple == "NWFVA__03-060__Meinsberg__NA" &
            code_layer == "M36" ~ 5,
          # As confirmed by partner
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            code_layer %in% c("M01", "M13") ~ 5,
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            code_layer == "M36" ~ 4,
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            code_layer == "M61" ~ 3,
          # As confirmed by partner
          plot_code_simple == "VUK__5__Cahnov-Soutok__NA" &
            code_layer == "M01" ~ 4,
          # As confirmed by partner
          plot_code_simple == "WSL__21__Tariche Bois Banal__NA" &
            code_layer == "M01" ~ 4,
          # As confirmed by partner
          plot_code_simple == "WSL__25__Seeliwald__NA" &
            code_layer %in% c("M01", "M13") ~ 3,
          # Note: The partner recalled collecting three rings for M36.
          # However, that is impossible (would give a bulk density of
          # 2622 kg m-3). After comparison with the PTF bulk density
          # (1151 kg m-3), it seems most likely that they took 5 rings
          # (1529 kg m-3). We therefore decided to assume 5 rings together
          # with the partner
          plot_code_simple == "WSL__25__Seeliwald__NA" &
            code_layer == "M36" ~ 5,
          # As reported
          plot_code_simple == "WSL__39__Combe Biosse__NA" &
            code_layer %in% c("M01", "M36") ~ 2,
          # As confirmed by partner
          plot_code_simple == "WSL__4__Pfynwald__NA" &
            code_layer == "M01" ~ 4,
          # Special case: undisturbed sample was splitted into undisturbed
          # and disturbed sample at the central lab
          plot_code_simple == "WSL__53__Murgtal__NA" &
            code_layer == "M61" ~ 0.6670326,
          # As confirmed by partner
          plot_code_simple == "WSL__53__Murgtal__NA" &
            code_layer == "M01" ~ 3,
          # As confirmed by partner
          plot_code_simple == "WSL__6__St. Jean__NA" &
            code_layer == "M01" ~ 3,
          # NW-FVA used 20 x small cilinders of 5.3 cm3 instead of the
          # standard kopecky rings in case the latter were impossible in stony
          # sites.
          # ---
          # M01
          (code_layer == "M01" &
             plot_code_simple %in% c(
               "FVA-BW__10__Zweribach__NA", # Based on feedback partner
               "FVA-BW__1036__Nollenwald__NA", # Based on feedback partner
               "FVA-BW__588__Altlochkar-Rotwasser__NA")) |
            # M13
            (code_layer == "M13" &
               plot_code_simple %in% c(
                 "FVA-BW__14__Wildseemoor__1123",
                 "FVA-BW__15__Waldmoor-Torfstich__1125",
                 "FVA-BW__500__Barlochkar__NA",
                 "FVA-BW__510__Sturmlesloch__NA",
                 "FVA-BW__59__Teufelsries__NA",
                 "NWFVA__03-049__Haringer Berg__NA")) |
            # M36
            (code_layer == "M36" &
               plot_code_simple %in% c(
                 "FVA-BW__602__Riedis__NA")) ~ 20,
          # Based on feedback partner
          code_layer == "M13" &
            plot_code_simple %in% c("NWFVA__03-074__Butterberg_NI__NA") ~ 40,
          # Based on feedback partner
          code_layer == "M13" &
            plot_code_simple %in% c("NWFVA__03-014__Huenstollen__NA") ~ 10,
          (code_layer == "M13" &
             plot_code_simple ==
               "NWFVA__06-004__Wattenberg und Hundsberg__b") ~ NA_real_,
          TRUE ~ count_rings
        ),
        undist_sampling_points = case_when(
          # Originally "P1,P2,P3,P4,P5" while all five rings were taken at P1
          plot_code_simple == "BFNP_EXTRA__59__Recherau__NA" &
            code_layer == "M61" ~ "P1,P1,P1,P1,P1",
          # Four rings (as confirmed by partner)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F048" &
            code_layer == "M01" ~ "P1,P2,P3,P4",
          # Probably one ring instead of five (as the partner also suspects)
          # (Not sure which sampling point but this does not matter)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F066" &
            code_layer == "M01" ~ "P1",
          # Probably one ring instead of five (as the partner also suspects)
          # (Not sure which sampling point but this does not matter)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F099" &
            code_layer == "M01" ~ "P1",
          # Based on communication with the partner
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" &
            code_layer == "M36" ~ "P2,P4",
          # One ring based on communication with the partner
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" &
            code_layer == "M61" ~ "P2",
          # Based on communication with the partner
          plot_code_simple == "CULS__123__Slovakia_Sutovska_beech__NA" &
            code_layer == "M01" ~ "P1,P2,P3,P4",
          # Based on communication with the partner
          plot_code_simple == "CULS__153__Slovakia_Koprova dolina_spruce__NA" &
            code_layer == "M01" ~ "P2,P3,P5",
          # Probably, five rings were taken in total
          plot_code_simple == "FVA-BW__17__Schnapsried__1127" &
            code_layer %in% c("M36", "M61") ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "FVA-BW__543__Schnepfenmoos__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # 20 rings were taken, but fill in something less confusing
          plot_code_simple == "FVA-BW__602__Riedis__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "FVA-BW__62__Feldseewald__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "FVA-BW__621__Siedigkopf__NA" &
            code_layer %in% c("M36", "M61") ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "FVA-BW__9__Napf__1109" &
            code_layer %in% c("M36", "M61") ~ "P1,P1,P1,P1,P1",
          # Three rings instead of five (second sampling event)
          plot_code_simple == "LWF__119_a__Stachel__NA" &
            code_layer == "M36" ~ "P1,P1,P1",
          # Three rings instead of two (second sampling event)
          plot_code_simple == "LWF__65_a__Friedergries__NA" &
            code_layer == "M01" ~ "P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "NWFVA__03-019__Stoeberhai__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "NWFVA__03-044__Limker Strang__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "NWFVA__03-046__Koenigsbuche__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # Five rings
          plot_code_simple == "NWFVA__03-060__Meinsberg__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "NWFVA__03-089__Winterlieth__NA" &
            code_layer %in% c("M36", "M61") ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "NWFVA__06-014__Kniebrecht__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # Probably, five rings were taken in total
          plot_code_simple == "NWFVA__06-006__Niddahaenge__a" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # As confirmed by partner
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            code_layer %in% c("M01", "M13") ~ "P1,P2,P3,P4,P5",
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            code_layer == "M36" ~ "P1,P3,P4,P5",
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            code_layer == "M61" ~ "P1,P3,P4",
          # As confirmed by partner
          plot_code_simple == "VUK__5__Cahnov-Soutok__NA" &
            code_layer == "M01" ~ "P1,P3,P4,P5",
          # As confirmed by partner
          plot_code_simple == "WSL__21__Tariche Bois Banal__NA" &
            code_layer == "M01" ~ "P1,P2,P3,P4",
          # As confirmed by partner
          plot_code_simple == "WSL__25__Seeliwald__NA" &
            code_layer %in% c("M01", "M13") ~ "P1,P1,P1",
          # Note: The partner recalled collecting three rings for M36.
          # However, that is impossible (would give a bulk density of
          # 2622 kg m-3). After comparison with the PTF bulk density
          # (1151 kg m-3), it seems most likely that they took 5 rings
          # (1529 kg m-3). We therefore decided to assume 5 rings together
          # with the partner
          plot_code_simple == "WSL__25__Seeliwald__NA" &
            code_layer == "M36" ~ "P1,P1,P1,P1,P1",
          # As reported
          plot_code_simple == "WSL__39__Combe Biosse__NA" &
            code_layer %in% c("M01", "M36") ~ "P3,P3",
          # As confirmed by partner
          plot_code_simple == "WSL__4__Pfynwald__NA" &
            code_layer == "M01" ~ "P2,P3,P4,P5",
          # As confirmed by partner
          plot_code_simple == "WSL__53__Murgtal__NA" &
            code_layer == "M01" ~ "P1,P2,P3",
          # As confirmed by partner
          plot_code_simple == "WSL__6__St. Jean__NA" &
            code_layer == "M01" ~ "P1,P2,P7",
          TRUE ~ undist_sampling_points
        ),
        vol_ring = case_when(
          # NW-FVA used 20 x small cilinders of 5 cm3 instead of the
          # standard kopecky rings in case the latter were impossible in stony
          # sites.
          # ---
          # M01
          (code_layer == "M01" &
             plot_code_simple %in% c(
               "FVA-BW__10__Zweribach__NA", # Based on feedback partner
               "FVA-BW__1036__Nollenwald__NA", # Based on feedback partner
               "FVA-BW__588__Altlochkar-Rotwasser__NA")) |
            # M13
            (code_layer == "M13" &
               plot_code_simple %in% c(
                 "FVA-BW__14__Wildseemoor__1123",
                 "FVA-BW__15__Waldmoor-Torfstich__1125",
                 "FVA-BW__500__Barlochkar__NA",
                 "FVA-BW__510__Sturmlesloch__NA",
                 "FVA-BW__59__Teufelsries__NA",
                 "NWFVA__03-049__Haringer Berg__NA",
                 # Wattenberg und Hundsberg:
                 # the partner and central lab doubt about the information
                 # reported by the field surveyors. Therefore, it will
                 # anyway be replaced by the pedotransfer-predicted bulk
                 # density. The central lab also received stones that are too
                 # big for the 5.3 cm³ small rings, which makes it doubtful
                 # whether the latter were actually used.
                 "NWFVA__06-004__Wattenberg und Hundsberg__b",
                 "NWFVA__03-074__Butterberg_NI__NA", # Based on feedback partner
                 "NWFVA__03-014__Huenstollen__NA" # Based on feedback partner
               )) |
            # M36
            (code_layer == "M36" &
               plot_code_simple %in% c(
                 "FVA-BW__602__Riedis__NA")) ~ 5.3,
          TRUE ~ vol_ring)
      )

  } # End of "if apply_corrections"








  if (create_inconsistency_report) {

    ### INCONSISTENCY 3 ----

    rule <- paste0("The total number of reported undisturbed samples from the ",
                   "central pit (P1) and additional sampling points should not",
                   " exceed 5 (unlikely). Please check how many undisturbed ",
                   "samples you took at the given depth.")
    rule_id <- "3"


    # Records with a problem

    recs_problem <- df_rings %>%
      mutate(
        flag_implausible = case_when(
          # M36
          (code_layer == "M36" & !is.na(P1_num_M36_undist_samples) &
             (P1_num_M36_undist_samples > 1) &
             ((P1_num_M36_undist_samples + count_rings_orig) > 6)) |
            # M61
            (code_layer == "M61" & !is.na(P1_num_M61_undist_samples) &
               (P1_num_M61_undist_samples > 1) &
               ((P1_num_M61_undist_samples + count_rings_orig) > 6)) ~ TRUE,
          TRUE ~ FALSE),
        P1_num_undist_samples = case_when(
          code_layer == "M36" ~ P1_num_M36_undist_samples,
          code_layer == "M61" ~ P1_num_M61_undist_samples)) %>%
      filter(flag_implausible == TRUE)

    inc <- recs_problem %>%
      mutate(
        inconsistency_reason =
          paste0(rule,
                 " [P1: ", P1_num_undist_samples,
                 ", P2-5: ", count_rings_orig - 1, "]"),
        count_rings_p2to5 = count_rings_orig - 1) %>%
      # pivot longer to get one row per parameter
      pivot_longer(
        cols = c(P1_num_undist_samples, count_rings_p2to5),
        names_to = "parameter",
        values_to = "value"
      ) %>%
      left_join(attribute_catalogue %>%
                  rename(parameter_description = descr,
                         parameter_unit = unit),
                by = join_by("parameter" == "column_name")) %>%
      mutate(
        parameter_description = case_when(
          parameter == "P1_num_undist_samples" ~
            paste0("Number of undisturbed samples (e.g., Kopecky rings) taken ",
                   "in the pit in sampling point P1 for given depth"),
          parameter == "count_rings_p2to5" ~
            paste0("Number of undisturbed samples (e.g., Kopecky rings) taken ",
                   "across sampling points P2 to P5 for given depth"),
          TRUE ~ parameter_description),
        parameter_unit = case_when(
          parameter == "P1_num_undist_samples" ~ "-",
          parameter == "count_rings_p2to5" ~ "-",
          TRUE ~ parameter_description)) %>%
      relocate(parameter_description, parameter_unit, .after = parameter) %>%
      mutate(
        # mark unique_inconsistency = TRUE
        # only for the first parameter per record
        unique_inconsistency = (grepl("P1_num_undist_samples", parameter)),
        source = "Survey123 app",
        team = team_harmonized,
        plot_code_app = code,
        res_id_inst = res_id_harmonized,
        sample_code = code_layer,
        rule_id = rule_id) %>%
      mutate(value = as.character(value)) %>%
      select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
             parameter, parameter_description, parameter_unit, value,
             inconsistency_reason, unique_inconsistency, rule_id)

    inconsistencies <- bind_rows(
      inconsistencies,
      inc)


  } # End of "if create_inconsistency_report"














  # 6. Depth bedrock ----


  df_bedrock_wide <- app_data %>%
    select(code, plot_code_simple,
           composed_site_id, team_harmonized, globalid,
           res_id_harmonized, wp, site_type,
           matches("^P[1-9]_depth_bedrock$"))

  if (apply_corrections) {

    df_bedrock_wide <- df_bedrock_wide %>%
      # ---
      # Any MANUAL CORRECTIONS for bedrock depths per point should go here!
      # (manual corrections are based on comments in app, field photos,
      #  feedback by partners, expert assessment)
      # ---
      mutate(
        # For specific columns
        P1_depth_bedrock = case_when(
          # Based on comment (P1_notes_dist_samples)
          plot_code_simple ==
            "CULS__150__Slovakia_Bielovodska dolina_spruce__NA" ~ 40,
          # Based on comment (P1_notes_dist_samples)
          plot_code_simple ==
            "CULS__155__Slovakia_Tomanova dolina_spruce__NA" ~ 95,
          # P1_other_remarks_fragment: "The deep is 90cm because there was
          # weathered bedrock already"
          plot_code_simple == "CULS__170__Slovakia_Skalna Alpa_beech__NA" ~ 90,
          # Several German plots sampled by NW-FVA need to be corrected based
          # on feedback by the partner (plausibility in combination with photos,
          # field forms etc)
          plot_code_simple == "FVA-BW__510__Sturmlesloch__NA" ~ 20,
          plot_code_simple == "FVA-BW__14__Wildseemoor__1123" ~ 20,
          plot_code_simple == "FVA-BW__543__Schnepfenmoos__NA" ~ 40,
          plot_code_simple == "NWFVA__03-041__Vogelherd__NA" ~ 80,
          plot_code_simple == "NWFVA__06-006__Niddahaenge__a" ~ 46,
          plot_code_simple == "NWFVA__03-044__Limker Strang__NA" ~ 50,
          plot_code_simple == "NWFVA__03-017__Grosser Staufenberg__NA" ~ 30,
          plot_code_simple == "FVA-BW__602__Riedis__NA" ~ 38,
          plot_code_simple == "NWFVA__03-074__Butterberg_NI__NA" ~ NA_real_,
          plot_code_simple == "NWFVA__03-014__Huenstollen__NA" ~ 20,
          plot_code_simple == "NWFVA__03-046__Koenigsbuche__NA" ~ 50,
          plot_code_simple == "LWF__100_a__Donauhaenge__NA" ~ 25,
          # Partner mentioned 40 cm, but the pit is 50 cm deep
          plot_code_simple == "LWF__119_a__Stachel__NA" ~ 50,
          # Partner mentioned 10 cm, but the pit is 40 cm deep
          plot_code_simple == "LWF__60_a__Echinger Lohe__NA" ~ 40,
          plot_code_simple == "LWF__160_a__Mittelberg__NA" ~ 25,
          plot_code_simple == "LWF__65_a__Friedergries__NA" ~ 40,
          plot_code_simple == "LWF__57_a__Groppenhofer und Rieder Leite__NA" ~
            35,
          TRUE ~ P1_depth_bedrock
        ),

        P2_depth_bedrock = case_when(
          # Based on other sampling points (probably forgotten)
          # Note: in the end, non-NA P[2-9] bedrock depths for FVA-BW will often
          # be replaced by NA in this R script, based on feedback of the partner
          # (probably, the bedrock depth of P1 was just copied to other points)
          plot_code_simple == "FVA-BW__500__Barlochkar__NA" ~ 23,
          # Comment P2_notes_dist_samples: "M01 horizont only 5cm deep"
          plot_code_simple == "WSL__11__Weidwald__NA" ~ 5,
          TRUE ~ P2_depth_bedrock
        ),

        P3_depth_bedrock = case_when(
          # Based on comment (P3_notes_dist_samples)
          plot_code_simple == "CULS__121__Slovakia_Kornietova_beech__NA" ~ 50,
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" ~ 60,
          plot_code_simple == "CULS__176__Slovakia_Ploska_thermophilic__NA" ~
            70,
          # Based on other sampling points (probably forgotten)
          # Note: in the end, non-NA P[2-9] bedrock depths for FVA-BW will often
          # be replaced by NA in this R script, based on feedback of the partner
          # (probably, the bedrock depth of P1 was just copied to other points)
          plot_code_simple == "FVA-BW__10__Zweribach__NA" ~ 10,
          plot_code_simple == "FVA-BW__1036__Nollenwald__NA" ~ 10,
          plot_code_simple == "FVA-BW__500__Barlochkar__NA" ~ 23,
          plot_code_simple == "FVA-BW__59__Teufelsries__NA" ~ 30,
          # As confirmed by partner
          plot_code_simple == "UL__7__Menina__NA" ~ 30,
          # Comment by partner: "Bedrock at 55" (stony site). Bedrock is
          # reported to be at the surface (0 cm) for P1, P2, P5, P6, P7.
          # Therefore, instead of P1, replace first NA (P3) by 55.
          plot_code_simple == "WSL__39__Combe Biosse__NA" ~ 55,
          TRUE ~ P3_depth_bedrock
        ),

        P4_depth_bedrock = case_when(
          # Based on comment (P4_notes_dist_samples: "Not possible to dig this
          # point"). For other points, they went all the way down to 100 cm
          # under extremely stony conditions (85 %), so "not possible" must mean
          # "bedrock"
          plot_code_simple == "CULS__153__Slovakia_Koprova dolina_spruce__NA" ~
            0,
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" ~ 60,
          # Based on other sampling points (probably forgotten)
          # Note: in the end, non-NA P[2-9] bedrock depths for FVA-BW will often
          # be replaced by NA in this R script, based on feedback of the partner
          # (probably, the bedrock depth of P1 was just copied to other points)
          plot_code_simple == "FVA-BW__1036__Nollenwald__NA" ~ 10,
          plot_code_simple == "FVA-BW__500__Barlochkar__NA" ~ 23,
          # As confirmed by partner
          plot_code_simple == "UL__1__Sumik__NA" ~ 30,
          plot_code_simple == "UL__5__Rajhenav__NA" ~ 30,
          TRUE ~ P4_depth_bedrock
        ),

        P5_depth_bedrock = case_when(
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" ~ 70,
          # Based on other sampling points (probably forgotten)
          # Note: in the end, non-NA P[2-9] bedrock depths for FVA-BW will often
          # be replaced by NA in this R script, based on feedback of the partner
          # (probably, the bedrock depth of P1 was just copied to other points)
          plot_code_simple == "FVA-BW__59__Teufelsries__NA" ~ 30,
          # As confirmed by partner
          plot_code_simple == "UL__1__Sumik__NA" ~ 30,
          plot_code_simple == "UL__7__Menina__NA" ~ 30,
          TRUE ~ P5_depth_bedrock
        ),

        P6_depth_bedrock = case_when(
          # As confirmed by partner
          plot_code_simple == "UL__7__Menina__NA" ~ 30,
          TRUE ~ P6_depth_bedrock
        ),

        P7_depth_bedrock = case_when(
          # Based on comment (P7_notes_dist_samples)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F038" ~
            0,
          # Based on other sampling points (probably forgotten)
          # Note: in the end, non-NA P[2-9] bedrock depths for FVA-BW will often
          # be replaced by NA in this R script, based on feedback of the partner
          # (probably, the bedrock depth of P1 was just copied to other points)
          plot_code_simple == "FVA-BW__10__Zweribach__NA" ~ 10,
          # As confirmed by partner
          plot_code_simple == "UL__1__Sumik__NA" ~ 30,
          TRUE ~ P7_depth_bedrock
        ),

        P9_depth_bedrock = case_when(
          plot_code_simple == "CULS__169__Slovakia_Padva_beech__NA" ~ 15,
          plot_code_simple == "FVA-BW__588__Altlochkar-Rotwasser__NA" ~ 10,
          # Based on other sampling points (probably forgotten)
          # Note: in the end, non-NA P[2-9] bedrock depths for FVA-BW will often
          # be replaced by NA in this R script, based on feedback of the partner
          # (probably, the bedrock depth of P1 was just copied to other points)
          plot_code_simple == "FVA-BW__500__Barlochkar__NA" ~ 23,
          TRUE ~ P9_depth_bedrock
        )) %>%
      mutate(
        across(
          matches("^P[1-9]_depth_bedrock$"),
          ~ ifelse(
            plot_code_simple %in% c(
              # Partner recommended to replace original bedrock depth (7.5 cm)
              # by 0 cm
              "WSL__23__Boedmerenwald__NA",
              # The same seems reasonable for another WSL plot without
              # below-ground samples
              # (bedrock depth originally between 2 - 45 cm, but they noted:
              # "No mineral soil layer, thin OL, very thick OFH and
              #  bedrock under"
              #  and this seems correct based on the photos)
              "WSL__40__La Niva__NA"),
            0,
            .x
          )
        )
      )


  } # End of "if apply_corrections"


  df_bedrock_wide <- df_bedrock_wide %>%
    rowwise() %>%
    mutate(
      all_same = {
        vals <- c_across(matches("^P[2-9]_depth_bedrock$"))
        all(!is.na(vals)) && length(unique(vals)) == 1 &&
          # Exception: the very shallow profiles (no M01):
          # Faulbach and Boedmerenwald
          vals[1] >= 10
      }
    ) %>%
    ungroup()












  if (create_inconsistency_report) {

    ### INCONSISTENCY 5 ----

    rule <- paste0("Bedrock depth is missing (NA) in some sampling points ",
                   "(P1-5), while others report shallow bedrock. Please ",
                   "confirm that all missing values are correctly marked as ",
                   "NA (i.e., bedrock below 100 cm)")
    rule_id <- "5"



    # Records with a problem

    recs_problem <- df_bedrock_wide %>%
      # The potential problem with the bedrock depths:
      # It is not a mandatory parameter, because of which it may be not be
      # filled in (for some sampling points).
      # → Question: does NA mean "no bedrock in upper 100 cm" or
      # "bedrock in upper 100 cm but depth not filled in by partner"?
      # There are some suspicious cases, e.g. Zweribach (NW-FVA),
      # Nollenwald (NW-FVA)
      # However, the bedrock appearance within a site can be very variable,
      # e.g., Hadecka planinka (VUK): 34, NA, 37, 37, NA. This is known to be
      # correct.
      mutate(
        # How many bedrock depths are filled in?
        n_filled = sum(!is.na(c_across(matches("^P[1-5]_depth_bedrock$")))),
        # Are all non-missing bedrock depths ≤ 30?
        # (This is also TRUE for records with only NAs - can be ignored)
        all_shallow = all(c_across(matches("^P[1-5]_depth_bedrock$"))[!is.na(
          c_across(matches("^P[1-5]_depth_bedrock$")))] <= 30),
        # How many bedrock depths are missing?
        n_missing = sum(is.na(c_across(matches("^P[1-5]_depth_bedrock$")))),
        # Suspicious if at least 2 bedrock depths are filled in and at least 1
        # is NA, with all of the filled bedrock depths being shallower than 30
        # cm?
        flag_suspicious = n_filled >= 2 & all_shallow & n_missing > 0) %>%
      filter(flag_suspicious == TRUE)

    inc <- recs_problem %>%
      mutate(
        inconsistency_reason = rule) %>%
      # pivot longer to get one row per parameter
      pivot_longer(
        cols = matches("^P[1-5]_depth_bedrock$"),
        names_to = "parameter",
        values_to = "value"
      ) %>%
      left_join(attribute_catalogue %>%
                  rename(parameter_description = descr,
                         parameter_unit = unit),
                by = join_by("parameter" == "column_name")) %>%
      relocate(parameter_description, parameter_unit, .after = parameter) %>%
      mutate(
        # mark unique_inconsistency = TRUE
        # only for the first parameter per record
        unique_inconsistency = (grepl("^P1", parameter)),
        source = "Survey123 app",
        team = team_harmonized,
        plot_code_app = code,
        res_id_inst = res_id_harmonized,
        sample_code = NA_character_,
        rule_id = rule_id) %>%
      mutate(value = as.character(value)) %>%
      select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
             parameter, parameter_description, parameter_unit, value,
             inconsistency_reason, unique_inconsistency, rule_id)

    inconsistencies <- bind_rows(
      inconsistencies,
      inc)




  ### INCONSISTENCY 4 ----


    rule <- paste0("Identical bedrock depth values are reported across ",
                   "sampling points (P2–9). This suggests the values may have ",
                   "been copied from P1 rather than independently observed. ",
                   "Please confirm how to interpret this.")

    rule_id <- "4"


    # Records with a problem

    recs_problem <- df_bedrock_wide %>%
      filter(all_same)

    inc <- recs_problem %>%
      pivot_longer(
        cols = matches("P\\d{1}_"),
        names_to = "parameter",
        values_to = "value"
      ) %>%
      mutate(
        inconsistency_reason = rule
        ) %>%
      left_join(attribute_catalogue %>%
                  rename(parameter_description = descr,
                         parameter_unit = unit),
                by = join_by("parameter" == "column_name")) %>%
      relocate(parameter_description, parameter_unit, .after = parameter) %>%
      mutate(
        # mark unique_inconsistency = TRUE
        # only for the first parameter per record
        unique_inconsistency = (grepl("P1", parameter)),
        source = "Survey123 app",
        team = team_harmonized,
        plot_code_app = code,
        res_id_inst = res_id_harmonized,
        sample_code = NA_character_,
        rule_id = rule_id) %>%
      mutate(value = as.character(value)) %>%
      select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
             parameter, parameter_description, parameter_unit, value,
             inconsistency_reason, unique_inconsistency, rule_id)

    inconsistencies <- bind_rows(
      inconsistencies,
      inc)

  } # End of "if create_inconsistency_report"








  if (apply_corrections) {

    df_bedrock_wide <- df_bedrock_wide %>%
      mutate(
        across(
          matches("^P[2-9]_depth_bedrock$"),
          ~ if_else(
            all_same | plot_code_simple %in% c(
              "FVA-BW__14__Wildseemoor__1123",
              # Considering mismatch between reported bedrock depth and
              # code_layer for which it was reported + impossible to know
              # for the given sampling point + as also suspected by the partner
              "FVA-BW__621__Siedigkopf__NA"),
            NA_real_,
            .x
          )
        )
      )

  } # End of "if apply_corrections"





  ## Summarise bedrock depths into one value per plot ----

  # Data preparations (for censoring):
  # Depths should represent the observed depth (in metres) or lower censoring
  # bound.
  # A censoring column must be added, indicating either "right" or "none"

  df_bedrock_cens <- df_bedrock_wide %>%
    select(-all_same) %>%
    filter(wp == "WP2") %>%
    select(
      plot_code_simple, team_harmonized, site_type,
      matches("P\\d{1}_")
    ) %>%
    # 1. Add the depth until which (un)disturbed samples have been collected
    # (for censoring from that depth onwards if shallow)
    left_join(
      df_layers %>%
        filter(!code_layer %in% c("OL", "OFH")) %>%
        mutate(
          depth_bottom = case_when(
            code_layer == "M61" ~ 100,
            code_layer == "M36" ~ 60,
            code_layer == "M13" ~ 30,
            code_layer == "M01" ~ 10
          ),
          valid = !(grepl("not", dist_check) & grepl("not", bulk_den_check))
        ) %>%
        group_by(plot_code_simple) %>%
        reframe(
          depth_reached = if (any(valid, na.rm = TRUE)) {
            max(depth_bottom[valid], na.rm = TRUE)
          } else {
            0
          }
        ),
      by = "plot_code_simple") %>%
    # Transform to long format (per sampling point)
    pivot_longer(
      cols = matches("P\\d{1}_"),
      names_to = "p_id",
      values_to = "depth"
    ) %>%
    mutate(
      p_id = gsub("_depth_bedrock", "", p_id)) %>%
    # 2. Add maximum depth reached with undisturbed sampling per sampling point
    #    (assuming that data are correctly reported)
    left_join(
      df_rings %>%
        select(plot_code_simple, code_layer, undist_sampling_points) %>%
        mutate(undist_sampling_points =
                 strsplit(as.character(undist_sampling_points), ",")) %>%
        unnest(undist_sampling_points) %>%
        rename(p_id = undist_sampling_points) %>%
        mutate(
          depth_bottom = case_when(
            code_layer == "M01" ~ 10,
            code_layer == "M13" ~ 30,
            code_layer == "M36" ~ 60,
            code_layer == "M61" ~ 100)
        ) %>%
        group_by(plot_code_simple, p_id) %>%
        reframe(
          depth_reached_undist = max(depth_bottom, na.rm = TRUE)) %>%
        # Complete cases for which no undisturbed samples could be collected
        complete(
          plot_code_simple,
          p_id = paste0("P", 1:5),
          fill = list(depth_reached_undist = 0)
        ),
      by = join_by("plot_code_simple", "p_id")
    ) %>%
    relocate(depth_reached_undist, .after = "depth_reached") %>%
    mutate(
      # Theoretical depths to be covered across sampling points for standard
      # and stony sites
      depth_theor = case_when(
        site_type == "Standard site" & p_id %in% paste0("P", 1:5) ~ 100,
        site_type == "Standard site" & !p_id %in% paste0("P", 1:5) ~ 30,
        site_type == "Stony site" & p_id %in% paste0("P", 1) ~ 100,
        site_type == "Stony site" & !p_id %in% paste0("P", 1) ~ 30),
      censoring = case_when(
        !is.na(depth) ~ "none",
        # Temporary set remaining depth NA cases to right-censored
        # For modelling with a beta distribution (in meters), the data are
        # right-censored
        TRUE ~ "right"
      ))


  if (apply_corrections) {

    df_bedrock_cens <- df_bedrock_cens %>%
      mutate(
        # Replace depth NA by censored value
        depth = case_when(
          !is.na(depth) ~ depth,
          # NA cases for which they have not (been able to) collect(ed) samples
          # until the theoretical depth to be covered:
          # use the deepest depth that was reached
          # Based on information in app data ---
          # If the partner indicated (in comments) that:
          # - they could/did not go deeper than a certain depth (that is
          #   shallower than the theoretical depths for the different sampling
          #   points in standard and stony sites), without reporting bedrock.
          #   This can be due to stoniness, compaction, soil being either too
          #   loose or too saturated to stay inside a gouge auger.
          #   Notes:
          #   · based on explicit comments or explicitly clear from the profile
          #     pit photo. "reason_bulk_den" with "Present bedrock" is not
          #     sufficient evidence (they may have gone deeper with disturbed
          #     samples)
          #   · without a profile pit, it is usually not sure whether
          #     blocking of a gouge auger is caused by the presence of coarse
          #     fragments or that of bedrock.
          #   · this assessment is a bit arbitrary (maybe not always
          #     indicated in the app). Mostly depends from partner to partner.
          #     E.g., CULS indicated "Not possible to dig this point" at
          #     P4_notes_dist_samples. For other points, they went all the way
          #     down to 100 cm under extremely stony conditions (85 %), so "not
          #     possible" must mean "bedrock" (uncensored).
          # - they could go deeper than the theoretical depth (e.g., until 100 cm
          #   at P[2-9] of stony sites)
          plot_code_simple == "AlberIT - UNIRC__22__Monte Tranquillo__NA" &
            p_id == "P3" ~ 15,
          plot_code_simple == "AlberIT - UNIRC__22__Monte Tranquillo__NA" &
            p_id == "P4" ~ 60,
          plot_code_simple == "AlberIT - UNIRC__24__Macchia dell'Orso__NA" &
            p_id %in% c("P2", "P4") ~ 35,
          plot_code_simple == "AlberIT - UNIRC__24__Macchia dell'Orso__NA" &
            p_id == "P3" ~ 25,
          plot_code_simple == "AlberIT - UNIRC__24__Macchia dell'Orso__NA" &
            p_id == "P5" ~ 65,
          plot_code_simple == "BFNP__IP_10__IP_Hohensteingehaenge__NA" &
            p_id %in% c("P4", "P5") ~ 100,
          plot_code_simple == "BFNP__IP_5__IP_Feistenberg__NA" &
            p_id %in% c("P4", "P5") ~ 100,
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F059" &
            p_id %in% c("P2", "P3") ~ 20,
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F138" &
            p_id %in% c("P2", "P4", "P6", "P9") ~ 10,
          plot_code_simple == "DISAFA - UNITO__13__Lago Perso__NA" &
            p_id == "P2" ~ 20,
          plot_code_simple == "DISAFA - UNITO__13__Lago Perso__NA" &
            p_id %in% paste0("P", c(3, 6, 7, 8, 9)) ~ 10,
          plot_code_simple == "DISAFA - UNITO__15__Alpe Devero__NA" ~ 10,
          plot_code_simple == "DISAFA - UNITO__4__Lom__NA" & p_id == "P1" ~ 30,
          plot_code_simple == "DISAFA - UNITO__9__Paneveggio__NA" &
            p_id == "P1" ~ 30,
          plot_code_simple == "DISAFA - UNITO__9__Paneveggio__NA" &
            p_id %in% c("P2", "P3", "P4") ~ 20,
          plot_code_simple == "DISAFA - UNITO__9__Paneveggio__NA" &
            p_id == "P5" ~ 15,
          plot_code_simple == "FVA-BW__621__Siedigkopf__NA" & p_id == "P1" ~ 70,
          plot_code_simple == "LWF__160_a__Mittelberg__NA" &
            p_id %in% paste0("P", 2:8) ~ 30, # M36 only collected in P9
          plot_code_simple == "NWFVA__03-074__Butterberg_NI__NA" &
            p_id == "P1" ~ 20, # Seems deeper than 20 cm
          plot_code_simple == "UCPH__7__Stroedam__NA" &
            p_id %in% paste0("P", 1:5) ~ 60,
          plot_code_simple == "UCPH__8__Suserup__NA" & p_id == "P3" ~ 70,
          plot_code_simple == "UCPH__8__Suserup__NA" & p_id == "P4" ~ 75,
          plot_code_simple == "UNITBV__4__Domogled__NA" & p_id == "P2" ~ 80,
          plot_code_simple == "UNITBV__4__Domogled__NA" & p_id == "P5" ~ 75,
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            p_id == "P2" ~ 30,
          plot_code_simple == "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN" &
            p_id == "P5" ~ 60,
          plot_code_simple == "UNIUD_EXTRA__1__Cansiglio__AF-MN" &
            p_id == "P3" ~ 60,
          plot_code_simple == "WSL__51__Tamangur__NA" &
            p_id %in% c("P7", "P9") ~ 0,
          TRUE ~ NA_real_))
  } # End of "if apply_corrections"


  df_bedrock_cens <- df_bedrock_cens %>%
    left_join(
      df_plot %>%
        select(plot_code_simple, slope_deg),
      by = "plot_code_simple"
    ) %>%
    mutate(
      # Replace depth NA by censored value
      depth = case_when(
        !is.na(depth) ~ depth,
        # General scenarios:
        # 1. Depth reached with undisturbed sampling (per sampling point)
        #    was deeper than theoretical depth
        #    (We only have this sampling point-differentiated informatio
        #     systematically for undisturbed samples, but if they reached
        #     deeper with undisturbed samples, then bedrock was for sure not
        #     shallower than that depth that was reached at the given point)
        depth_reached_undist > depth_theor ~ depth_reached_undist,
        # 2. Overall depth reached in plot is shallower than theoretical depth
        #    (e.g., Nollenwald, Zweribach)
        depth_reached < depth_theor ~ depth_reached,
        TRUE ~ depth_theor
        ),
      # Depths above are plumb-vertical (all measured depths are assumed to
      # be measured plumb-vertically following the instructions in the soil
      # sampling protocol). The final depth of the stock is
      # 100 cm ortogonally to the local slope gradient, i.e. 100 / cos(slope)
      # cm plumb-vertically. In order to simplify the Bayesian approach with
      # Beta distribution, let's use the summarise_bedrock_depths script
      # only with ortogonal depths, so that the maximum is 1 (100 cm).
      # Therefore, temporarily convert the depths to their ortogonal
      # counterpart.
      depth_ort = depth * cos(slope_deg * pi / 180)
      ) %>%
    mutate(
      # Convert to m
      depth_ort_m = depth_ort / 100,
      # When depth_ort is 100, there is no censoring for SOC stock in 100 cm
      # Note: plots for which default depths of 100 cm were reached across
      # P1-5, but with a slope > 0, will need censoring also for P1-5.
      # For example, with a slope of 20 °C, the stock needs to go until 106 cm
      # (plumb-vertically) while they went to 100 cm only.
      censoring = case_when(
        depth_ort >= 99.5 ~ "none",
        TRUE ~ censoring),
      # Boundary-avoiding transformation
      depth_ort_m = case_when(
        depth_ort_m < 0.005 ~ 0.005,
        depth_ort_m > 0.995 ~ 0.995,
        TRUE ~ depth_ort_m
      ))











  # Summarise bedrock depths using BRMS
  # Remove files in './models/bedrock_brms/' if needed, as changing the iter
  # value does not always trigger refitting of the brms model.

  source("./src/functions/summarise_bedrock_depths.R")
  df_bedrock_summ <-
    summarise_bedrock_depths(df_bedrock = df_bedrock_cens,
                             iter = 2000,
                             correction_status =
                               if (apply_corrections) {"with_corrections"} else
                                 {"without_corrections"})


  df_bedrock <- df_bedrock_wide %>%
    filter(wp == "WP2") %>%
    select(-all_same) %>%
    arrange(team_harmonized) %>%
    rowwise() %>%
    mutate(
      depth_bedrock_compiled = paste(
        as.character(round(c_across(matches("^P[1-9]_depth_bedrock$")))),
        collapse = ",")
      ) %>%
    ungroup() %>%
    # Add censoring information
    left_join(
      df_bedrock_cens %>%
        pivot_wider(
          id_cols = c(plot_code_simple, site_type, depth_reached, slope_deg),
          names_from = p_id,
          values_from = c(depth, censoring)
        ) %>%
        rowwise() %>%
        mutate(
          depth_bedrock_cens_compiled = paste(
            as.character(round(c_across(matches("^depth_P[1-9]$")))),
            collapse = ","
          ),
          censoring_compiled = paste(
            c_across(matches("^censoring_P[1-9]$")),
            collapse = ","
          )
        ) %>%
        ungroup() %>%
        select(plot_code_simple, depth_reached, slope_deg,
               contains("compiled")),
      by = "plot_code_simple"
    ) %>%
    # Add summarised bedrock depth (posterior mean and 95 % credible interval)
    left_join(
      df_bedrock_summ %>%
        mutate(depth_bedrock = .epred,
               depth_bedrock_min = .lower,
               depth_bedrock_max = .upper
               ) %>%
        select(plot_code_simple, starts_with("depth_bedrock")),
      by = "plot_code_simple") %>%
    mutate(
      default = (
        (site_type == "Standard site" &
           depth_bedrock_compiled == "NA,NA,NA,NA,NA,NA,NA,NA,NA" &
           depth_bedrock_cens_compiled == "100,100,100,100,100,30,30,30,30" # &
           # censoring_compiled ==
           # "none,none,none,none,none,right,right,right,right"
         ) |
          (site_type == "Stony site" &
             depth_bedrock_compiled == "NA,NA,NA,NA,NA,NA,NA,NA,NA" &
             depth_bedrock_cens_compiled == "100,30,30,30,30,30,30,30,30" # &
             # censoring_compiled ==
             # "none,right,right,right,right,right,right,right,right"
           )),
      # Final depth and uncertainty ranges (converted back to plumb-vertical)
      depth_bedrock_min = case_when(
        site_type == "Standard site" & default == TRUE ~
          # Take the mean of all standard site records without non-NA depths
          # and with a small slope (else, there is already some effect of
          # the cosine conversion to ortogonal depths)
          round(1E2 * mean(
            depth_bedrock_min[
              site_type == "Standard site" & default == TRUE & slope_deg <= 5],
            na.rm = TRUE) / cos(slope_deg * pi / 180)
            ),
        site_type == "Stony site" & default == TRUE ~
          # Take the mean of all stony site records without non-NA depths
          # and with a small slope (else, there is already some effect of
          # the cosine conversion to ortogonal depths)
          round(1E2 * mean(
            depth_bedrock_min[
              site_type == "Stony site" & default == TRUE & slope_deg <= 5],
            na.rm = TRUE) / cos(slope_deg * pi / 180)),
        depth_bedrock_compiled == "0,0,0,0,0,0,0,0,0" ~ 0,
        TRUE ~ round(1E2 * depth_bedrock_min / cos(slope_deg * pi / 180))
        ),
      depth_bedrock_max = case_when(
        site_type %in% c("Standard site", "Stony site") &
          depth_bedrock_compiled == "NA,NA,NA,NA,NA,NA,NA,NA,NA" ~
          round(100 / cos(slope_deg * pi / 180)),
        TRUE ~ round(1E2 * depth_bedrock_max / cos(slope_deg * pi / 180))
        ),
      depth_bedrock = case_when(
        # Take 100 / cos(slope) for records without non-NA depths
        # (both for standard and stony sites)
        site_type %in% c("Standard site", "Stony site") &
          default == TRUE ~ round(100 / cos(slope_deg * pi / 180)),
        depth_bedrock_compiled == "0,0,0,0,0,0,0,0,0" ~ 0,
        TRUE ~ round(1E2 * depth_bedrock / cos(slope_deg * pi / 180))
      )) %>%
    select(-matches("^P[1-9]_depth_bedrock$")) %>%
    left_join(
      df_plot %>%
        select(plot_code_simple,
               wrb_ref_soil_group,
               wrb_qualifier_1),
      by = "plot_code_simple") %>%
    relocate(
      wrb_ref_soil_group, wrb_qualifier_1, .after = "site_type") %>%
    relocate(ends_with("compiled"), .after = last_col())

  if (apply_corrections) {

    df_bedrock <- df_bedrock %>%
      mutate(
        depth_bedrock = case_when(
          # Explicitly mentioned in "P1_other_remarks_fragment"
          # "Soil depth 74 cm"
          # (this is plot-level because different depth reported for P1)
          # (assumed to be measured plumb-vertically)
          plot_code_simple == "UL__4__Ravna gora__NA" ~ 74,
          TRUE ~ depth_bedrock
          ))

  } # End of "if apply_corrections"








  # 7. Compile all layer-specific data ----

  df_layers_all <- df_layers %>%
    left_join(df_properties,
              by = join_by(code, code_layer)) %>%
    left_join(df_ff_summ %>%
                select(-plot_code_simple, -institute_sampling),
              by = join_by(code, code_layer)) %>%
    left_join(df_rings %>%
                select(
                  code, code_layer,
                  vol_ring, count_rings, undist_sampling_points),
              by = join_by(code, code_layer)) %>%
    # Add average bedrock depth in order to update the lowest layer limit
    # of the lowest layer of the profile
    left_join(df_bedrock %>%
                select(code, site_type,
                       depth_bedrock, depth_bedrock_min, depth_bedrock_max,
                       depth_bedrock_compiled, depth_bedrock_cens_compiled,
                       depth_reached),
              by = "code") %>%
    select(-P1_num_undist_samples) %>%
    relocate(site_type, .after = "code_layer") %>%
    relocate(tamass, .before = dist) %>%
    relocate(
      contains("edna"), contains("back_up"), contains("_sampleID"),
      .after = last_col()) %>%
    relocate(contains("depth_bedrock"), .after = "P1_coaf_50mm")







  ### INCONSISTENCY 6 ----

  if (create_inconsistency_report) {

    rule <- paste0("Volumetric percentage of coarse fragments > 50 mm should ",
                   "not exceed that > 2 mm. You may have only indicated ",
                   "coarse fragments between 2–50 mm under > 2 mm.")

    rule_id <- "6"


    # Records with a problem

    recs_problem <- df_layers_all %>%
      filter(P1_coaf_50mm > P1_coaf_2mm)

    inc <- recs_problem %>%
      mutate(
        inconsistency_reason = rule) %>%
      # pivot longer to get one row per parameter
      pivot_longer(
        cols = c(P1_coaf_2mm, P1_coaf_50mm),
        names_to = "parameter",
        values_to = "value"
      ) %>%
      mutate(
        parameter = case_when(
          grepl("2mm", parameter) ~ #P1_m01coaf_2mm
            paste0("P1_", tolower(code_layer), "coaf_2mm"),
          grepl("50mm", parameter) ~ #P1_m01coaf_2mm
            paste0("P1_", tolower(code_layer), "coaf_50mm"))) %>%
      left_join(attribute_catalogue %>%
                  rename(parameter_description = descr,
                         parameter_unit = unit),
                by = join_by("parameter" == "column_name")) %>%
      relocate(parameter_description, parameter_unit, .after = parameter) %>%
      mutate(
        # mark unique_inconsistency = TRUE
        # only for the first parameter per record
        unique_inconsistency = (grepl("2mm", parameter)),
        source = "Survey123 app",
        team = team_harmonized,
        plot_code_app = code,
        res_id_inst = res_id_harmonized,
        sample_code = code_layer,
        rule_id = rule_id) %>%
      mutate(value = as.character(value)) %>%
      select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
             parameter, parameter_description, parameter_unit, value,
             inconsistency_reason, unique_inconsistency, rule_id)

    inconsistencies <- bind_rows(
      inconsistencies,
      inc)

  } # End of "if create_inconsistency_report"








  if (apply_corrections) {

    df_layers_all <- df_layers_all %>%
      # ---
      # Any MANUAL CORRECTIONS for the coarse fragment contents should go here!
      # (manual corrections are based on comments in app, field photos,
      #  feedback by partners, expert assessment)
      # ---
      mutate(
        P1_coaf_2mm = case_when(
          # Values 2 mm swapped with values 50 mm
          plot_code_simple == "INBO__17__Kluisbos__NA" &
            code_layer %in% c("M13", "M36", "M61") ~ 3,
          plot_code_simple == "INCDS__1__Runcu__NA" & code_layer == "M36" ~ 40,
          # Swapped (confirmed by partner)
          plot_code_simple == "VUK__102__Doutnac__NA" &
            code_layer == "M13" ~ 80,
          # Mistake in app submission (rare anthropogenic slag)
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__Haras" &
            grepl("^M", code_layer) ~ 0,
          # WSL assumed that P1_coaf_2mm represents vol% 2-50 mm.
          # One record is an exception to this (although they did not go
          # that deep in this plot, so it will be replaced by some default
          # if below depth bedrock)
          plot_code_simple == "WSL__51__Tamangur__NA" &
            code_layer == "M61" ~ 0,
          TRUE ~ P1_coaf_2mm),
        P1_coaf_50mm = case_when(
          # Values 2 mm swapped with values 50 mm
          plot_code_simple == "INBO__17__Kluisbos__NA" &
            code_layer %in% c("M13", "M36", "M61") ~ 1,
          plot_code_simple == "INCDS__1__Runcu__NA" & code_layer == "M36" ~ 30,
          # Swapped (confirmed by partner)
          plot_code_simple == "VUK__102__Doutnac__NA" &
            code_layer == "M13" ~ 70,
          # Mistake in app submission (rare anthropogenic slag)
          plot_code_simple ==
            "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__Haras" &
            grepl("^M", code_layer) ~ 0,
          # Partner reported bedrock to appear at 95 cm but with 20 % cracks,
          # below which they still took samples.
          # Solution: consider bedrock depth NA, and change the vol% coarse
          # fragments of M36. From 75 to 95 cm: 0 % coarse fragments; from
          # 95 to 100 cm: 80 % coarse fragments → weighted average of 16 %
          # coarse fragments
          plot_code_simple == "WSL__25__Seeliwald__NA" & code_layer == "M36" ~
            16,
          TRUE ~ P1_coaf_50mm)
      ) %>%
      # Correct P1_coaf_2mm in case the partner wrongly interpreted it
      # as vol% 2-50 mm
      # ---
      # Does "P1_coaf_2mm" accidentally represent "vol% 2-50 mm" instead of
      # "vol% >2 mm" for all plots of the given sampling partner?
      # Based on manual checking:
        # BFNP: YES (confirmed by partner)
        # BGD-NP: correct (confirmed by partner and sum >100 vol%)
        # LWF: YES (confirmed by partner)
        # USV: correct (confirmed by partner)
        # CULS: YES (confirmed by partner)
        # UCPH: correct (confirmed by partner)
        # IBL: correct (confirmed by partner)
        # IBER-BAS: YES (confirmed by partner)
        # INBO: correct
        # NWFVA: correct (confirmed by sum >100 vol%)
        # FVA-BW: correct (confirmed by sum >100 vol%)
        # SFI: correct (confirmed by partner and sum >100 vol%)
        # UNITBV: correct (confirmed by partner and sum >100 vol%)
        # UNITO (incl. AlberIT - UNIRC): YES (confirmed by partner)
        # UNIUD: YES (confirmed by partner)
        # VUK: correct
        #   (confirmed by partner and sum >100 vol%, one record swapped values)
        # WR: YES (confirmed by partner)
        # WSL: YES (confirmed by partner)
      mutate(
        P1_coaf_2mm = case_when(
          institute_sampling %in% c(
            "AlberIT - UNIRC", "BFNP", "CULS", "IBER-BAS", "LWF",
            "UNITO", "UNIUD", "WR", "WSL") ~
            coalesce(P1_coaf_2mm, 0) + coalesce(P1_coaf_50mm, 0),
          TRUE ~ P1_coaf_2mm)
      ) %>%
      # ---
      # Any MANUAL CORRECTIONS for the mass of field-moist (sub)samples should
      # go here!
      # (manual corrections are based on comments in app, field photos,
      #  feedback by partners, expert assessment)
      # ---
      left_join(
        # For BGD-NP, "dist" masses include masses of bags. Subtract those
        # (they differ per sample)
        data.frame(
          res_id = rep(c("F016","F066","F035","F099","F036",
                         "F138","F048","F038","F073","F023","F059"), each = 2),
          code_layer = rep(c("OL","OFH"), times = 11),
          mass_bag_field = c(
            11.49, 11.68,
            11.71, 12.32,
            12.10, 12.52,
            11.91, 12.10,
            11.24, 12.21,
            18.32, 18.05,
            13.20, 20.05,
            15.06, 18.36,
            12.15, 11.96,
            11.35, 11.20,
            19.96, 19.56)) %>%
          mutate(
            plot_code_simple =
              paste0("BGD-NP__1__Berchtesgaden National Park__", res_id)) %>%
          select(-res_id),
        by = join_by("plot_code_simple", "code_layer")
        ) %>%
      mutate(
        bulk_den = case_when(
          # Second sampling event LWF
          plot_code_simple == "LWF__119_a__Stachel__NA" &
            code_layer == "M01" ~ 632, # Dry: 565
          plot_code_simple == "LWF__119_a__Stachel__NA" &
            code_layer == "M36" ~ 521, # Dry: 440
          plot_code_simple == "LWF__100_a__Donauhaenge__NA" &
            code_layer == "M01" ~ 610, # Dry: 485
          plot_code_simple == "LWF__57_a__Groppenhofer und Rieder Leite__NA" &
            code_layer == "M01" ~ 575, # dry: 458
          plot_code_simple == "LWF__65_a__Friedergries__NA" &
            code_layer == "M01" ~ 722, # dry: 557
          TRUE ~ bulk_den),
        dist = case_when(
          plot_code_simple == "LWF__119_a__Stachel__NA" &
            code_layer == "M13" ~ 1379, # Dry: 1252
          plot_code_simple == "LWF__100_a__Donauhaenge__NA" &
            code_layer == "M13" ~ 538, # Dry: 458
          plot_code_simple == "LWF__57_a__Groppenhofer und Rieder Leite__NA" &
            code_layer == "M01" ~ 574, # Dry: 451
          plot_code_simple == "LWF__65_a__Friedergries__NA" &
            code_layer == "M13" ~ 599, # Dry: 495
          plot_code_simple == "LWF__65_a__Friedergries__NA" &
            code_layer == "OFH" ~ 4343, # Dry (see df_local_lab): 1150
          # Note: BFNP initially doubted whether the forest floor masses they
          # recorded were gross or net, but got confirmation by the student
          # that they were net (else, 2 * 13 would have had to be subtracted)
          #
          # Corrections to correct for likely sample loss
          # as requested by the partner
          # Original: 183.9 (net)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F035" &
            code_layer == "OL" ~ 127.58,
          # Corrections to correct for likely sample loss
          # as requested by the partner
          # Original: 256.85 (net)
          plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F073" &
            code_layer == "OL" ~ 161.34,
          # Corrections from gross to net as requested by the partner
          institute_sampling == "BGD-NP" & code_layer %in% c("OL", "OFH") ~
            dist - mass_bag_field,
          TRUE ~ dist)
      ) %>%
      select(-any_of(c("mass_bag_field")))

  } # End of "if apply_corrections"





  df_layers_all <- df_layers_all %>%
    # Filter out forest floor layers that were absent (not collected)
    # e.g., WSL__36__Steibruchhau__NA
    filter(!(code_layer %in% c("OL", "OFH") &
              dist_check == "Sample not collected")) %>%
    # Add a column "layer_number":
    # Sequential index assigned to each soil layer within a plot.
    # The upper layer (often forest floor) is designated
    # as layer number 1, and each successive layer below it is assigned
    # an incrementally higher number, such as 2, 3, 4 etc.
    mutate(
      # Give each layer a fixed order
      layer_sort = match(code_layer, layers)
    ) %>%
    arrange(code, layer_sort) %>%
    group_by(code) %>%
    mutate(
      layer_number = row_number()
    ) %>%
    group_by(code) %>%
    # Add columns with the layer limits
    mutate(
      # Lower depth
      depth_bottom = case_when(
        code_layer == "OFH" ~ 0,
        # Lower depth OL: depends on whether there is any OFH below it
        code_layer == "OL" & any(code_layer == "OFH") ~
          -thickness[code_layer == "OFH"][1],
        code_layer == "OL" & !any(code_layer == "OFH") ~ 0,
        # Below-ground layers: for now consider the full depth range until
        # 100 cm (with theoretical depths), regardless of whether there
        # is bedrock present or not.
        code_layer == "M01" ~ 10,
        code_layer == "M13" ~ 30,
        code_layer == "M36" ~ 60,
        code_layer == "M61" ~ 100,
        TRUE ~ NA_real_),
      depth_top = case_when(
        code_layer == "OFH" ~ -thickness,
        # Upper depth OL: depends on whether there is any OFH below it
        code_layer == "OL" & any(code_layer == "OFH") ~
          -sum(thickness[code_layer %in% c("OL", "OFH")], na.rm = TRUE),
        code_layer == "OL" & !any(code_layer == "OFH") ~ -thickness,
        code_layer == "M01" ~ 0,
        code_layer == "M13" ~ 10,
        code_layer == "M36" ~ 30,
        code_layer == "M61" ~ 60,
        TRUE ~ NA_real_),
      depth_bottom_bedrock = ifelse(
        coalesce(depth_bedrock, 100) > depth_top,
        # If bedrock appears below or in the given layer
        case_when(
          # Below-ground layers: bottom depth until bedrock if present in the
          # layer (i.e., the shallowest among the "theoretical depth" and
          # bedrock depth. For example, M36 in a plot with bedrock appearing
          # at 54 cm will have 54 cm (not 60 cm) as depth_bottom_bedrock
          code_layer == "M01" ~
            ifelse(!is.na(depth_bedrock),
                   min(c(10, depth_bedrock)),
                   10),
          code_layer == "M13" ~
            ifelse(!is.na(depth_bedrock),
                   min(c(30, depth_bedrock)),
                   30),
          code_layer == "M36" ~
            ifelse(!is.na(depth_bedrock),
                   min(c(60, depth_bedrock)),
                   60),
          code_layer == "M61" ~
            ifelse(!is.na(depth_bedrock),
                   min(c(100, depth_bedrock)),
                   100),
          TRUE ~ depth_bottom),
        # If bedrock appears above the upper boundary of the layer
        NA_real_
      ),
      thickness = case_when(
        !is.na(thickness) ~ thickness,
        !is.na(depth_bottom) ~ (depth_bottom - depth_top)
      )
      ) %>%
    ungroup() %>%
    select(-layer_sort) %>%
    relocate(depth_top, depth_bottom, layer_number,
             thickness, thickness_min, thickness_max,
             depth_bedrock, depth_bedrock_min, depth_bedrock_max,
             depth_bedrock_compiled, depth_bedrock_cens_compiled,
             .after = code_layer)





  if (apply_corrections) {

    # "WSL__25__Seeliwald__NA":
    # Histosol with a peat layer of 45 cm thick
    # 0-cm line was supposed to be on top of this peat layer, but
    # the partner considered it below the 45-cm layer during sampling,
    # i.e., they interpreted the top 20 cm as OL (in reality HF (fibric)),
    # and the 25 cm below this so-called OL as OFH (in reality HM (mesic) and
    # HS (sapric)).

    # Solution:
    # For now, do not replace code_layer (as this may give problems with
    # joining of other datasets, e.g., lab data), but correct the depths
    # of the layers.
    # In other words, from now onwards, layer depths do no longer follow the
    # theoretical depths!! (e.g., "M01" from 45 to 55 cm instead of 0 to 10 cm)
    # Use depths rather than code_layer to identify below-ground layers from
    # now onwards...

    df_layers_all <- df_layers_all %>%
      mutate(
        depth_top = case_when(
          plot_code_simple == "WSL__25__Seeliwald__NA" ~ depth_top + 45,
          TRUE ~ depth_top
        ),
        depth_bottom = case_when(
          plot_code_simple == "WSL__25__Seeliwald__NA" ~ depth_bottom + 45,
          TRUE ~ depth_bottom
        ),
        depth_bottom_bedrock = ifelse(
          plot_code_simple == "WSL__25__Seeliwald__NA",
          case_when(
            depth_top >= depth_bedrock ~ NA_real_,
            depth_bottom > depth_bedrock ~ depth_bedrock,
            TRUE ~ depth_bottom
          ),
          depth_bottom_bedrock
        )
      )

  } # End of "if apply_corrections"





  df_layers_all <- df_layers_all %>%
    mutate(
      # Add a column "P1_depth_reached" with the depth that was probably
      # reached in P1 (in order to see whether the reported vol% coarse
      # fragments was probably directly observed or estimated by the partner)
      P1_depth_reached =
        # Depth reached in P1
        case_when(
          plot_code_simple %in% c(
            # make sure that vol% coarse fragments of
            # "WSL__39__Combe Biosse__NA" remains 0 below 30 cm
            # (in spite of high vol% shallower)
            "WSL__39__Combe Biosse__NA", # 0 vol% M36 is correct!
            "BGD-NP__1__Berchtesgaden National Park__F023",
            "WSL__21__Tariche Bois Banal__NA",
            "LWF__65_a__Friedergries__NA") ~ depth_reached,
          plot_code_simple == "VUK__36__Hadecka planinka__NA" ~ 30,
          TRUE ~ as.numeric(sub(",.*", "", depth_bedrock_cens_compiled))),
      # Add a column "P1_coaf_method" that explains how the values in
      # P1_coaf_2mm and P1_coaf_50mm were determined
      # (whether they are direct observations or they can be interpreted
      #  as "NA" because below the depth reached in P1)
      P1_coaf_method = case_when(
        grepl("^M", code_layer) &
          (P1_depth_reached > depth_top) ~ "Direct observation",
        grepl("^M", code_layer) ~ "Default below observations",
        TRUE ~ NA_character_)
      ) %>%
    relocate(P1_coaf_method, P1_depth_reached,
             .after = "P1_coaf_50mm")







  ### INCONSISTENCY 12 ----

  if (create_inconsistency_report) {

    rule <- paste0("P1 reports 100 vol% coarse fragments for this layer, ",
                   "while (disturbed) samples were collected at other points ",
                   "in the same layer, indicating that fine earth was ",
                   "present. This may reflect spatial variation. The plot-",
                   "representative coarse fragment content will be capped at ",
                   "minimum 80 vol% so that this layer contributes to the ",
                   "carbon stock.")

    rule_id <- "12"


    # Records with a problem

    recs_problem <- df_layers_all %>%
      # As VUK provided detailed spatial information on vol% coarse fragments
      filter(institute_sampling != "VUK") %>%
      filter((P1_coaf_2mm == 100) &
               (dist_check == "Sample collected"))

    inc <- recs_problem %>%
      mutate(
        inconsistency_reason = rule) %>%
      mutate(P1_coaf_2mm = as.character(P1_coaf_2mm),
             P1_coaf_50mm = as.character(P1_coaf_50mm)) %>%
      # pivot longer to get one row per parameter
      pivot_longer(
        cols = c(P1_coaf_2mm, P1_coaf_50mm, dist_check),
        names_to = "parameter",
        values_to = "value"
      ) %>%
      mutate(
        parameter = case_when(
          grepl("2mm", parameter) ~ #P1_m01coaf_2mm
            paste0("P1_", tolower(code_layer), "coaf_2mm"),
          grepl("50mm", parameter) ~ #P1_m01coaf_2mm
            paste0("P1_", tolower(code_layer), "coaf_50mm"),
          grepl("check", parameter) ~ #m01_carbon_check
            paste0(tolower(code_layer), "_carbon_check")
          )) %>%
      left_join(attribute_catalogue %>%
                  rename(parameter_description = descr,
                         parameter_unit = unit),
                by = join_by("parameter" == "column_name")) %>%
      relocate(parameter_description, parameter_unit, .after = parameter) %>%
      mutate(
        # mark unique_inconsistency = TRUE
        # only for the first parameter per record
        unique_inconsistency = (grepl("2mm", parameter)),
        source = "Survey123 app",
        team = team_harmonized,
        plot_code_app = code,
        res_id_inst = res_id_harmonized,
        sample_code = code_layer,
        rule_id = rule_id) %>%
      mutate(value = as.character(value)) %>%
      select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
             parameter, parameter_description, parameter_unit, value,
             inconsistency_reason, unique_inconsistency, rule_id)

    inconsistencies <- bind_rows(
      inconsistencies,
      inc)

  } # End of "if create_inconsistency_report"



  if (apply_corrections) {

    df_layers_all <- df_layers_all %>%
      relocate(plot_code_simple, .after = "institute_sampling") %>%
      mutate(
        P1_coaf_2mm_orig = P1_coaf_2mm,
        P1_coaf_50mm_orig = P1_coaf_50mm
      ) %>%
      group_by(plot_code_simple) %>%
      arrange(layer_number, .by_group = TRUE) %>%
      # STEP 1: Info from layer above
      mutate(
        prev_method = lag(P1_coaf_method),
        prev_coaf = lag(P1_coaf_2mm),
        prev_layer = lag(code_layer)
      ) %>%
      # STEP 2:
      # Set vol% coarse fragments to NA when not directly observed or 100
      mutate(
        P1_coaf_2mm = case_when(
          # Forest floor layers (assumption: no stones)
          grepl("^O", code_layer) ~ NA_real_,
          # Always NA if 100
          P1_coaf_2mm == 100 ~ NA_real_,
          # Default below obs → NA, except special condition
          P1_coaf_method == "Default below observations" &
            !(prev_method == "Direct observation" &
                P1_coaf_2mm >= 80 & P1_coaf_2mm < 100) ~ NA_real_,
          TRUE ~ P1_coaf_2mm
        ),
        P1_coaf_50mm = case_when(
          is.na(P1_coaf_2mm) ~ NA_real_,
          TRUE ~ P1_coaf_50mm)
      ) %>%
      # STEP 3: Apply LOCF within mineral layers
      group_by(plot_code_simple) %>%
      mutate(
        P1_coaf_2mm_filled = P1_coaf_2mm,
        P1_coaf_50mm_filled = P1_coaf_50mm
      ) %>%
      # LOCF (downward fill)
      fill(P1_coaf_2mm_filled, .direction = "down") %>%
      fill(P1_coaf_50mm_filled, .direction = "down") %>%
      # STEP 4: Gap-fill using LOCF or 80
      mutate(
        P1_coaf_2mm = case_when(
          # Only for mineral layers with NA
          grepl("^M", code_layer) & is.na(P1_coaf_2mm) &
            grepl("^M", prev_layer) &
            !is.na(P1_coaf_2mm_filled) &
            P1_coaf_2mm_filled >= 80 &
            P1_coaf_2mm_filled < 100 ~ P1_coaf_2mm_filled,
          # Otherwise → 80
          grepl("^M", code_layer) & is.na(P1_coaf_2mm) ~ 80,
          TRUE ~ P1_coaf_2mm
        ),
        P1_coaf_50mm = case_when(
          # Only for mineral layers with NA
          grepl("^M", code_layer) & is.na(P1_coaf_50mm) &
            grepl("^M", prev_layer) &
            !is.na(P1_coaf_50mm_filled) &
            P1_coaf_2mm_filled >= 80 &
            P1_coaf_2mm_filled < 100 ~ P1_coaf_50mm_filled,
          # Otherwise → 80
          grepl("^M", code_layer) & is.na(P1_coaf_50mm) ~ 80,
          TRUE ~ P1_coaf_50mm)
      ) %>%
      ungroup() %>%
      select(-prev_method, -prev_coaf, -prev_layer,
             -contains("_filled"), -contains("_orig"))


  } # End of "if apply_corrections"











  if (create_inconsistency_report) {

  ### INCONSISTENCY 7 ----

  rule <- paste0("Undisturbed mass should be plausible in comparison with ",
                 "number and volume of rings")
  rule_id <- "7"


  # These are the 5-95 % quantiles of the bulk densities of mineral matrices
  # in ICP Forests (systematic Level I) (kg m-3)
  # Note: this is dry while values from the field are moist!

  mc_wet_by_dry_min <- 1
  mc_wet_by_dry_max <- 1.6 # Estimated based on Wijnendale which was quite wet

  bd_mineral_plaus <- c(720 * mc_wet_by_dry_min, 1535 * mc_wet_by_dry_max)

  # Records with a problem

  recs_problem <- df_layers_all %>%
    mutate(
      vol_undist = vol_ring * count_rings * 1E-6, # m3
      bulk_den_est_min = # g
        vol_undist * bd_mineral_plaus[1] * 1E3,
      bulk_den_est_max = # g
        vol_undist * bd_mineral_plaus[2] * 1E3) %>%
    filter(bulk_den > 0 &
             (bulk_den < bulk_den_est_min |
                bulk_den > bulk_den_est_max)) %>%
    relocate(contains("bulk_den_est_m"), .after = bulk_den) %>%
    relocate(vol_undist, vol_ring, count_rings, .before = bulk_den)


  inc <- recs_problem %>%
    mutate(
      inconsistency_reason =
        paste0(rule,
               " [min: ", round(bulk_den_est_min),
               " g, max: ", round(bulk_den_est_max), " g]"),
      count_rings = paste0(count_rings, " (", undist_sampling_points, ")"),
      bulk_den = as.character(round(bulk_den, 1)),
      vol_ring = as.character(vol_ring)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c(bulk_den, count_rings, vol_ring),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    mutate(
      parameter = case_when(
        parameter %in% c("bulk_den") ~
          paste(tolower(code_layer), parameter, sep = "_"),
        TRUE ~ parameter)) %>%
    left_join(attribute_catalogue %>%
                rename(parameter_description = descr,
                       parameter_unit = unit),
              by = join_by("parameter" == "column_name")) %>%
    mutate(
      parameter_description = case_when(
        parameter == "count_rings" ~
          paste0("Number of undisturbed samples (e.g., Kopecky rings) taken ",
                 "for given depth"),
        TRUE ~ parameter_description),
      parameter_unit = case_when(
        parameter == "count_rings" ~ "-",
        TRUE ~ parameter_unit)) %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("bulk_den", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    mutate(value = as.character(value)) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)








  ### INCONSISTENCY 8 ----

  rule <- paste0("Subsample(s) forest floor for lab cannot weigh more than sum",
                 " of all reported forest floor masses across sampling points")
  rule_id <- "8"



  # Records with a problem

  recs_problem <- df_layers_all %>%
    filter(code_layer %in% c("OL", "OFH")) %>%
    # Records for which we ignored a sampling point are not reliable
    filter(!(grepl("NA", tamass_compiled) &
               !grepl(",NA,NA,NA,NA", tamass_compiled))) %>%
    mutate(
      mass_samples = rowSums(
        across(c(dist, edna1, edna2)),
        na.rm = TRUE),
      rel_mass_subsample = (mass_samples / tamass)) %>%
    relocate(rel_mass_subsample, mass_samples, edna1, edna2, .after = dist) %>%
    filter(!is.na(dist) & !is.na(tamass) &
             # Let's allow a little bit of difference
             # due to weighing uncertainty, i.e. > 1.X instead of > 1
             (rel_mass_subsample > 1.1))


  inc <- recs_problem %>%
    mutate(
      inconsistency_reason =
        paste0(rule,
               " [", tamass_compiled, " g]"),
      tamass = as.character(round(tamass)),
      dist = as.character(dist),
      edna1 = as.character(edna1),
      edna2 = as.character(edna2)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c(dist, tamass, edna1, edna2),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    filter(
      !(code_layer == "OL" & grepl("edna", parameter))
    ) %>%
    mutate(
      parameter = case_when(
        parameter %in% c("dist") & code_layer == "OL" ~
          paste0(code_layer, "_flamm"),
        parameter %in% c("dist") & code_layer == "OFH" ~
          paste0(code_layer, "_carbon"),
        TRUE ~ parameter)) %>%
    left_join(attribute_catalogue %>%
                rename(parameter_description = descr,
                       parameter_unit = unit),
              by = join_by("parameter" == "column_name")) %>%
    mutate(
      parameter_description = case_when(
        parameter == "tamass" ~
          "Net field-moist mass of the given layer across all sampling points",
        TRUE ~ parameter_description),
      parameter_unit = case_when(
        parameter == "tamass" ~ "g",
        TRUE ~ parameter_unit)) %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("_flamm|_carbon", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    mutate(value = as.character(value)) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)




  ### INCONSISTENCY 9 ----

  # Note: after implementing Bayesian summarising of bedrock depths,
  # no layers appear below depth_bedrock_max (a few appear below depth_bedrock)

  rule <- paste0("For depths below the deepest reported bedrock depth, ",
                 "no samples are expected to be taken")

  rule_id <- "9"


  # Records with a problem

  recs_problem <- df_layers_all %>%
    filter(dist_check == "Sample collected") %>%
    filter(depth_top > depth_bedrock_max)

  inc <- recs_problem %>%
    mutate(
      inconsistency_reason = rule,
      depth_bedrock_max = as.character(depth_bedrock_max)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c(dist_check, depth_bedrock_max),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    mutate(
      parameter = case_when(
        parameter == "dist_check" & code_layer == "OFH" ~ "OFH_carbon_check",
        parameter == "dist_check" & code_layer != "OFH" ~
          paste(tolower(code_layer), "carbon_check", sep = "_"),
        TRUE ~ parameter)) %>%
    left_join(attribute_catalogue %>%
                rename(parameter_description = descr,
                       parameter_unit = unit),
              by = join_by("parameter" == "column_name")) %>%
    mutate(
      parameter_description = case_when(
        parameter == "depth_bedrock_max" ~
          paste0("Deepest reported bedrock depth in sampling points P1-5"),
        TRUE ~ parameter_description),
      parameter_unit = case_when(
        parameter == "depth_bedrock_max" ~ "cm",
        TRUE ~ parameter_unit)) %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("check", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    mutate(value = as.character(value)) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)







  ### INCONSISTENCY 10 ----

  # Note: this check becomes redundant, as this has been manually checked
  # and corrected earlier in the script.
  # (Potential TO DO: expand for undisturbed & cross-check with sample list
  #  - note: this is also manually checked and corrected)


  rule <- paste0("Samples weighing more than 0 g are expected to show ",
                 "'sample collected'")

  rule_id <- "10"



  # Records with a problem

  recs_problem <- df_layers_all %>%
    filter(dist_check != "Sample collected") %>%
    filter(dist > 0)

  inc <- recs_problem %>%
    mutate(
      inconsistency_reason = rule,
      dist = as.character(dist)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c(dist_check, dist),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    mutate(
      parameter = case_when(
        parameter == "dist" & code_layer == "OFH" ~ "OFH_carbon",
        parameter == "dist" & code_layer != "OFH" ~
          paste(tolower(code_layer), "carbon", sep = "_"),
        parameter == "dist_check" & code_layer == "OFH" ~ "OFH_carbon_check",
        parameter == "dist_check" & code_layer != "OFH" ~
          paste(tolower(code_layer), "carbon_check", sep = "_"),
        TRUE ~ parameter)) %>%
    left_join(attribute_catalogue %>%
                rename(parameter_description = descr,
                       parameter_unit = unit),
              by = join_by("parameter" == "column_name")) %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("check", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    mutate(value = as.character(value)) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)

  } # End of "if create_inconsistency_report"







# 8. Update df_plot ----

df_plot <- df_plot %>%
    # Add bedrock depth
    left_join(df_bedrock %>%
                select(code,
                       depth_bedrock, depth_bedrock_min, depth_bedrock_max,
                       depth_bedrock_compiled, depth_bedrock_cens_compiled,
                       depth_reached),
              by = "code")






# 9. Compile sampling point-specific data ----
# (e.g., to assist with determining any need for right-censoring bedrock depths)

df_sampling_points <-
  # ---
  # 1. Comments disturbed samples
  # ---
  app_data %>%
  filter(wp == "WP2") %>%
  select(institute_sampling, plot_code_simple,
         (contains("M01") | contains("M13") | contains("M36") |
            contains("M61")) &
           starts_with("P") &
           contains("dist") &
           # These columns do not contain properties
           !ends_with("_undist_samples")
  ) %>%
  mutate(across(matches("^P\\d+_M\\d+"), as.character)) %>%
  pivot_longer(
    cols = matches("^P\\d+_M\\d+"),
    names_to = c("sampling_point", "code_layer", "variable"),
    names_pattern = "^(P\\d+)_?(M\\d+)([a-z0-9_]+)$",
    values_to = "value"
  ) %>%
  # There is a problem with some codes (probably due to Excel)
  # They are expected in the format "10,60,30", but some values show
  # "10.6" (which should probably be "10,60")
  # Correct
  mutate(
    # First correct some very long values like "40.299999999999997"
    # (should be "40,30")
    value = if_else(
      str_length(value) > 15 & str_count(value, "\\.") == 1,
      suppressWarnings(str_replace(sprintf("%.2f", as.numeric(value)),
                                   "\\.", ",")),
      value),
    value = case_when(
      # If there's a dot with only 1 digit after it, add 0 and replace dot
      # with comma
      str_detect(value, "\\.\\d$") ~ str_replace(value, "\\.(\\d)$", ",\\10"),
      # If there's a dot with 2 digits after it, just replace dot with comma
      str_detect(value, "\\.\\d{2}$") ~ str_replace(value, "\\.", ","),
      TRUE ~ value)) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  mutate(dist = strsplit(as.character(dist), ",")) %>%
  unnest(dist) %>%  # Unnest the list into individual rows
  mutate(dist = na_if(dist, "99")) %>%
  left_join(d_properties %>%
              rename(properties = property) %>%
              mutate(code = as.character(code)),
            by = join_by("dist" == "code")) %>%
  # Summarise all the properties (reported under the disturbed sampling)
  # across all sampling points
  # (more frequently reported data are first)
  group_by(plot_code_simple, code_layer, sampling_point) %>%
  reframe(
    dist = if_else(
      all(is.na(properties)),
      NA_character_,
      paste(sort(properties), collapse = ", "))
  ) %>%
  ungroup() %>%
  rename(properties = dist) %>%
  pivot_wider(
    names_from = code_layer,
    values_from = properties,
    names_prefix = "props_"
  ) %>%
  rename(p_id = sampling_point) %>%
  left_join(
    # ---
    # 2. Add maximum depth reached with undisturbed sampling
    #    (assuming that data are correctly reported)
    # ---
    df_rings %>%
      select(plot_code_simple, code_layer, undist_sampling_points) %>%
      mutate(undist_sampling_points =
               strsplit(as.character(undist_sampling_points), ",")) %>%
      unnest(undist_sampling_points) %>%
      rename(p_id = undist_sampling_points) %>%
      mutate(
        depth_bottom = case_when(
          code_layer == "M01" ~ 10,
          code_layer == "M13" ~ 30,
          code_layer == "M36" ~ 60,
          code_layer == "M61" ~ 100)
      ) %>%
      group_by(plot_code_simple, p_id) %>%
      reframe(
        depth_reached_undist = max(depth_bottom, na.rm = TRUE)) %>%
      # Complete cases for which no undisturbed samples could be collected
      complete(
        plot_code_simple,
        p_id = paste0("P", 1:5),
        fill = list(depth_reached_undist = 0)
      ),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  left_join(
    # ---
    # 3. Add remarks disturbed samples
    # ---
    app_data %>%
      select(plot_code_simple,
             ends_with("_notes_dist_samples")) %>%
      pivot_longer(
        cols = matches("P\\d{1}_"),
        names_to = "p_id",
        values_to = "notes_dist"
      ) %>%
      mutate(
        p_id = gsub("_notes_dist_samples", "", p_id)),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  left_join(
    # ---
    # 4. Add reason bulk density
    # ---
    app_data %>%
      select(plot_code_simple,
             ends_with("_reason_bulk_den")) %>%
      pivot_longer(
        cols = matches("P\\d{1}_"),
        names_to = "p_id",
        values_to = "reason_bulk_den"
      ) %>%
      mutate(
        p_id = gsub("_reason_bulk_den", "", p_id),
        reason_bulk_den = if_else(
          reason_bulk_den == "()",
          NA_character_,
          reason_bulk_den)),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  left_join(
    # ---
    # 5. Add notes bulk density
    # ---
    app_data %>%
      select(plot_code_simple,
             ends_with("_notes_bulk_den")) %>%
      pivot_longer(
        cols = matches("P\\d{1}_"),
        names_to = "p_id",
        values_to = "notes_bulk_den"
      ) %>%
      mutate(
        p_id = gsub("_notes_bulk_den", "", p_id)),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  left_join(
    # ---
    # 6. Add bedrock depth
    # ---
    df_bedrock_wide %>%
      filter(wp == "WP2") %>%
      select(
        plot_code_simple, team_harmonized, site_type,
        matches("P\\d{1}_")
      ) %>%
      pivot_longer(
        cols = matches("P\\d{1}_"),
        names_to = "p_id",
        values_to = "depth_bedrock"
      ) %>%
      mutate(
        p_id = gsub("_depth_bedrock", "", p_id)) %>%
      select(-team_harmonized),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  left_join(
    # ---
    # 7. Add "expected_pos" and "describe_pos"
    # ---
    app_data %>%
      select(plot_code_simple,
             ends_with("_expected_pos"), ends_with("_describe_pos")) %>%
      pivot_longer(
        cols = -plot_code_simple,
        names_to = c("p_id", ".value"),
        names_pattern = "(P[2-9])_(expected_pos|describe_pos)"
      ),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  left_join(
    # ---
    # 8. Add OL/OFH thickness and total mass
    # ---
    df_sampling_points_ff %>%
      rename(p_id = sampling_point) %>%
      select(plot_code_simple, p_id, code_layer, thickness, tamass) %>%
      pivot_wider(
        names_from = code_layer,
        values_from = c(thickness, tamass),
        names_glue = "{tolower(code_layer)}_{.value}"
      ),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  left_join(
    # ---
    # 9. Add remarks forest floor sample
    # ---
    app_data %>%
      select(plot_code_simple,
             ends_with("_remarks_fofloor_sample")) %>%
      pivot_longer(
        cols = -plot_code_simple,
        names_to = c("p_id", ".value"),
        names_pattern = "(P[1-9])_(.*)"
      ),
    by = join_by("plot_code_simple", "p_id")
  ) %>%
  relocate(site_type, .before = "p_id") %>%
  relocate(contains("_pos"), contains("depth"), .after = "p_id")




# 10. Return datasets ----

out <- list(
  # Layer-specific data
  df_layers = df_layers_all %>%
    rename_with(~ str_replace_all(., "No_point", "no_point")) %>%
    select(-ends_with("depth_reached"),
           -starts_with("depth_bedrock"),
           -ends_with("_sampleID")) %>%
    select(
      globalid, wp, institute_sampling, site_type, plot_code_simple,
      layer_number, code_layer, depth_top, depth_bottom, depth_bottom_bedrock,
      starts_with("thickness"),
      dist, starts_with("tamass"), surface_frame, count_ff_frames,
      starts_with("P1_coaf_"),
      bulk_den, vol_ring, count_rings, undist_sampling_points,
      properties, properties_five_pts,
      remarks_sample_dist,
      edna1, edna2, back_up,
      ends_with("_check"), everything()
    ),
  # Plot-specific data
  df_plot = df_plot %>%
    select(-starts_with("code_wrb_"), -ends_with("_survey"),
           -any_of(c("moisture", "scheme", "num_photo_humus",
                     "management_type",
                     "coordinates", "survey_year"))) %>%
    select(
      globalid, wp, ends_with("harmonized"), institute_sampling,
      site_type, composed_site_id, plot_code, plot_code_simple,
      latitude_dec, longitude_dec, hor_accuracy,
      survey_date, survey_month,
      air_temperature, actual_weather, past_weather, soil_condition, soil_dist,
      macrorelief, slope_type, slope_deg,
      starts_with("depth_"),
      humus_form,
      wrb_ref_soil_group, wrb_qualifier_1,
      gen_observtn, other_remarks, other_observtn, P1_other_remarks_fragment,
      remarks_bulk_den_sample,
      everything()
      ),
  # Sampling point-specific data
  df_sampling_points = df_sampling_points
)

if (create_inconsistency_report) {
  # Inconsistencies
  out$inconsistencies <- as_tibble(inconsistencies)
}

return(out)

}
