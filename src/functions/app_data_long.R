
app_data_long <- function(app_data_wide) {

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
           contains("M01"), contains("M13"), contains("M36"),
           contains("M61")) %>%
    select(code, starts_with("P")) %>%
    select(code, contains("dist")) %>%
    mutate(across(matches("^P\\d+_M\\d+"), as.character)) %>%
    # Not immediately sure what these two columns indicate
    select(-P1_num_M36_undist_samples, -P1_num_M61_undist_samples) %>%
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
    select(code, code_layer, sampling_point, everything()) %>%
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
      dist = if (all(is.na(properties))) {
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
    ungroup() %>%
    rename(properties = dist)

  ## Plot-specific properties ----


  df_properties_plot <- left_join(
    # 1. gen_observtn (convert code first)
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
          # str_length(value) > 15 & str_count(value, "\\.") == 1 ~
          #   str_replace(sprintf("%.2f", as.numeric(value)), "\\.", ","),
          # Otherwise leave as is
          TRUE ~ value)) %>%
      # pivot_wider(
      #   names_from = variable,
      #   values_from = value
      # ) %>%
      # select(code, code_layer, sampling_point, everything()) %>%
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
      ungroup(),
    # 2. other properties
    app_data %>%
      select(
        code,
        wp,
        other_remarks,
        other_observtn,
        ends_with("remarks_fofloor_sample"),
        ends_with("notes_dist_samples"),
        ends_with("other_remarks_fragment"),
        ends_with("notes_bulk_den"),
        ends_with("remarks_sample"),
        ends_with("remarks_bulk_den_sample")),
    by = "code")



  # TO DO: we will need to check these comments one by one and correct any
  # changes manually. For example, we need to know if less than 5 forest floor
  # sampling frames are taken.
  # Also related to rings: sometimes people can only take rings in one sampling
  # point, e.g. for M01, so M01 is only indicated in P*_bulk_density_lyr for
  # one sampling point, while multiple rings may have been taken (central pit)!






  # 2. Forest floor ----

  ## Compile mass and thickness of forest floor sampling point ----

  # First create a dataset per sampling point

  df_sampling_points <- app_data %>%
    select(
      code, team_harmonized, globalid, res_id_harmonized, wp,
      surface_frame,
      matches(paste0("(^|_)", paste0(c("OL", "OFH"), collapse = "|")))) %>%
    select(code, team_harmonized, globalid, res_id_harmonized, wp,
           surface_frame, starts_with("P")) %>%
    select(code, team_harmonized, globalid, res_id_harmonized, wp,
           surface_frame, contains("thickness"), contains("tamass")) %>%
    mutate(across(matches("^P\\d+_M\\d+"), as.character)) %>%
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
    select(code, code_layer, sampling_point, everything()) %>%
    # Filter out rows from sampling points P6-P9 if tamass is NA
    filter(
      !(sampling_point %in% c("P6", "P7", "P8", "P9")) | !is.na(tamass)) %>%
    arrange(code, desc(code_layer))


  ### INCONSISTENCY 2 ----

  rule <- paste0("Forest floor mass should be plausible in comparison with ",
                 "thickness and surface area frame")
  rule_id <- "2"

  name_export <- "inconsistencies_app_long"

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
    value = character(0),
    inconsistency_reason = character(0),
    unique_inconsistency = logical(0),
    rule_id = character(0))

  # These are the 5-95 % quantiles of the bulk densities of organic matrices
  # in ICP Forests (systematic Level I) (kg m-3)
  # Note: this is dry while values from the field are moist!

  mc_wet_by_dry_min <- 1
  mc_wet_by_dry_max <- 3 # Estimated based on Sevendonk which was quite wet

  bd_ofh_plaus <- c(55 * mc_wet_by_dry_min, 190 * mc_wet_by_dry_max)
  bd_ol_plaus <- c(15 * mc_wet_by_dry_min, 136 * mc_wet_by_dry_max)
  bd_peat_plaus <- c(51 * mc_wet_by_dry_min, 221 * mc_wet_by_dry_max)

  # Records with a problem

  recs_problem <- df_sampling_points %>%
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
    # Since there is quite some uncertainty in measurement of layer thicknesses
    # we can filter for a relative deviation of at least X
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
        parameter %in% c("thickness", "tamass") ~ as.character(round(value, 1)),
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
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("tamass", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)


  # Note: as a potential additional inconsistency check, I was wondering
  # if a forest floor tamass of "0" means actually that there is no forest
  # floor was present, of just if they didn't sample it. But I have the
  # impression that we can trust that 0 means no forest floor (so we need
  # to include these points in the calculation of the areal mass).
  # Likewise, NA in sampling points 1-5 is suspicious, but in 2025, it was
  # not possible to submit the survey without filling this in. There are a
  # few NAs in the VUK 2024 sites from WP3. These also seem to indicate
  # "no (considerable) forest floor" considering the layer thickness. Which
  # makes sense in WP3 sites.



  ## Summarise forest floor across sampling points ----

  df_sampling_points_summ <- df_sampling_points %>%
    group_by(code, code_layer, surface_frame) %>%
    reframe(
      count_ff_frames = n(),
      thickness_min = ifelse(
        any(!is.na(thickness)),
        min(as.numeric(thickness), na.rm = TRUE),
        NA_real_),
      thickness_max = ifelse(
        any(!is.na(thickness)),
        max(as.numeric(thickness), na.rm = TRUE),
        NA_real_),
      thickness = ifelse(
        any(!is.na(thickness)),
        round(mean(as.numeric(thickness), na.rm = TRUE), 1),
        NA_real_),
      tamass_compiled = paste(as.character(round(tamass)), collapse = ","),
      tamass_min = ifelse(
        any(!is.na(tamass)),
        min(as.numeric(tamass), na.rm = TRUE),
        NA_real_),
      tamass_max = ifelse(
        any(!is.na(tamass)),
        max(as.numeric(tamass), na.rm = TRUE),
        NA_real_),
      tamass_mean = ifelse(
        any(!is.na(tamass)),
        round(mean(as.numeric(tamass), na.rm = TRUE), 1),
        NA_real_),
      tamass = ifelse(
        any(!is.na(tamass)),
        sum(as.numeric(tamass), na.rm = TRUE),
        NA_real_)) %>%
    ungroup()

  # TO DO: in case there are partners which an exceptional number of
  # forest floor sampling frame locations covered per site
  # (e.g., WSL Seeliwald), based on the information from df_properties_plot,
  # I would incorporate this here manually by updating
  # df_sampling_points_summ$count_ff_frames





  # 3. Potential to take undisturbed samples ----

  d_kopecky_depths <- tibble(
    code = c(10, 11, 12, 13),
    code_layer = c("M01", "M13", "M36", "M61"))

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
             site_type,
             # Volume of kopecky ring
             # (typically one ring per depth x sampling point)
             vol_ring,
             P1_num_M36_undist_samples, P1_num_M61_undist_samples,
             starts_with("P")) %>%
      select(code, team_harmonized, globalid, res_id_harmonized,
             site_type, vol_ring,
             P1_num_M36_undist_samples, P1_num_M61_undist_samples,
             ends_with("_bulk_density")) %>%
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
               site_type, code_layer, vol_ring) %>%
      reframe(
        undist_sampling_points = paste(sort(unique(sampling_point)),
                                       collapse = ","),
        count_rings = n()),
    # 2025 records
    app_data %>%
      select(code, team_harmonized, globalid, res_id_harmonized,
             site_type,
             # Volume of kopecky ring
             # (typically one ring per depth x sampling point)
             vol_ring,
             starts_with("P")) %>%
      select(code, team_harmonized, globalid, res_id_harmonized,
             site_type, vol_ring,
             P1_num_M36_undist_samples, P1_num_M61_undist_samples,
             ends_with("_bulk_density_lyr")) %>%
      pivot_longer(cols = matches("^P[1-5]_bulk_density_lyr$"),
                   names_to = "sampling_point",
                   values_to = "code_depths") %>%
      mutate(
        # Keep only "P1", "P2", etc.
        sampling_point = str_extract(sampling_point, "^P\\d+")) %>%
      separate_rows(code_depths, sep = ",") %>%
      rename(code_layer = code_depths) %>%
      arrange(code, code_layer) %>%
      # filter(!is.na(code_layer)) %>%
      group_by(code, team_harmonized, globalid, res_id_harmonized,
               site_type, code_layer, vol_ring,
               P1_num_M36_undist_samples, P1_num_M61_undist_samples) %>%
      reframe(
        undist_sampling_points = paste(sort(unique(sampling_point)),
                                       collapse = ","),
        count_rings = n())
    ) %>%
    # For plots in which all M36 and M61 rings were taken from the deep
    # central pit, correct the number of rings based on the columns
    # P1_num_M36_undist_samples, P1_num_M61_undist_samples
    mutate(
      count_rings_orig = count_rings,
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
        TRUE ~ count_rings_orig))









  ### INCONSISTENCY X ----

  rule <- paste0("The total number of reported undisturbed samples from the ",
                 "central pit (P1) and additional sampling points should not ",
                 "exceed 5 (unlikely). Please check how many undisturbed ",
                 "samples you took at the given depth.")
  rule_id <- "X"


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
        TRUE ~ parameter_description)) %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("P1_num_undist_samples", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)



  # Add inconsistencies:
  # P1_num + count_rings_orig > 6
  # P1_num == 1 & count_rings == 1


  ### INCONSISTENCY X ----

  rule <- paste0("The total number of reported undisturbed samples from the ",
                 "central pit (P1) and additional sampling points should ",
                 "not be 1 (unlikely). Please check how many undisturbed ",
                 "samples you took at the given depth.")

  rule_id <- "X"


  # Records with a problem

  recs_problem <- df_rings %>%
    mutate(
      P1_num_undist_samples = case_when(
        code_layer == "M36" ~ P1_num_M36_undist_samples,
        code_layer == "M61" ~ P1_num_M61_undist_samples)) %>%
    filter(
      !is.na(P1_num_undist_samples) &
      (P1_num_undist_samples + count_rings_orig - 1) == 1)

  # Note: some "standard" plots also have one ring per depth layer (e.g.,
  # Laerchenberg), but this is more plausible, because no deep pit had to
  # be dug. With the deep pit, it would make sense to take more than one
  # undisturbed sample.

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
        TRUE ~ parameter_description)) %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("P1_num_undist_samples", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)










  # 4. Depth bedrock ----


  df_bedrock <- app_data %>%
    select(code, composed_site_id, team_harmonized, globalid,
           res_id_harmonized, wp, site_type,
           matches("^P[1-9]_depth_bedrock$")) %>%
    arrange(team_harmonized) %>%
    rowwise() %>%
    # The potential problem with the bedrock depths:
    # It is not a mandatory parameter, because of which it may be not be filled
    # in (for some sampling points).
    # → Question: does NA mean "no bedrock in upper 100 cm" or
    # "bedrock in upper 100 cm but depth not filled in by partner"?
    # There are some suspicious cases, e.g. Zweribach (NW-FVA),
    # Nollenwald (NW-FVA)
    # However, the bedrock appearance within a site can be very variable, e.g.,
    # Hadecka planinka (VUK): 34, NA, 37, 37, NA. This is known to be correct.
    # Another question: since partners typically didn't auger deeper than 30 cm
    # in points 6 - 9: it is maybe more correct to ignore those columns?
    # (because these columns cannot help us if the bedrock appears between 30
    #  and 100 cm)
    mutate(
      # How many bedrock depths are filled in?
      n_filled = sum(!is.na(c_across(matches("^P[1-5]_depth_bedrock$")))),
      # Are all non-missing bedrock depths ≤ 30?
      all_shallow = all(c_across(matches("^P[1-5]_depth_bedrock$"))[!is.na(
        c_across(matches("^P[1-5]_depth_bedrock$")))] <= 30),
      # How many bedrock depths are missing?
      n_missing = sum(is.na(c_across(matches("^P[1-5]_depth_bedrock$")))),
      # Suspicious if at least 2 bedrock depths are filled in and at least 1
      # is NA, with all of the filled bedrock depths being shallower than 30 cm?
      #
      flag_suspicious = n_filled >= 2 & all_shallow & n_missing > 0,
      # Most shallow bedrock depth reported
      depth_bedrock_min =
        round(min(replace_na(c_across(matches("^P[1-5]_depth_bedrock$")),
                             100))),
      # Deepest bedrock depth reported
      depth_bedrock_max = case_when(
        # If flagged as suspicious: ignore NAs for the average of P1-5
        flag_suspicious ~
          round(suppressWarnings(max(c_across(
            matches("^P[1-5]_depth_bedrock$")),
                    na.rm = TRUE))),
        # If site_type is stony, and P2-5 are NA: only use P1
        site_type == "Stony site" &
          all(is.na(c_across(matches("^P[2-5]_depth_bedrock$")))) ~
          round(replace_na(P1_depth_bedrock, 100)),
        # Else, take the average P1-5 (assuming NA equals 100)
        TRUE ~
          round(max(replace_na(c_across(matches("^P[1-5]_depth_bedrock$")),
                               100)))),
      # Average depth bedrock (until which we will calculate the stocks)
      depth_bedrock = case_when(
        # If flagged as suspicious: ignore NAs for the average of P1-5
        flag_suspicious ~
          round(mean(c_across(matches("^P[1-5]_depth_bedrock$")),
                     na.rm = TRUE)),
        # If site_type is stony, and P2-5 are NA: only use P1
        site_type == "Stony site" &
          all(is.na(c_across(matches("^P[2-5]_depth_bedrock$")))) ~
          round(replace_na(P1_depth_bedrock, 100)),
        # Else, take the average P1-5 (assuming NA equals 100)
        TRUE ~
          round(mean(replace_na(c_across(matches("^P[1-5]_depth_bedrock$")),
                                100))))) %>%
    ungroup()

  # TO DO: Maybe it would be good to compare this with the pictures from the
  # field (especially for stony sites)


  ### INCONSISTENCY 3 ----

  rule <- paste0("Bedrock depth is missing (NA) in some sampling points ",
                 "(P1-5), while others report shallow bedrock. Please ",
                 "confirm that all missing values are correctly marked as ",
                 "NA (i.e., bedrock below 100 cm)")
  rule_id <- "3"



  # Records with a problem

  recs_problem <- df_bedrock %>%
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
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("^P1", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = NA_character_,
      rule_id = rule_id) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)








  # 5. Reshape sample checklist into one record per code_layer per plot ----

  df_layers <- app_data %>%
    select(-ends_with("dist"), -contains("thickness"), -contains("tamass"),
           -vol_ring) %>%
    rename_with(~ str_replace_all(., "eDNA", "edna")) %>%
    rename_with(~ str_replace_all(., "(flamm|carbon)", "dist")) %>%
    select(code, team_harmonized, globalid, res_id_harmonized,
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
    select(code, team_harmonized, globalid, res_id_harmonized,
           code_layer, variable, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value) %>%
    rename(remarks_sample_dist = remarks_sample) %>%
    # Automatically detect and convert columns that contain only
    # numeric-convertible values to numeric class
    mutate(across(
      where(~ all(suppressWarnings(!is.na(as.numeric(.)) | is.na(.)))),
      as.numeric))





  ### INCONSISTENCY X ----

  rule <- paste0("Volumetric percentage of coarse fragments > 50 mm should ",
                 "not exceed that > 2 mm. You may have only indicated coarse ",
                 "fragments between 2–50 mm under > 2 mm.")

  rule_id <- "X"


  # Records with a problem

  recs_problem <- df_layers %>%
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
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("2mm", parameter)),
      source = "Survey123 app",
      team = team_harmonized,
      plot_code_app = code,
      res_id_inst = res_id_harmonized,
      sample_code = code_layer,
      rule_id = rule_id) %>%
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)






  # 6. Compile all layer-specific data ----

  df_layers_all <- df_layers %>%
    left_join(df_properties,
              by = join_by(code, code_layer)) %>%
    left_join(df_sampling_points_summ,
              by = join_by(code, code_layer)) %>%
    left_join(df_rings %>%
                select(-team_harmonized, -globalid, -res_id_harmonized),
              by = join_by(code, code_layer)) %>%
    # Add average bedrock depth in order to update the lowest layer limit
    # of the lowest layer of the profile
    left_join(df_bedrock %>%
                select(code,
                       depth_bedrock, depth_bedrock_min, depth_bedrock_max),
              by = "code") %>%
    relocate(tamass, .before = dist) %>%
    relocate(
      contains("_sampleID"), .after = last_col()) %>%
    # Add a column "layer_number":
    # Sequential index assigned to each soil layer within a plot.
    # The upper layer (often forest floor) is designated
    # as layer number 1, and each successive layer below it is assigned
    # an incrementally higher number, such as 2, 3, 4 etc.
    mutate(
      # give each layer a fixed order
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
      # Upper depth
      depth_bottom = case_when(
        code_layer == "OFH" ~ 0,
        # Lower depth OL: depends on whether there is any OFH below it
        code_layer == "OL" & any(code_layer == "OFH") ~
          -thickness[code_layer == "OFH"],
        code_layer == "OL" & !any(code_layer == "OFH") ~ 0,
        # Below-ground layers: bottom depth is always the shallowest
        # among the "theoretical depth" and bedrock depth. For example, M36 in
        # a plot with bedrock appearing at 54 cm will have 54 cm
        # (not 60 cm) as depth_bottom
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
        TRUE ~ NA_real_)) %>%
    ungroup() %>%
    select(-layer_sort) %>%
    relocate(depth_top, depth_bottom, layer_number,
             thickness, thickness_min, thickness_max,
             depth_bedrock, depth_bedrock_min, depth_bedrock_max,
             .after = code_layer)




  ### INCONSISTENCY 4 ----

  rule <- paste0("Undisturbed mass should be plausible in comparison with ",
                 "number and volume of rings")
  rule_id <- "4"


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
    # It may be that bulk density samples are heavier than expected if more
    # rings have been taken at an individual sampling point. But for now,
    # we can just include all problematic cases.

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
        TRUE ~ parameter_description)) %>%
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
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)








  ### INCONSISTENCY 5 ----

  rule <- paste0("Subsample forest floor for lab cannot weigh more than sum of",
                 "all reported forest floor masses across sampling points")
  rule_id <- "5"



  # Records with a problem

  recs_problem <- df_layers_all %>%
    filter(code_layer %in% c("OL", "OFH")) %>%
    mutate(rel_mass_subsample = (dist / tamass)) %>%
    relocate(rel_mass_subsample, .after = dist) %>%
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
      dist = as.character(dist)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c(dist, tamass),
      names_to = "parameter",
      values_to = "value"
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
        TRUE ~ parameter_description)) %>%
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
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)




  ### INCONSISTENCY X ----

  rule <- paste0("For depths below the deepest reported bedrock depth, ",
                 "no samples are expected to be taken")

  rule_id <- "X"


  # Records with a problem

  recs_problem <- df_layers_all %>%
    filter(dist_check == "Sample collected") %>%
    filter(depth_top > depth_bedrock_max)

  inc <- recs_problem %>%
    mutate(
      inconsistency_reason = rule) %>%
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
          paste0("Number of undisturbed samples (e.g., Kopecky rings) taken for ",
                 "given depth"),
        TRUE ~ parameter_description)) %>%
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
    select(source, team, plot_code_app, res_id_inst, globalid, sample_code,
           parameter, parameter_description, parameter_unit, value,
           inconsistency_reason, unique_inconsistency, rule_id)

  inconsistencies <- bind_rows(
    inconsistencies,
    inc)







# 7. Compile all plot-specific data ----



  assign(name_export, inconsistencies, envir = .GlobalEnv)
  cat(paste0("Object '", name_export, "' is imported in global environment.\n"))


# add depths

# return plot-specific data as well


c("code", "survey_date",
  "humus_form", "wrb_ref_soil_group", "wrb_qualifier_1", "wrb_qualifier_supp",
  "latitude", "longitude", "coordinates", "hor_accuracy",
  "air_temperature", "actual_weather", "past_weather",
  "soil_condition", "soil_dist", "macrorelief", "slope_type", "slope_deg",
  "moisture", "site_type", "scheme", "num_photo_humus", "management_type",
  "start_survey", "end_survey", "survey_month", "survey_year",
  "P2_expected_pos", "P2_describe_pos",
  "P3_expected_pos", "P3_describe_pos",
  "P4_expected_pos", "P4_describe_pos",
  "P5_expected_pos", "P5_describe_pos",
  "P6_expected_pos", "P6_describe_pos",
  "P7_expected_pos", "P7_describe_pos",
  "P8_expected_pos", "P8_describe_pos",
  "P9_expected_pos", "P9_describe_pos")




}
