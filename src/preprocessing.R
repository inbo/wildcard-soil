
# Preprocessing of all raw WILDCARD data into a harmonised version
# ----------------------------------------------------------------

# This script was made to compile all data from the INBO test soil sampling
# events in Zoniënwoud and Bos Terrijst in 2024. The final scripts for the
# 'real' data could follow a similar workflow.

# It's not ready yet, and some things need to be optimised.



# TEST SAMPLING

# Source raw data:
# · Data from Survey123 app
# · Data from local lab (spreadsheet named “forest_floor_data_[institute]”)
# · Data from analytical central lab (via inbolims)
# · Data from sample pretreatment central lab

# Aim:
# Spreadsheet with site-level data per depth layer


# Show "document outline" window of this script in R Studio
# using Ctrl + Shift + O




# 1. Prepare packages ----

# Define required packages

stopifnot(require("tidyverse"),
          require("openxlsx"),
          require("parsedate"),
          require("googlesheets4"),
          require("googledrive"),
          require("assertthat"),
          require("inbolims"),
          require("ggpmisc"),
          require("patchwork"))

# Source URLs of sensitive Google Drive links
source("./data/sensitive_metadata/google_drive_links.R")


# 2. Import data ----

## · Data from Survey123 app ----

data_app <- read.csv("./data/raw_data/app_data/INBO_WP3_DATA.csv")
glimpse(data_app)

sumava <- read_excel("./src/sandbox/Field_Form_WP3_Sumava_wet_chrono.xlsx")
glimpse(sumava)


# Theoretical samples

sample_chem_theor <- c(
  "OFH_carbon",
  "M01_carbon",
  "M13_carbon",
  "M36_carbon",
  "M61_carbon")

sample_bd_theor <- c(
  "M01_Bulk_density",
  "M13_Bulk_density",
  "M36_Bulk_density",
  "M61_Bulk_density")

sample_edna_theor <- c(
  "OFH_eDNA1",
  "OFH_eDNA2",
  "M01_eDNA1",
  "M01_eDNA2")

sample_flamm_theor <- c(
  "OL_flammability")



## · Data from local lab ----
##   (spreadsheet named “forest_floor_data_[institute]”)




## · Data from sample pretreatment central lab ----

dist <- read_sheet(as_id(id_dist),
                   skip = 1)

undist <- read_sheet(as_id(id_undist),
                     skip = 1)




## · Data from analytical central lab (via inbolims) ----

# Switch on VPN of INBO
cat("Switch on VPN of INBO.\n")

connection <- lims_connect()
samples_lab <- lims_sample_information(connection, project = c("V-24V006-01"))


data_lab <- read_lims_data(connection = connection,
                               project = c("V-24V006-01"),
                               sql_template = "default",
                               show_query = FALSE)

below_loq <- data_lab %>%
  filter(grepl("<", WaardeGeformatteerd))

assertthat::assert_that(nrow(below_loq) == 0,
                        msg = paste0("Some lab values are below LOQ. ",
                                     "Implement below-LOQ harmonisation in ",
                                     "R script."))

# TO DO: harmonise below-LOQ data! (replace those by 50 % of the LOQ)
# See last part of R script:
# https://github.com/inbo/fscc/blob/main/src/functions/harmonise_below_loqs.R
# For the SOC stocks, you can propagate the uncertainty in TC values below LOQ:
# minimum: 0
# maximum: LOQ

# Also add the uncertainty in lab analyses of TOC etc (using ring test data?)

datax_lab <- lims_report_xtab(data_lab)

datax_lab <- datax_lab %>%
  mutate(across(
    where(~ all(suppressWarnings(!is.na(as.numeric(.)) | is.na(.)))),
    as.numeric))




# Plot survey-level data ----


df_plot_survey <- data_app %>%
  rename(
    country_code = country,
    institute = team,
    res_id_inst = region,
    sub_id = site_id,
    wrb_ref_soil_group = ref_soil_grp,
    wrb_qualifier_1 = any_of(c("principal_q_RG", "principal_q")),
    date_time = any_of(c("date_time (UTC)", "date_time")),
    x = any_of(c("x", "X_WGS")),
    y = any_of(c("y", "Y_WGS"))) %>%
  mutate(
    # Create column only if missing
    air_temperature =
      if (!"air_temperature" %in% names(.)) NA_real_ else air_temperature) %>%
  mutate(
    plot_code = paste0(institute, "__",
                       res_id_inst, "__",
                       sub_id, "__",
                       plot_id)) %>%
  mutate(
    latitude = coalesce(x,
                        latitude),
    longitude = coalesce(y,
                         longitude),
    latitude = as.numeric(str_replace(latitude, ",", ".")),
    longitude = as.numeric(str_replace(longitude, ",", ".")),
    hor_accuracy = coalesce(hor_accuracy_aut,
                            hor_accuracy_man)) %>%
  mutate(survey_date = as.Date(parse_date(date_time)),
         survey_year = as.integer(format(survey_date, "%Y"))) %>%
  select(country_code,
         institute,
         res_id_inst,
         sub_id,
         plot_id,
         plot_code,
         latitude, longitude, hor_accuracy,
         survey_date, survey_year,
         actual_weather, air_temperature, past_weather,
         soil_condition, soil_dist, moisture,
         slope_type, slope_deg,
         other_remarks,
         bulk_den_fraction_prof,
         humus_form,
         wrb_ref_soil_group, wrb_qualifier_1)


glimpse(df_plot_survey)



# Layer data app ----

## Summarise properties from disturbed samples across the sampling points ----

layers <- c("OL", "OFH", "M01", "M13", "M36", "M61")

d_properties <- read.csv(paste0("./data/additional_data/",
                                "dictionary_disturbed_properties.csv"),
                         sep = ";")

df_properties <- data_app %>%
  # Make sure that all layer codes ("M01", "m01" etc) are capitalised
  # (in the column names)
  # (only column names starting with this code_layer or
  # containing "_[code_layer]", i.e., "vol_ring" shouldn't be converted)
  rename_with(
    ~ str_replace_all(.,
                      paste0("(?<=^|_)", paste0(tolower(layers),
                                                collapse = "|")),
                      toupper),
    matches(paste0("(^|_)", paste0(layers, collapse = "|")))
  ) %>%
  select(code,
         contains("M01"), contains("M13"), contains("M36"), contains("M61")) %>%
  select(code, starts_with("P")) %>%
  select(code, contains("dist")) %>%
  mutate(across(matches("^P\\d+_M\\d+"), as.character)) %>%
  pivot_longer(
    cols = matches("^P\\d+_M\\d+"),
    names_to = c("sampling_point", "code_layer", "variable"),
    names_pattern = "^(P\\d+)_?(M\\d+)([a-z0-9_]+)$",
    values_to = "value"
  ) %>%
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

glimpse(df_properties)

# TO DO: Add other reported remarks in this dataframe
# (maybe most useful)

# other_remarks
# other_observtn
# gen_observtn
# remarks_fofloor_sample
# P1_notes_dist_samples
# P1_remarks_fofloor_sample
# P1_other_remarks_fragment
# etc




## Summarise mass and thickness of forest floor ----


df_sampling_points <- data_app %>%
  # Make sure that all layer codes ("M01", "m01" etc) are capitalised
  # (in the column names)
  # (only column names starting with this code_layer or
  # containing "_[code_layer]", i.e., "vol_ring" shouldn't be converted)
  rename_with(
    ~ str_replace_all(.,
                      paste0("(?<=^|_)", paste0(tolower(layers),
                                                collapse = "|")),
                      toupper),
    matches(paste0("(^|_)", paste0(layers, collapse = "|")))
  ) %>%
  mutate(
    # Create column only if missing
    # Surface area per sampling point
    # (to be multiplied with number of sampling points)
    surface_frame =
      if (!"surface_frame" %in% names(.)) .0625 else surface_frame) %>%
  select(
    code, surface_frame,
    matches(paste0("(^|_)", paste0(c("OL", "OFH"), collapse = "|")))) %>%
  select(code, surface_frame, starts_with("P")) %>%
  select(code, surface_frame, contains("thickness"), contains("tamass")) %>%
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
    !(sampling_point %in% c("P6", "P7", "P8", "P9")) | !is.na(tamass))

glimpse(df_sampling_points)

# PIR: relationship between thickness and mass should be within plausible range
# PIR: double-check rows for which tamass is NA (simply no forest floor?)

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


glimpse(df_sampling_points_summ)


## Summarise potential to take undisturbed samples ----

d_kopecky_depths <- tibble(
  code = c(10, 11, 12, 13),
  code_layer = c("M01", "M13", "M36", "M61"))

df_rings <- data_app %>%
  select(code,
         # Volume of kopecky ring
         # (typically one ring per depth x sampling point)
         vol_ring,
         starts_with("P")) %>%
  select(code, vol_ring, ends_with("_bulk_density")) %>%
  pivot_longer(cols = starts_with("P"),
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
  group_by(code, code_layer, vol_ring) %>%
  reframe(
    undist_sampling_points = paste(sort(unique(sampling_point)),
                                   collapse = ","),
    count_rings = n())



## Reshape data sample checklist into one row per code_layer per plot ----


df_layers <- data_app %>%
  select(-ends_with("dist"), -contains("thickness"), -contains("tamass"),
         -vol_ring) %>%
  rename_with(~ str_replace_all(., paste0("(", paste(tolower(layers),
                                                     collapse = "|"), ")"),
                                toupper),
              matches(paste(layers, collapse = "|"))) %>%
  rename_with(~ str_replace_all(., "eDNA", "edna")) %>%
  rename_with(~ str_replace_all(., "(flamm|carbon)", "dist")) %>%
  select(code,
         contains("OL"), contains("OFH"),
         contains("M01"), contains("M13"), contains("M36"), contains("M61")) %>%
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
                             paste0(layers, collapse = "|"), ")"), "_\\1_") %>%
      str_replace_all("__", "_") %>%
      str_replace_all("^_|_$", ""),
    code_layer = str_extract(full_name, paste0(layers, collapse = "|")),
    variable = str_remove(full_name,
                          paste0("_?", paste0(layers, collapse = "|"), "_?")),
    variable = str_replace_all(variable, "__", "_"),
    variable = str_replace_all(variable, "^_|_$", "")
  ) %>%
  select(code, code_layer, variable, value) %>%
  pivot_wider(
    names_from = variable,
    values_from = value)


## Add summarised data per sampling point ----

df_layers <- df_layers %>%
  left_join(df_properties,
            by = join_by(code, code_layer)) %>%
  left_join(df_sampling_points_summ,
            by = join_by(code, code_layer)) %>%
  left_join(df_rings,
            by = join_by(code, code_layer)) %>%
  relocate(tamass, .before = dist)



glimpse(df_layers)


# Data pretreatment disturbed ----

glimpse(dist)

dist_harm <- dist %>%
  rename(sample_id = `sample_id (unieke veldcode uit Survey123 app)`,
         survey_date = `Datum bemonstering`,
         mass_recipient = `Massa (g) recipiënt zonder deksel`,
         mass_before =
           `Massa (g) monster voor (inclusief recipiënt zonder deksel)`,
         mass_after =
           `Massa monster (g) na (inclusief recipiënt zonder deksel)`) %>%
  select(sample_id, survey_date, mass_recipient, mass_before, mass_after) %>%
  mutate(
    survey_date = as.Date(parse_date(survey_date)),
    survey_year = as.integer(format(survey_date, "%Y")),
    plot_code = case_when(
      grepl("^WILDCARD2024_11", sample_id) ~ "INBO_11_NA_111000",
      grepl("^WILDCARD2024_INBO_1", sample_id) ~ "INBO_1_OldCore_2051",
      TRUE ~ str_extract(sample_id, "(?<=-)[^-]+(?:-[^-]+)*-[^-]+(?=-)")),
    country_code = case_when(
      grepl("^WILDCARD2024", sample_id) ~ "BE",
      TRUE ~ str_extract(sample_id, "^[^-]+")),
    sample_code = case_when(
      grepl("^WILDCARD2024", sample_id) ~ str_extract(sample_id, "[^\\_]+$"),
      TRUE ~ str_extract(sample_id, "[^\\-]+$")),
    code_layer = str_extract(sample_code, "^[^_-]+")) %>%
  mutate(
    plot_code_app = paste0(
      country_code, "-",
      gsub("_", "-", plot_code))) %>%
  mutate(
    # clean and convert to numeric
    mass_recipient = sapply(mass_recipient, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }),
    mass_before = sapply(mass_before, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }),
    mass_after = sapply(mass_after, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }))

assertthat::assert_that(
  all(is.na(dist_harm$mass_before) | !is.na(dist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_before is not NA.")

assertthat::assert_that(
  all(is.na(dist_harm$mass_after) | !is.na(dist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_after is not NA.")

dist_harm <- dist_harm %>%
  # Calculate net mass
  mutate(
    mass_before_net = mass_before - mass_recipient,
    mass_after_net = mass_after - mass_recipient)


glimpse(dist_harm)

# Filter for test data 2024

dist2024 <- dist_harm %>%
  filter(grepl("^WILDCARD2024", sample_id))

# Summarise per layer

dist2024 <- dist2024 %>%
  # Step 1: Create OL subtotals
  filter(code_layer == "OL") %>%
  select(plot_code_app, plot_code, sample_code, mass_after_net) %>%
  pivot_wider(
    names_from = sample_code,
    values_from = mass_after_net,
    names_prefix = "mass_after_"
  ) %>%
  mutate(
    code_layer = "OL",
    mass_after = rowSums(across(starts_with("mass_after_")), na.rm = TRUE)
  ) %>%
  rename(
    mass_after_leaves = `mass_after_OL-leaves`,
    mass_after_twigs_medium = `mass_after_OL-twigs-medium`,
    mass_after_twigs_small = `mass_after_OL-twigs-small`
  ) %>%
  left_join(
    dist2024 %>%
      filter(code_layer == "OL") %>%
      group_by(plot_code_app) %>%
      reframe(
        mass_before = ifelse(
          any(!is.na(mass_before_net)),
          sum(mass_before_net, na.rm = TRUE),
          NA_real_)),
    by = "plot_code_app") %>%
  select(plot_code_app, plot_code, code_layer, mass_before, mass_after,
         mass_after_leaves, mass_after_twigs_medium, mass_after_twigs_small) %>%
  # Step 2: Bind with non-OL rows
  bind_rows(
    dist2024 %>%
      filter(code_layer != "OL") %>%
      mutate(mass_after = mass_after_net,
             mass_before = mass_before_net) %>%
      select(plot_code_app, plot_code,
             code_layer, mass_before, mass_after) %>%
      mutate(
        mass_after_leaves = NA_real_,
        mass_after_twigs_medium = NA_real_,
        mass_after_twigs_small = NA_real_
      )) %>%
  # Step 3: Arrange by plot and layer
  arrange(plot_code_app, code_layer)






# Data pretreatment undisturbed ----

glimpse(undist)

undist_harm <- undist %>%
  rename(
    sample_id = `sample_id (unieke veldcode uit Survey123 app)`,
    survey_date = `Datum bemonstering`,
    mass_recipient = `Massa (g) recipiënt zonder deksel`,
    mass_before =
      `Massa (g) monster voor (inclusief recipiënt zonder deksel en steentjes)`,
    mass_after =
      `Massa fine earth (g) na (inclusief recipiënt zonder deksel)`,
    mass_cf = `Massa steentjes (> 2 mm) (g) droog zonder recipiënt`,
    mass_roots =
      `Massa wortels (droog; indien aanzienlijk) (g) zonder recipiënt`) %>%
  select(sample_id, survey_date, mass_recipient, mass_before, mass_after,
         mass_cf, mass_roots) %>%
  mutate(
    survey_date = as.Date(parse_date(survey_date)),
    survey_year = as.integer(format(survey_date, "%Y")),
    plot_code = case_when(
      grepl("^WILDCARD2024_11", sample_id) ~ "INBO_11_NA_111000",
      grepl("^WILDCARD2024_INBO_1", sample_id) ~ "INBO_1_OldCore_2051",
      TRUE ~ str_extract(sample_id, "(?<=-)[^-]+(?:-[^-]+)*-[^-]+(?=-)")),
    country_code = case_when(
      grepl("^WILDCARD2024", sample_id) ~ "BE",
      TRUE ~ str_extract(sample_id, "^[^-]+")),
    sample_code = case_when(
      grepl("^WILDCARD2024", sample_id) ~ str_extract(sample_id, "[^\\_]+$"),
      TRUE ~ str_extract(sample_id, "[^\\-]+$")),
    code_layer = str_extract(sample_code, "^[^_-]+")) %>%
  mutate(
    plot_code_app = paste0(
      country_code, "-",
      gsub("_", "-", plot_code))) %>%
  mutate(
    # clean and convert to numeric
    mass_recipient = sapply(mass_recipient, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }),
    mass_before = sapply(mass_before, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }),
    mass_after = sapply(mass_after, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }),
    mass_cf = sapply(mass_cf, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }),
    mass_roots = sapply(mass_roots, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }))


assertthat::assert_that(
  all(is.na(undist_harm$mass_before) | !is.na(undist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_before is not NA.")

assertthat::assert_that(
  all(is.na(undist_harm$mass_after) | !is.na(undist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_after is not NA.")

undist_harm <- undist_harm %>%
  # Calculate net mass
  # mass_cf and mass_roots are net already
  mutate(
    mass_before_net = mass_before - mass_recipient,
    mass_after_net = mass_after - mass_recipient)

glimpse(undist_harm)



# Filter for test data 2024

undist2024 <- undist_harm %>%
  filter(grepl("^WILDCARD2024", sample_id))


## Calculate areal masses ----
#  Bulk density (of fine earth), vol% coarse fragments, areal mass forest floor

df_2024 <- undist2024 %>%
  # left_join(df_rings,
  #           by = join_by("plot_code_app" == "code",
  #                        code_layer)) %>%
  # No Survey123 app data were reported for "BE-INBO-1-OldCore-2051",
  # but the Kopecky rings were also taken for all sampling points and depths
  # Better use the code above for the "real" samples
  left_join(df_rings,
            by = join_by(code_layer)) %>%
  # left_join(df_layers %>%
  #             select(code, code_layer,
  #                    P1_coaf_2mm, P1_coaf_50mm),
  #           by = join_by("plot_code_app" == "code",
  #                        code_layer)) %>%
  # No Survey123 app data were reported for "BE-INBO-1-OldCore-2051",
  # But there were likewise no stones were observed in the field
  # Better use the code above for the "real" samples
  left_join(df_layers %>%
              mutate(coaf_2mm = as.numeric(P1_coaf_2mm),
                     coaf_50mm = as.numeric(P1_coaf_50mm)) %>%
              select(code_layer,
                     coaf_2mm, coaf_50mm),
            by = join_by(code_layer)) %>%
  left_join(df_layers %>%
              mutate(bulk_density_field_moist = as.numeric(bulk_den)) %>%
              select(code, code_layer,
                     bulk_density_field_moist),
            by = join_by("plot_code_app" == "code",
                         code_layer)) %>%
  relocate(bulk_density_field_moist, .before = mass_before_net)

# PIR: Check if lab masses of bulk density samples (after drying)
# correspond roughly with masses from the field
# PIR: Check if lab masses of bulk density samples (before drying)
# correspond roughly with masses from the field
# PIR: Check if coarse fragments reported in 2-50 mm class in the field
# correspond more or less with those from the central lab
# PIR: Check if coarse fragments percentage reported in ">2 mm" fraction
# is >= those reported in the ">50 mm" fraction


source("./src/functions/uncertainty_functions.R")

test <- df_2024 %>%
  mutate(
    moisture_content_wet = (mass_before_net - mass_after_net) / mass_before_net,
    moisture_content = (mass_before_net - mass_after_net) / mass_after_net,
    # "bulk density" (kg m-3) refers to the bulk density of fine earth,
    # defined as "the ratio of the oven-dry mass of the fine earth (< 2 mm) to
    # the volume of the fine earth and pore space of the soil"
    # (basically, the bulk density in stone-free areas)
    bulk_density =
      round(1E-3 * mass_after_net / # convert from g to kg
      (count_rings * vol_ring * 1E-6 - # convert from cm3 to m3
         (1E-3 * mass_cf / 2650) - # Volume in ring occupied by stones (< 50 mm)
                                   # in m3
                                   # TO DO: check if particle density is
                                   # reported in Survey123 app
         (1E-3 * mass_roots / 1400))) # Volume in ring occupied by roots
                                       # in m3
                                       # 1400 kg m-3 is taken from literature
                                       # but can be replaced by a better value
                                       # for the density of roots
  ) %>%
  rowwise() %>%
  mutate(bd_unc = list(add_bd_uncertainty(bulk_density))) %>%
  unnest_wider(bd_unc) %>%
  mutate(
    # Coarse fragments
    # ---
    # First data source: lab (undisturbed samples)
    # Assumption: this includes coarse fragments < 50 mm.
    # This data source can be weighed more precisely and accounts
    # to some extent for spatial variation (5 sampling points):
    # 1. Calculate mass percentage
    cf_mass_ring = 1E2 * mass_cf / (mass_after_net + mass_cf),
    # 2. Convert mass percentage to volumetric percentage
    coarse_fragment_vol_ring =
      (cf_mass_ring * bulk_density) /
      (2650 - # TO DO: check if particle density is reported in Survey123 app
         1E-2 * cf_mass_ring * (2650 - bulk_density)),
    # ---
    # Second data source: field estimation (e.g., based on profile pit)
    # This carries a bigger uncertainty (visual estimation; one sampling point)
    # but is the main data source for coarse fragments > 50 mm
    # ---
    # Calculate coarse_fragment_vol based on both data sources
    # Question: how to integrate coarse_fragment_vol_ring with
    # (coaf_50mm - coaf_2mm),
    # i.e., the visually estimated fraction between 2 - 50 mm?
    # · Lab data are more spatially representative, and measured more accurately
    #   However, they can have a bias because not all stones will be
    #   included in the ring (e.g., if you hit a stone of 40 mm with the Kopecky
    #   ring, it won't be possible to take that sample, and you will usually
    #   have a new attempt to take an undisturbed sample next to it, where there
    #   may not be stones)
    # · Field estimate will have a considerable uncertainty (e.g., often one
    #   sampling point so no spatial variation)
    coarse_fragment_vol = coarse_fragment_vol_ring + coaf_50mm
    ) %>%
  rowwise() %>%
  mutate(cf_unc = list(add_cf_uncertainty(coaf_2mm,
                                              coaf_50mm,
                                              coarse_fragment_vol_ring))) %>%
  unnest_wider(cf_unc)





glimpse(test)





# PIR: vol_ring and surface_frame should be > 0


