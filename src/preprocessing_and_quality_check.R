
# Pre-processing and quality check
# --------------------------------

# This script creates partner inconsistency reports with inconsistencies
# in the data submitted by the partner (Survey123 + forest floor masses)
# and compiles and pre-processes the data to obtain the enhanced
# layer-specific dataset (+ plot-specific metadata like soil type, weather...)

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
          require("patchwork"),
          require("plotly"),
          require("purrr"),
          require("brms"),
          require("cmdstanr"),
          require("tidybayes"),
          require("here"),
          require("sf"),
          require("terra"),
          require("stars"),
          require("ggtext"))

# Source URLs of sensitive Google Drive links
source("./data/sensitive_metadata/google_drive_links.R")




# 2. Get data ----

# In theory, it should be possible to left_join everything using either
# plot_code_simple (for plot-specific data) or plot_code_simple and code_layer
# (for layer-specific data)

## · Initial script settings

apply_corrections <- TRUE
create_inconsistency_report <- TRUE


## · Site metadata ----

if (!exists("his", envir = globalenv())) {
  source("./src/functions/get_his.R")
  his <- get_his()
}

if (!exists("wp3_sites", envir = globalenv())) {
  source("./src/functions/get_wp3_sites.R")
  wp3_sites <- get_wp3_sites()
}







## · Data Survey123 app ----

# Data Survey123 app wide format

source("./src/functions/get_app_data.R")
res_get_app_data <- get_app_data()

app_data_wide <- res_get_app_data$app_data_wide

# Convert data Survey123 app to long format

# This script also generates an inconsistency report with several
# inconsistencies

source("./src/functions/app_data_long.R")
res_app_data_long <- app_data_long(app_data_wide = app_data_wide,
                                   apply_corrections = apply_corrections)

df_layers_out <- res_app_data_long$df_layers
df_plot <- res_app_data_long$df_plot
df_sampling_points <- res_app_data_long$df_sampling_points






## · Data from local lab ----
# (spreadsheet named “forest_floor_data_[institute]”)

source("./src/functions/get_data_local_lab.R")
res_get_data_local_lab <- get_data_local_lab()

df_local_lab <- res_get_data_local_lab$df_local_lab




## · Data from sample pretreatment central lab ----

dist <- read_sheet(as_id(id_dist),
                   skip = 1)

undist <- read_sheet(as_id(id_undist),
                     skip = 1)





## · Data from analytical central lab (via inbolims) ----

# More information on https://github.com/inbo/inbolims

# Switch on VPN of INBO
cat("Switch on VPN of INBO.\n")

source("./src/functions/get_lab_data.R")
data_lab <- get_lab_data()

if (nrow(data_lab) > 0) {

  data_lab <- data_lab %>%
  # Add sample_id_simple, plot_code_simple, institute_sampling,
  # sample_code, code_layer
  # At the moment it's not possible to test this using 2025 lab data, but
  # it should work out (since sample_id should be the same like in the WBLs)
  add_ids_simple()

  assertthat::assert_that(all(!is.na(data_lab$plot_code_simple)),
                          msg = paste0("Something went wrong with ",
                                       "column 'sample_id_simple'"))
}









## · Sample lists ----
#    (Only for other partners, not INBO)

source("./src/functions/get_sample_lists.R")
samples <- get_sample_lists()

assertthat::assert_that(length(which(is.na(samples$sample_id_simple))) == 0,
                        msg = paste0("Some NAs are present in column ",
                                     "'sample_id_simple'."))





## · eDNA analyses ----

# Probably not needed in this script, but just to show how to add the columns
# sample_id_simple etc based on the sample_ids (unharmonised) in
# the 'diepvries' list:

# Apply it as follows:
source("./src/functions/add_ids_simple.R")
# [name dataframe] %>%
#       add_ids_simple(orig_harm = FALSE)

# If you want to see the samples from the freezer sample list:
# freezer <- read_sheet(as_id(id_freezer))






# 3. Combine masses for disturbed samples (forest floor) ----

layers <- c("OL", "OFH", "M01", "M13", "M36", "M61")


## 3.1. Data pretreatment disturbed ----

columns_to_check <- c("mass_recipient",
                      "mass_before_gross",
                      "mass_after_gross",
                      "mass_objects")

source("./src/functions/safe_numeric_convert.R")
source("./src/functions/add_ids_simple.R")

dist_harm <- dist %>%
  rename(sample_id = `sample_id (unieke veldcode uit Survey123 app)`,
         survey_date = `Datum bemonstering`,
         mass_recipient = `Massa (g) recipiënt zonder deksel`,
         mass_before_gross =
           `Massa (g) monster voor (inclusief recipiënt zonder deksel)`,
         mass_after_gross =
           `Massa monster (g) na (inclusief recipiënt zonder deksel)`,
         mass_objects =
           `Massa object verwijderd voor drogen (netto, g)`,
         remarks_pretrt = ...32) %>%
  # Remove records from 2024 test
  filter(!grepl("^WILDCARD2024_", sample_id)) %>%
  select(sample_id, any_of(columns_to_check),
         remarks_pretrt) %>%
  # Add sample_id_sample, plot_code_simple, institute_sampling,
  # sample_code, code_layer
  add_ids_simple() %>%
  mutate(across(all_of(columns_to_check),
                ~ safe_numeric_convert(.x, cur_column())))

assertthat::assert_that(
  all(is.na(dist_harm$mass_before_gross) | !is.na(dist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_before is not NA.")

assertthat::assert_that(
  all(is.na(dist_harm$mass_after_gross) | !is.na(dist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_after is not NA.")

dist_harm <- dist_harm %>%
  # Calculate net mass
  mutate(
    mass_before =
      # Add any objects that were removed before first weighing and drying
      # of sample pretreatment central lab. This way, any objects that do not
      # belong in OFH are included in the mass ratio after versus before
      # drying (in the assumption that this fraction is representative for
      # the whole layer).
      mass_before_gross + coalesce(mass_objects, 0) - mass_recipient,
    mass_after = mass_after_gross - mass_recipient)









## 3.2. Combine all masses for disturbed layers per layer ----

# We have the following masses for OFH (in chronological order):
# 1. Field-moist all points (app) → column "tamass"
# 2. Field-moist subsample (app) → column "dist"
# 3. Local lab partner → column "mass_dry_partner"
# 4. Central lab WBL before → column "mass_before"
# 5. Central lab WBL after → column "mass_after"

# Here, we combine:
# · Masses from WBL disturbed central lab
# · Masses from local lab (forest floor)
# · df_layers (Survey123 app), which contains field-moist masses

dist_masses <-
  bind_rows(
    # 1. Central lab samples from other partners
    # Part 2 other partners: from central lab sample pretreatment (dist)
    dist_harm %>%
      filter(institute_sampling != "INBO") %>%
      select(plot_code_simple, sample_id_simple,
             code_layer, mass_before, mass_after) %>%
      # ---
      # Mistakes: switched samples at central lab (pretreatment disturbed)
      # ---
      mutate(
        plot_code_simple = case_when(
          sample_id_simple == "VUK__1__b__OFH_carbon" ~
            "VUK__1__Zofin__c",
          sample_id_simple == "VUK__1__c__OFH_carbon" ~
            "VUK__1__Zofin__b",
          TRUE ~ plot_code_simple
          ),
        sample_id_simple = case_when(
          code_layer == "OFH" & plot_code_simple == "VUK__1__Zofin__b" ~
            "VUK__1__b__OFH_carbon",
          code_layer == "OFH" & plot_code_simple == "VUK__1__Zofin__c" ~
            "VUK__1__c__OFH_carbon",
          TRUE ~ sample_id_simple
        )) %>%
      mutate(
        mass_dry_leaves = NA_real_,
        mass_dry_twigs_medium = NA_real_,
        mass_dry_twigs_small = NA_real_),
    # 2. From INBO
    dist_harm %>%
      # 2. Part 1 INBO: Create OL subtotals
      filter(code_layer == "OL") %>%
      mutate(
        sample_id_simple = gsub("_OL_.*$", "_OL_flammability",
                                sample_id_simple)) %>%
      select(plot_code_simple, sample_id_simple,
             sample_code, mass_after) %>%
      pivot_wider(
        names_from = sample_code,
        values_from = mass_after,
        names_prefix = "mass_dry_"
      ) %>%
      mutate(
        code_layer = "OL",
        mass_after = rowSums(across(starts_with("mass_dry_")), na.rm = TRUE)
      ) %>%
      rename(
        mass_dry_leaves = `mass_dry_OL_leaves`,
        mass_dry_twigs_medium = `mass_dry_OL_twigs_medium`,
        mass_dry_twigs_small = `mass_dry_OL_twigs_small`
      ) %>%
      left_join(
        dist_harm %>%
          filter(code_layer == "OL") %>%
          group_by(plot_code_simple) %>%
          reframe(
            mass_before = ifelse(
              any(!is.na(mass_before)),
              sum(mass_before, na.rm = TRUE),
              NA_real_)),
        by = "plot_code_simple") %>%
      select(plot_code_simple, sample_id_simple,
             code_layer, mass_before, mass_after,
             mass_dry_leaves, mass_dry_twigs_medium,
             mass_dry_twigs_small) %>%
      # 2. Part 2 INBO: Bind with non-OL rows
      bind_rows(
        dist_harm %>%
          filter(institute_sampling == "INBO") %>%
          filter(code_layer != "OL") %>%
          filter(!grepl("BackUp", sample_id)) %>%
          select(plot_code_simple, sample_id_simple,
                 code_layer, mass_before, mass_after) %>%
          mutate(
            mass_dry_leaves = NA_real_,
            mass_dry_twigs_medium = NA_real_,
            mass_dry_twigs_small = NA_real_))
    ) %>%
  full_join(
    # 3. Local lab data other partners
    df_local_lab %>%
      mutate(
        mass_dry_partner =
          rowSums(across(starts_with("mass_dry_")), na.rm = TRUE)) %>%
      select(plot_code_simple, code_layer, sample_id_simple,
             mass_dry_partner, mass_dry_leaves_ol, mass_dry_twigs_medium_ol,
             mass_dry_twigs_small_ol),
    by = join_by("plot_code_simple", "code_layer", "sample_id_simple")
  ) %>%
  mutate(code_layer = factor(code_layer,
                             levels = layers)) %>%
  # Arrange by plot and layer
  arrange(plot_code_simple, code_layer) %>%
  relocate(mass_dry_partner, .before = mass_before) %>%
  # Combine the OL fraction columns
  # (since those for Belgium are in a different column than the others)
  mutate(
    mass_dry_leaves = coalesce(mass_dry_leaves,
                               mass_dry_leaves_ol),
    mass_dry_twigs_medium = coalesce(mass_dry_twigs_medium,
                                     mass_dry_twigs_medium_ol),
    mass_dry_twigs_small = coalesce(mass_dry_twigs_small,
                                    mass_dry_twigs_small_ol)) %>%
  # Calculate percentage leaves, twigs medium, twigs small for OL
  mutate(
    perc_dry_leaves = case_when(
      grepl("^INBO__", plot_code_simple) ~
        round(1E2 * mass_dry_leaves / mass_after, 1),
      TRUE ~ round(1E2 * mass_dry_leaves / mass_dry_partner, 1)),
    perc_dry_twigs_medium = case_when(
      grepl("^INBO__", plot_code_simple) ~
        round(1E2 * mass_dry_twigs_medium / mass_after, 1),
      TRUE ~ round(1E2 * mass_dry_twigs_medium / mass_dry_partner, 1)),
    perc_dry_twigs_small = case_when(
      grepl("^INBO__", plot_code_simple) ~
        round(1E2 * mass_dry_twigs_small / mass_after, 1),
      TRUE ~ round(1E2 * mass_dry_twigs_small / mass_dry_partner, 1))
    ) %>%
  select(-mass_dry_leaves_ol, -mass_dry_twigs_medium_ol,
         -mass_dry_twigs_small_ol)



# Combine with df_layers (layer-specific data Survey123 app)

df_layers <- df_layers_out %>%
  left_join(dist_masses %>%
              # We need to use the masses of the newest sample ("OFH_carbon_b")
              # since it is more reliable (living roots were removed more
              # meticulously).
              filter(sample_id_simple != "LWF__65_a__NA__OFH_carbon"),
            by = join_by("plot_code_simple", "code_layer")) %>%
  relocate(mass_dry_partner, mass_before, mass_after,
           .after = dist) %>%
  mutate(code_layer = factor(code_layer,
                             levels = layers)) %>%
  arrange(institute_sampling, plot_code_simple, layer_number)




## 3.3. Calculate areal_mass (dry) for forest floor (t ha-1) ----

# We have the following masses for OFH (in chronological order):
# 1. Field-moist all points (app) → column "tamass"
# 2. Field-moist subsample (app) → column "dist"
# 3. Local lab partner → column "mass_dry_partner"
# 4. Central lab WBL before → column "mass_before"
# 5. Central lab WBL after → column "mass_after"

# In theory, masses should decrease in this order, although 3 and 4 should
# be the same, and 5 should also not be much lighter than 4 since partners
# are supposed to have dried the samples properly.

# In theory (partner samples), we should proceed as follows:
# - First calculate the moisture content based on:
#   mass_dry_partner versus dist, and mass_after versus mass_before
# - Calculate dry areal mass based on tamass, surface_frame, count_ff_frames
#   and the moisture content

# However, there seem to be some exceptions (typically per partner):
# - It seems so far that some of the samples were not yet dry when they
#   arrived at the central lab. This was visible for the UNIUD and BFNP
#   samples upon arrival. Also, during sample pretreatment at the central lab,
#   we sometimes observed inconsistencies with the definition of OFH, sometimes
#   not possible to solve (e.g., too much mineral matrix in OFH), and sometimes
#   possible to solve, e.g., the presence of unwanted objects such as
#   too big cones, coarse fragments etc. Those are removed in the central lab,
#   and their mass is reflected in the ratio of mass_after to mass_before
#   of the central lab. This ratio therefore reflects remaining
#   soil moisture and unwanted objects. In the assumption that the
#   concentration of these objects in the central lab subsample is
#   representative for their concentration in the the complete OFH layer,
#   we will include this ratio in the soil moisture ratio determined based
#   on the local lab masses.
# - BFNP: they only provided local lab masses for OL, not OFH. They forgot to
#   record OFH masses after drying. Some lose a lot of weight in the central lab
#   (e.g., Hoellbachgespreng). mass_before seems in line with dist, but
#   mass_after is sometimes surprisingly low. OL masses seem plausible.
# - UNIUD: they seem to have switched the sequence: they probably took the
#   whole composite sample across 5 sampling points to the local lab,
#   dried it and recorded the mass ("mass_dry_partner"), then took a subsample
#   (dry) and reported this subsample's mass in the app ("dist"). It makes
#   sense because they filled in the app afterwards. Some OFH samples from
#   Cansiglio and Val Alba were indeed clearly moist upon arrival in the
#   central lab. The moisture content needs to be a combination of
#   mass_dry_partner versus tamass and mass_after versus mass_before
# - WR: values reported under "dist" for OL look implausible
#   (with mass_dry_partner higher than dist and plausible in comparison with
#   tamass). Probably, the full sample (tamass) was taken to the local lab
#   for drying.
# - INBO: no "mass_dry_partner". "mass_before" should therefore
#   be similar to "dist". There must be some typos, e.g., Bos Terrijst.



if (apply_corrections) {

  df_layers <- df_layers %>%
    mutate(
      # Corrections for UNIUD samples
      dist_orig = dist,
      mass_dry_partner_orig = mass_dry_partner,
      mass_dry_partner = case_when(
        # UNIUD:
        # Mass of dried subsample local lab is probably submitted under "dist"
        institute_sampling == "UNIUD" ~ dist_orig,
        TRUE ~ mass_dry_partner),
      dist = case_when(
        # UNIUD:
        # Multiply moist-to-dry ratio with the mass of the dried subsample
        # from the local lab in order to obtain what should have been "dist"
        # (mass of field-moist subsample)
        # In other words: what is now reported as:
        # dist * (tamass / mass_dry_partner)
        institute_sampling == "UNIUD" ~
          round(dist_orig * (tamass / mass_dry_partner_orig), 2),
        # WR:
        # dist seems incorrect for OL layers, and it seems like they
        # must have taken the full OL sample to the local lab based on the data
        institute_sampling == "WR" & code_layer == "OL" ~
          tamass,
        TRUE ~ dist)
    ) %>%
    select(-dist_orig, -mass_dry_partner_orig)

  # Solve inconsistencies

  before_to_dry_partner_plaus <- c(0.9, 1.1)
  after_to_before_plaus <- c(0.8, 1.01)

  df_layers <- df_layers %>%
    # Solve inconsistencies in forest floor masses
    mutate(
      mass_before = case_when(
        # Must have been a typo based on the field forms
        # (Original: 666.44)
        plot_code_simple == "INBO__11__Bos Terrijst__NA" &
          code_layer == "OFH" ~ 566.44,
        TRUE ~ mass_before
      ),
      mass_dry_partner = case_when(
        # Typo as confirmed by the partner (originally 720.9),
        plot_code_simple == "VUK__38__Novorecky mocal__NA" &
          code_layer == "OFH" ~ 72.9,
        TRUE ~ mass_dry_partner)
    ) %>%
    # Remove inconsistent forest floor masses
    mutate(
      before_to_dry_partner = case_when(
        institute_sampling == "INBO" ~ mass_before / dist,
        TRUE ~ mass_before / mass_dry_partner),
      after_to_before = mass_after / mass_before,
      # Is ratio of mass_before (central lab) to mass_dry_partner (local lab)
      # plausible? (should be ~ 1)
      # Note that mass_dry_partner is missing for INBO and BFNP
      b2dp_flag = (wp == "WP2" &
                     (!is.na(mass_dry_partner) | institute_sampling == "INBO") &
                     code_layer == "OFH" &
                     (before_to_dry_partner < before_to_dry_partner_plaus[1] |
                        before_to_dry_partner >
                        before_to_dry_partner_plaus[2])),
      # Is ratio of mass_after to mass_before (central lab) plausible?
      # (Except for BFNP, UNIUD and INBO, for which mass_before was not
      #  (entirely) dry, this should be close to 1)
      a2b_flag = (wp == "WP2" &
                    !institute_sampling %in% c("BFNP", "UNIUD", "INBO") &
                    code_layer == "OFH" &
                    (after_to_before < after_to_before_plaus[1] |
                       after_to_before > after_to_before_plaus[2])),
      # Scenario 1:
      # if mass_before is quite different from both mass_dry_partner and
      # mass_after, assume that there is a mistake (e.g., typo) in mass_before
      # Action: replace by NA
      # This is the case for
      #  "WULS__1__Bialowieza National Park__NA__Transect IV",
      #  "FVA-BW__15__Waldmoor-Torfstich__1125", "VUK__1__Zofin__c"
      #  "INBO__1__Zonienwoud-Joseph Zwaenepoel reservaat__1stExtension"
      mass_before = ifelse(((institute_sampling == "INBO" & b2dp_flag) |
                              (b2dp_flag & a2b_flag)),
                           NA_real_,
                           mass_before),
      # Scenario 2:
      # if mass_after is unexpectedly quite different from mass_before while
      # mass_before is consistent with mass_dry_partner, assume that there
      # is a mistake (e.g., typo) in mass_after
      # Action: replace by NA
      # This is the case for "FVA-BW__543__Schnepfenmoos__NA",
      # "UNITBV__1__Nera__NA", "WR__9__Pijpebrandje__NA"
      mass_after = ifelse((!b2dp_flag & a2b_flag), NA_real_, mass_after)
      # Scenario 3:
      # if mass_before is quite different from mass_dry partner but
      # consistent with mass_after (i.e., b2dp_flag & !a2b_flag),
      # assume that the moisture content of the central lab
      # (dry_to_moist_central) is still realistic (manually verified).
      # No need to take any action.
      # This is the case for "NWFVA__03-011__Walbecker Warte__NA",
      # "UL__8__Krokar__NA", "UL__9__Gorjanci__NA",
      # "DISAFA - UNITO__9__Paneveggio__NA", "WSL__25__Seeliwald__NA"
      # Some feedback from partners (for cases for which mass of
      # sample in sample list upon shipment ("samples") was reported and
      # points out that there is an issue in mass_dry_partner):
      # "UL__9__Gorjanci__NA": part of sample accidentally lost before shipment
      # (this confirms that our equation for dry_to_moist already solves
      #  the mismatch)
    ) %>%
    select(-any_of(c(
      "before_to_dry_partner", "after_to_before",
      "b2dp_flag", "a2b_flag"
    )))


} # End of "if apply_corrections"





df_layers <- df_layers %>%
  mutate(
    # Dry mass fraction local lab
    # i.e., dry mass as determined in the local lab relative to field-moist mass
    dry_to_moist_local = ifelse(
      code_layer %in% c("OL", "OFH"),
      case_when(
        institute_sampling == "INBO" ~ pmin(mass_after / dist, 1),
        institute_sampling == "BFNP" & code_layer == "OFH" ~
          # No local lab mass reported
          pmin(mass_before / dist, 1),
        # Partners with the "normal" sample flow
        TRUE ~ pmin(mass_dry_partner / dist, 1)),
      NA_real_),
    # Dry mass fraction central lab
    # i.e., dry mass as determined in the central lab relative to the mass
    # of the sample coming from the local lab (in theory dry, but often
    # contains remaining moisture or unwanted objects such as stones, cones...
    # which are hence reflected in this ratio)
    # This is only available for OFH though
    dry_to_moist_central = ifelse(
      code_layer == "OFH",
      case_when(
        institute_sampling == "INBO" ~ 1,
        # "RO__USV__1__NA__1__OFH_carbon": Part was spilled during
        # oven-drying, so better to use dry_to_moist_local only
        plot_code_simple == "USV__1__Vanatori Natural Park (core area)__NA" ~ 1,
        is.na(mass_before) | is.na(mass_after) ~ NA_real_,
        # Default:
        # ratio of mass_after to mass_before
        # Take minimum of mass_before and mass_dry_partner
        # (should in theory be the same)
        TRUE ~
          pmin(mass_after / pmin(mass_before, mass_dry_partner, na.rm = TRUE),
               1)),
      NA_real_))

# In order to gap-fill dry_to_moist_central
# (for a few records where either mass_before or mass_after were implausible -
# only for partners for which moisture contents at central lab were consistent
# and negligible, and only for records for which no unwanted objects were
# observed, i.e., different before and after drying only due to moisture)

df_dry_to_moist_central <- df_layers %>%
  filter(wp == "WP2") %>%
  filter(code_layer == "OFH") %>%
  filter(!institute_sampling %in% c("BFNP", "UNIUD", "INBO")) %>%
  # Remove records with remarks reported during pretreatment central lab
  # (often due to unwanted objects or other inconsistencies)
  left_join(dist_harm %>%
              select(plot_code_simple, code_layer, sample_id_simple,
                     remarks_pretrt),
            by = join_by("plot_code_simple", "code_layer",
                         "sample_id_simple")) %>%
  filter(is.na(remarks_pretrt)) %>%
  group_by(wp, institute_sampling) %>%
  reframe(
    dry_to_moist_central_mean = ifelse(
      any(!is.na(dry_to_moist_central)),
      mean(dry_to_moist_central, na.rm = TRUE),
      NA_real_),
    dry_to_moist_central_min = ifelse(
      any(!is.na(dry_to_moist_central)),
      min(dry_to_moist_central, na.rm = TRUE),
      NA_real_),
    dry_to_moist_central_max = ifelse(
      any(!is.na(dry_to_moist_central)),
      max(dry_to_moist_central, na.rm = TRUE),
      NA_real_),
    diff = dry_to_moist_central_max - dry_to_moist_central_min)


df_layers <- df_layers %>%
  # Gap-fill moisture contents central lab
  left_join(df_dry_to_moist_central %>%
              select(wp, institute_sampling, dry_to_moist_central_mean),
            by = join_by("wp", "institute_sampling")) %>%
  mutate(
    dry_to_moist_central = case_when(
      code_layer == "OFH" ~ coalesce(dry_to_moist_central,
                                     dry_to_moist_central_mean),
      TRUE ~ dry_to_moist_central)
  ) %>%
  select(-any_of(c("dry_to_moist_central_mean"))) %>%
  # Copy moisture contents central lab from OFH to OL per plot
  group_by(plot_code_simple) %>%
  mutate(
    dry_to_moist_central = ifelse(
      code_layer == "OL",
      dry_to_moist_central[code_layer == "OFH"][1],
      dry_to_moist_central)
  ) %>%
  ungroup() %>%
  mutate(
    dry_to_moist = round(dry_to_moist_local * dry_to_moist_central , 3),
    # kg m-2 (1 kg m-2 = 10 t ha-1)
    areal_mass = ifelse(
      code_layer %in% c("OL", "OFH"),
      round(
      tamass * 1E-3 * # kg field-moist material over x sampling frame areas
        dry_to_moist / # g dry material / g field-moist material
        (surface_frame * count_ff_frames), 2), # m2 covered by all frames
      NA_real_),
    # Minimum and maximum of areal mass, based on the variation
    # between sampling points
    across(
      c(tamass_min, tamass_max),
      ~ ifelse(
        code_layer %in% c("OL", "OFH"),
        round(
          .x * 1E-3 * # kg - This is for one sampling point (one frame)
            dry_to_moist /
            surface_frame, # m2 covered by one frame
          2
        ),
        NA_real_
      ),
      .names = "areal_mass_{sub('tamass_', '', .col)}"
    ),
    # Add bulk_density based on areal mass (kg m-3)
    # (for Seeliwald, where original so-called OL and OFH layers become
    #  below-ground)
    across(
      c(areal_mass,
        areal_mass_min,
        areal_mass_max),
      ~ if_else(
        !is.na(.x),
        round(
          .x / # kg m-2
            (thickness * 1E-2), # thickness in cm to m
          2
        ),
        NA_real_
      ),
      .names = "bulk_density_dist{sub('areal_mass', '', .col)}"
    )
    )


df_layers_ff <- df_layers









if (create_inconsistency_report) {

  ### INCONSISTENCY 13 ----

  rule <- paste0("Mass of subsample after drying in local lab is higher than ",
                 "mass of field-moist subsample (after mixing).")

  rule_id <- "13"

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



  # Records with a problem

  recs_problem <- df_layers %>%
    filter(code_layer %in% c("OL", "OFH")) %>%
    filter(mass_dry_partner > dist)

  inc <- recs_problem %>%
    mutate(
      inconsistency_reason = rule,
      dist = as.character(dist),
      mass_dry_partner = as.character(mass_dry_partner),
      mass_dry_leaves = as.character(mass_dry_leaves),
      mass_dry_twigs_medium = as.character(mass_dry_twigs_medium),
      mass_dry_twigs_small = as.character(mass_dry_twigs_small)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c(dist, mass_dry_partner, mass_dry_leaves, mass_dry_twigs_medium,
               mass_dry_twigs_small),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    filter(
      !(code_layer == "OL" & parameter == "mass_dry_partner")
    ) %>%
    filter(
      !(code_layer == "OFH" & grepl("leaves|twigs", parameter))
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
        parameter == "mass_dry_partner" ~
          paste0("Net mass determined after drying in the local lab of OFH ",
                 "subsample taken from the field"),
        parameter == "mass_dry_leaves" ~
          paste0("Net mass determined after drying in the local lab of ",
                 "'leaves' in OL subsample taken from the field"),
        parameter == "mass_dry_twigs_medium" ~
          paste0("Net mass determined after drying in the local lab of ",
                 "'twigs-medium' in OL subsample taken from the field"),
        parameter == "mass_dry_twigs_small" ~
          paste0("Net mass determined after drying in the local lab of ",
                 "'twigs-small' in OL subsample taken from the field"),
        TRUE ~ parameter_description),
      parameter_unit = "g") %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = (grepl("_flamm|_carbon", parameter)),
      source = as.character(ifelse(grepl("_flamm|_carbon", parameter),
                                   "Survey123 app",
                                   "Forest floor data (local lab)")),
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




  ### INCONSISTENCY 14 ----

  rule <- paste0("Mass recorded after drying in local lab must roughly ",
                 "correspond with mass of OFH sample received by central lab")

  rule_id <- "14"


  # Records with a problem

  before_to_dry_partner_plaus <- c(0.9, 1.1)
  after_to_before_plaus <- c(0.8, 1.01)

  recs_problem <- df_layers %>%
    filter(code_layer == "OFH") %>%
    mutate(
      before_to_dry_partner = case_when(
        institute_sampling == "INBO" ~ mass_before / dist,
        TRUE ~ mass_before / mass_dry_partner),
      after_to_before = mass_after / mass_before,
      b2dp_flag = (wp == "WP2" &
                     (!is.na(mass_dry_partner) | institute_sampling == "INBO") &
                     (before_to_dry_partner < before_to_dry_partner_plaus[1] |
                        before_to_dry_partner >
                        before_to_dry_partner_plaus[2])),
      a2b_flag = (wp == "WP2" &
                    institute_sampling != "INBO" &
                    (after_to_before < after_to_before_plaus[1] |
                       after_to_before > after_to_before_plaus[2]))
    ) %>%
    # Inconsistency potentially caused by partner:
    # mass_before is quite different from mass_dry partner but
    # consistent with mass_after (i.e., b2dp_flag & !a2b_flag)
    # This is the case for "NWFVA__03-011__Walbecker Warte__NA",
    # "UL__8__Krokar__NA", "UL__9__Gorjanci__NA",
    # "DISAFA - UNITO__9__Paneveggio__NA", "WSL__25__Seeliwald__NA"
    filter(b2dp_flag & !a2b_flag)

  inc <- recs_problem %>%
    mutate(
      inconsistency_reason =  paste0(rule,
                                     " [net: ", mass_before, " g]"),
      mass_dry_partner = as.character(mass_dry_partner)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c(mass_dry_partner),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    mutate(
      parameter_description =
          paste0("Net mass determined after drying in the local lab of OFH ",
                 "subsample taken from the field"),
      parameter_unit = "g") %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency = TRUE,
      source = "Forest floor data (local lab)",
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













# 4. Combine undisturbed info for bulk density and coarse fragments ----

## 4.1. Data pretreatment undisturbed ----

glimpse(undist)

columns_to_check <- c("mass_recipient_residu",
                      "mass_recipient",
                      "mass_before_gross",
                      "mass_after_total_gross",
                      "mass_after_fe_gross",
                      "mass_cf_wet_siev_gross",
                      "mass_cf_orig",
                      "vol_cf_before",
                      "vol_cf_after",
                      "mass_roots")

source("./src/functions/safe_numeric_convert.R")
source("./src/functions/add_ids_simple.R")

undist_harm <- undist %>%
  rename(
    sample_id = `sample_id (unieke veldcode uit Survey123 app)`,
    survey_date = `Datum bemonstering`,
    # This column contains the gross mass of the original recipient from the
    # field (ziploc bag) with any (moist) residu left after transferring
    # its content to the oven recipient. The mass of the clean empty recipient
    # (tare; ziploc bag) with sticker is 25.11 g for the Belgian samples.
    # For the correction of the foreign samples, we will use an estimate based
    # on the Belgian samples since we do not know the tare weight of the
    # clean recipients of the partners.
    mass_recipient_residu = `Massa ziploc met residu na legen (g) (tarra)`,
    # This column contains the tare mass of the oven recipient (black container)
    mass_recipient = `Massa (g) oven-recipiënt zonder deksel`,
    # This column contains the gross mass of the full sample after drying
    # (incl. coarse fragments). The column was added after it became clear
    # that dry sieving is difficult. The mass of any coarse fragments or
    # roots (determined after wet sieving) needs to be subtracted from its net
    # value to obtain the dry mass of fine earth.
    mass_after_total_gross =
      `Massa (g) monster na drogen (inclusief recipiënt zonder deksel)`,
    # This column contains the mass of coarse fragments (including recipient,
    # i.e., gross) after wet sieving, recorded after 26 March 2026 evening.
    # See details below (mass_cf).
    mass_cf_wet_siev_gross =
      `Massa steentjes (> 2 mm) (g) droog met recipiënt (na wet sieving)`,
    # This column contains the net or gross mass of any dry coarse fragments
    # Details: This column contains all coarse fragments masses determined
    # before 26 March 2026 evening - either after dry sieving (old method)
    # or wet sieving (new method). Usually the recorded mass is the net mass
    # except when the column "is_mass_cf_gross" says "Ja". However, there
    # are some samples for which part of the stones was previously
    # separated, weighed (with weighed recorded in this column) and discarded,
    # while the sample could not be properly sieved with only dry sieving
    # and therefore still contains stones. For those samples, the remainder
    # was again sieved using wet sieving, for which the mass is recorded
    # in the new column "mass_cf_wet_siev". For those samples, the recorded
    # volume corresponds with the coarse fragments mass in the new column
    # "mass_cf_wet_siev_gross" (not "mass_cf_orig"). If only the old column
    # "mass_cf_orig" is non-NA, the volume of stones corresponds with
    # that mass. The final mass of coarse fragments is the sum of both columns
    # (in most cases, only one of both columns is non-NA). Note that this
    # column is sometimes shows the same mass as mass_cf_orig (or mass_cf_orig
    # of some compiled samples) due to accidental reweighing since mass_cf_orig
    # was originally hidden. Whether this is true, is indicated in column
    # "ignore_mass_cf_wet_siev".
    mass_cf_orig = `Massa steentjes (> 2 mm) (g) droog zonder recipiënt`,
    # This column contains the tare volume (mL) of the cilinder with water
    # BEFORE coarse fragments were added
    # through water displacement after wet sieving
    vol_cf_before = `Volume VOOR steentjes toevoegen (ml)`,
    # This column contains the gross volume (mL) of the cilinder with water
    # AFTER coarse fragments were added (i.e., volume of coarse fragments
    # through water displacement after wet sieving)
    vol_cf_after = `Volume NA steentjes toevoegen (ml)`,
    # This column contains the net mass of any dry roots
    mass_roots =
      `Massa wortels (droog; indien aanzienlijk) (g) zonder recipiënt`,
    # Remarks during pretreatment
    remarks_pretrt = ...25,
    # This column is a logical ("Ja" for TRUE, NA for FALSE) indicating
    # whether column "mass_cf" includes the recipient (i.e., gross mass)
    is_mass_cf_gross = `Massa steentjes (oude kolom U) inclusief bakje?`,
    # Should column "mass_cf_wet_siev_gross" be ignored because the mass of
    # the same sample (or compiled samples) for which the mass was already
    # recorded in column "mass_cf_orig" was re-recorded?
    ignore_mass_cf_wet_siev =
      `Kolom T to be ignored? (compiled sample)`
    ) %>%
  # Using rename_with because of too long column name
  # This column contains the gross mass of the full sample or the fine earth
  # (after dry sieving) before it is placed in the oven at 105 °C
  rename_with(~"mass_before_gross",
              all_of(paste0("Massa (g) monster voor ",
                            "(inclusief oven-recipiënt zonder ",
                            "deksel, zonder steentjes en ",
                            "zonder los label)"))) %>%
  # This column contains the gross mass of the dry fine earth sample after
  # drying (excl. coarse fragments).
  rename_with(~"mass_after_fe_gross",
              all_of(paste0("Massa fine earth (g) na ",
                            "(inclusief recipiënt zonder ",
                            "deksel, zonder steentjes, ",
                            "zonder los label)"))) %>%
  # Remove records from 2024 test
  filter(!grepl("^WILDCARD2024_", sample_id)) %>%
  select(sample_id, survey_date, any_of(columns_to_check),
         remarks_pretrt, is_mass_cf_gross, ignore_mass_cf_wet_siev) %>%
  # Add sample_id_sample, plot_code_simple, institute_sampling,
  # sample_code, code_layer
  add_ids_simple() %>%
  mutate(
    mass_before_gross = case_when(
      # Typo by lab pretreatment colleagues
      grepl("yui", mass_before_gross) ~ NA,
      TRUE ~ mass_before_gross),
    vol_cf_before = case_when(
      vol_cf_before == "/" ~ NA,
      TRUE ~ vol_cf_before)
    ) %>%
  mutate(across(all_of(columns_to_check),
                ~ safe_numeric_convert(.x, cur_column())),
         remarks_pretrt = case_when(
           trimws(toupper(as.character(remarks_pretrt))) == "NULL" ~
             NA_character_,
           TRUE ~ as.character(remarks_pretrt))) %>%
  mutate(
    # Corrections in WBL data
    mass_recipient = case_when(
      # Total sample mass and mass recipient not recorded (forgotten?),
      # but coarse fragments were determined:
      # assume mean mass_recipient from this plot
      sample_id == "PL__WULS__1__NA__Transect III__M13_Bulk_Density" ~ 27.3,
      TRUE ~ mass_recipient),
    # Some special cases in which the CF values need to be gap-filled or
    # replaced by 0
    cf_zero =
      # Some samples for which mass coarse fragments was missing in WBL,
      # but which are clearly without stones based on other information
      sample_id %in% c(
        "DE__NWFVA__03-077__NA__NA__M61_Bulk_Density",
        "SI__UL__6__NA__NA__M13_Bulk_Density",
        "SI__UL__6__NA__NA__M36_Bulk_Density",
        "HR__UNIUD/HSI__3__LI-OG__NA__M01_Bulk_Density",
        "HR__UNIUD/HSI__3__LI-OG__NA__M13_Bulk_Density",
        "HR__UNIUD/HSI__3__LI-OG__NA__M36_Bulk_Density",
        "HR__UNIUD/HSI__3__LI-OG__NA__M61_Bulk_Density"
      ) |
      # Also for INBO NAs
      (grepl("^BE-INBO", sample_id) & is.na(mass_cf_orig) &
         is.na(mass_cf_wet_siev_gross)) |
      # Some INBO samples were processed using dry sieving, and the sieving
      # residu consisted of soil aggregates rather than stones, so better
      # to ignore those so-called "coarse fragments"
      (grepl("^BE-INBO", sample_id) &
         grepl("Geen echte steentjes", remarks_pretrt)),
    # Apply 0s
    across(
      c(mass_cf_wet_siev_gross, vol_cf_before),
      ~ ifelse(cf_zero, 0, .)
    ),
    vol_cf_after = ifelse(cf_zero, NA_real_, vol_cf_after)) %>%
  select(-any_of("cf_zero")) %>%
  # Remove INBO WP3 samples
  filter(!grepl("^BE-INBO-INBO_", sample_id))


assertthat::assert_that(
  all(is.na(undist_harm$mass_before_gross) |
        !is.na(undist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_before is not NA.")

assertthat::assert_that(
  all(is.na(undist_harm$mass_after_fe_gross) |
        !is.na(undist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_after (fine earth) is not NA.")

assertthat::assert_that(
  all(is.na(undist_harm$mass_after_total_gross) |
        !is.na(undist_harm$mass_recipient)),
  msg = "mass_recipient must not be NA when mass_after (total) is not NA.")




mass_clean_ziploc_inbo <- 25.11 # g (with sticker)
mass_recipient_residu <- # Assumption: field-moist
  mean(undist_harm$mass_recipient_residu, na.rm = TRUE) - mass_clean_ziploc_inbo

# 1400 kg m-3 is taken from literature
# but can be replaced by a better value
# for the density of roots
density_roots <- 1400


undist_harm <- undist_harm %>%
  # Calculate net mass
  # mass_cf and mass_roots are net already
  mutate(
    mass_cf_wet_siev = case_when(
      grepl("ignore", ignore_mass_cf_wet_siev, ignore.case = TRUE) ~ NA_real_,
      # Bigger than 0 means it should be gross
      (!is.na(mass_cf_wet_siev_gross) & mass_cf_wet_siev_gross > 0) ~
        mass_cf_wet_siev_gross - mass_recipient,
      TRUE ~ mass_cf_wet_siev_gross),
    mass_cf_siev = case_when(
      !is.na(mass_cf_orig) &
        mass_cf_orig > 0 &
        grepl("ja", is_mass_cf_gross, ignore.case = TRUE) ~
        mass_cf_orig - mass_recipient,
      TRUE ~ mass_cf_orig),
    mass_cf =
      ifelse(!is.na(mass_cf_wet_siev) | !is.na(mass_cf_siev),
             pmax(coalesce(mass_cf_wet_siev, 0) + coalesce(mass_cf_siev, 0),
                  0, na.rm = TRUE),
             NA_real_),
    # Mass of the coarse fragments (fraction) for which volume was determined
    # (i.e., for some samples for which part of the coarse fragments was first
    #  removed via dry sieving, and the remainder was later removed via
    #  wet sieving, for which the volume was then determined)
    mass_cf_for_vol =
      ifelse(
        (!is.na(mass_cf_wet_siev) &
           mass_cf_wet_siev > 0 &
           !is.na(mass_cf_siev) &
           mass_cf_siev > 0),
        mass_cf_wet_siev,
        mass_cf),
    vol_cf = case_when(
      !is.na(vol_cf_before) & is.na(vol_cf_after) & vol_cf_before == 0 ~ 0,
      TRUE ~ vol_cf_after - vol_cf_before),
    # Estimated mass based on reported volume (to detect any inconsistencies)
    mass_from_vol =
      (vol_cf_after - vol_cf_before) * 2.39,
    # Particle density in kg m-3
    particle_density = ifelse(
      !is.na(mass_cf_for_vol) & !is.na(vol_cf) &
        mass_cf_for_vol > 0 & vol_cf > 0,
      1E3 * mass_cf_for_vol / vol_cf,
      NA_real_),
    # Net mass of undisturbed samples including coarse fragments before drying
    mass_before = case_when(
      # Old method: coarse fragments were separated before recording mass_before
      # and putting the sample in the oven. In other words, original
      # mass_before represents fine earth only for the samples that were
      # treated using the old method.
      # Add mass of coarse fragments (only those using the old dry sieving
      # method) and any roots.
      # Double-checked with version history: mass coarse fragments determined
      # using old method is always in mass_cf_siev (not mass_cf_wet_siev)
      !is.na(mass_after_fe_gross) ~
        mass_before_gross + coalesce(mass_cf_siev, 0) +
        coalesce(mass_roots, 0) - mass_recipient,
      # Else (new method), coarse fragments are already included
      TRUE ~ mass_before_gross - mass_recipient),
    # Analogous for mass_after
    mass_after = case_when(
      # Old method:
      # Add mass of coarse fragments (mass_cf_siev) and any roots.
      !is.na(mass_after_fe_gross) ~
        mass_after_fe_gross + coalesce(mass_cf_siev, 0) +
        coalesce(mass_roots, 0) - mass_recipient,
      # Else (new method), coarse fragments are already included
      TRUE ~ mass_after_total_gross - mass_recipient))

if (apply_corrections) {

  # Plausible range of observed particle densities
  # 80 % quantile after filtering for vol_cf > 10
  part_dens_plaus <- c(1976.706, 2724.126)

  # Remove implausible volumes and particle densities

  # Assumption: limit of quantification of volume determination is 5 mL
  # (below that, it is difficult to identify differences in the measuring
  #  cylinder by water displacement)

  # Assumption: if there is an inconsistency between mass and volume of
  # coarse fragments, the mass is usually more reliable than the volume
  # (manually verified)

  undist_harm <- undist_harm %>%
    mutate(
      vol_cf = ifelse(
        !is.na(vol_cf) &
          (vol_cf < 5 | particle_density < part_dens_plaus[1] |
             particle_density > part_dens_plaus[2]),
        NA_real_,
        vol_cf),
      particle_density = ifelse(
        !is.na(vol_cf),
        particle_density,
        NA_real_)) %>%
    mutate(
      # Special case for "WSL__53__Murgtal__NA" M61 (100 cm3) that was splitted
      # in order to subsample a disturbed sample for the analytical lab
      # "count_rings" was already adjusted (including remaining undisturbed
      # sample and stone, which was not included in WBL-undisturbed of
      # central lab)
      mass_cf = case_when(
        plot_code_simple == "WSL__53__Murgtal__NA" & code_layer == "M61" ~
          66.4,
        TRUE ~ mass_cf),
      vol_cf = case_when(
        plot_code_simple == "WSL__53__Murgtal__NA" & code_layer == "M61" ~
          NA_real_,
        TRUE ~ vol_cf),
      # Special case for "BGD-NP__1__Berchtesgaden National Park__F048" M01,
      # for which the bulk density back-up sample was accidentally sent
      # to the central lab too, and taken together with the 'real' undisturbed
      # sample representing four rings. As the number of rings in the back-up
      # sample is unknown, we will multiply the masses recorded in the central
      # lab by (494 - 11.86) / (494 - 11.86 + 172 - 11.86), with
      # 494: dry mass (g, including bags) of 'real' undisturbed sample
      # 172: dry mass (g, including bags) of back-up sample
      # 11.86: mass (g) of bags per sample
      mass_cf = case_when(
        plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F048" &
          code_layer == "M01" ~
          (494 - 11.86) / (494 - 11.86 + 172 - 11.86) * mass_cf,
        TRUE ~ mass_cf),
      mass_cf_for_vol = case_when(
        plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F048" &
          code_layer == "M01" ~
          (494 - 11.86) / (494 - 11.86 + 172 - 11.86) * mass_cf_for_vol,
        TRUE ~ mass_cf_for_vol),
      vol_cf = case_when(
        plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F048" &
          code_layer == "M01" ~
          (494 - 11.86) / (494 - 11.86 + 172 - 11.86) * vol_cf,
        TRUE ~ vol_cf),
      mass_before = case_when(
        plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F048" &
          code_layer == "M01" ~
          (494 - 11.86) / (494 - 11.86 + 172 - 11.86) * mass_before,
        TRUE ~ mass_before),
      mass_after = case_when(
        plot_code_simple == "BGD-NP__1__Berchtesgaden National Park__F048" &
          code_layer == "M01" ~
          (494 - 11.86) / (494 - 11.86 + 172 - 11.86) * mass_after,
        TRUE ~ mass_after)
      )

} # End of "if apply_corrections"




# Calculate grouped averages of particle densities for gap-filling

  # Optional TO DO: incorporate particle density grouped par parent material
  # class (lithology)

  # Global - option 1 (slope of linear regression through origin)
  pd_global <- 1E3 * coef(lm(mass_cf ~ vol_cf - 1, data = undist_harm))[1]

  # Global - option 2 (weighted mean of particle density)
  pd_global <-
    with(undist_harm,
         sum(particle_density * mass_cf, na.rm = TRUE) /
           sum(mass_cf[!is.na(particle_density)], na.rm = TRUE))

  # Gap-fill vol_cf

  undist_harm <- undist_harm %>%
    # Prepare data to use for gap-filling: weighted mean per plot
    group_by(plot_code_simple) %>%
    mutate(
      pd_plot = if (any(!is.na(particle_density))) {
        sum(particle_density * mass_cf, na.rm = TRUE) /
          sum(mass_cf[!is.na(particle_density)], na.rm = TRUE)
      } else {
        NA_real_
      }
    ) %>%
    ungroup() %>%
    mutate(
      particle_density_source = case_when(
        !is.na(particle_density) ~ "Observed",
        !is.na(pd_plot) ~ "Weighted mean for plot",
        TRUE ~ "Weighted mean (all observations)"
      ),
      particle_density = coalesce(particle_density,
                                  pd_plot,
                                  pd_global),
      vol_cf = ifelse(
        is.na(vol_cf) & !is.na(mass_cf),
        round(1E3 * mass_cf / particle_density),
        vol_cf)
    )








## 4.2. Calculate areal masses ----
#  Bulk density (of fine earth), vol% coarse fragments, areal mass forest floor

  source("./src/functions/uncertainty_functions.R")

  df_layers <- df_layers %>%
    rename(
      mass_before_dist = mass_before,
      mass_after_dist = mass_after) %>%
    left_join(
      undist_harm %>%
        select(plot_code_simple, code_layer,
               mass_before, mass_after,
               mass_cf, vol_cf, particle_density, particle_density_source,
               mass_roots, remarks_pretrt),
      by = join_by("plot_code_simple", "code_layer"))


  if (apply_corrections) {

    # Solve inconsistencies

    before_to_field_moist_plaus <- c(0.3, 1.015)
    after_to_before_plaus <- c(0.5, 1.01)

    df_layers <- df_layers %>%
      filter(wp == "WP2") %>%
      # Some obvious typos in WBL
      mutate(
        mass_before = case_when(
          # Originally 575.06 g
          plot_code_simple == "UNIUD__2__Val Alba__BF-NM" &
            code_layer == "M36" ~
            675.06,
          TRUE ~ mass_before
        )
      ) %>%
      # Remove inconsistent undisturbed sample masses
      mutate(
        before_to_field_moist = mass_before / bulk_den,
        after_to_before = mass_after / mass_before,
        # Is ratio of mass_before (central lab) to bulk_den (field-moist)
        # plausible?
        b2fm_flag = (wp == "WP2" &
                       grepl("^M", code_layer) &
                       !is.na(mass_before) &
                       # Higher moisture content in Histosols looks plausible
                       (!plot_code_simple %in% c(
                         "VUK__34__Rejviz__NA",
                         "WSL__25__Seeliwald__NA")) &
                       # Ignore Murgtal M61 because of sample splitting
                       !(plot_code_simple == "WSL__53__Murgtal__NA" &
                           code_layer == "M61") &
                       # Some exceptions:
                       # for which the bulk_den (field-moist) value from the
                       # partner was likely a mistake (based on masses
                       # measured in the central lab + total volume of rings)
                       # However, no corrections needed since mass_after
                       # seems plausible
                       !((plot_code_simple == "INBO__2__Wijnendalebos__NA" &
                            code_layer == "M13") |
                           (plot_code_simple ==
                              "BFNP__IP_10__IP_Hohensteingehaenge__NA" &
                              code_layer == "M36") |
                           (plot_code_simple ==
                              "BGD-NP__1__Berchtesgaden National Park__F048" &
                              code_layer == "M01")) &
                       (before_to_field_moist < before_to_field_moist_plaus[1] |
                          before_to_field_moist >
                          before_to_field_moist_plaus[2])),
        # Is ratio of mass_after to mass_before (central lab) plausible?
        # (Except for BFNP, UNIUD and INBO, for which mass_before was not
        #  (entirely) dry, this should be close to 1)
        a2b_flag = (wp == "WP2" &
                      grepl("^M", code_layer) &
                      !is.na(mass_before) &
                      (after_to_before < after_to_before_plaus[1] |
                         after_to_before > after_to_before_plaus[2])),
        # Scenario 1:
        # if mass_before is quite different from bulk_den (but plausible
        # in comparison with mass_after), assume that both mass_before and
        # mass_after are implausible (this is manually verified).
        # Action: replace mass_before and mass_after by NA
        # This is the case for
        #  "NWFVA__06-006__Niddahaenge__a" M36
        #  "BFNP__2122__Laerchenberg__NA" M13
        #  "VUK__36__Hadecka planinka__NA" M61
        across(
          c(mass_before, mass_after),
          ~ ifelse(b2fm_flag, NA_real_, .)
        ),
        # Scenario 2:
        # if mass_after is quite different from mass_before (but mass_before
        # is plausible in comparison with bulk_den), assume that
        # both mass_before and mass_after are implausible (this is manually
        # verified).
        # Action: replace mass_before and mass_after by NA
        # (replacement of mass_after is most important, but do the same with
        #  ass_before to avoid any confusion)
        # This is the case for
        #  "NPS__21__Spicnik__2" M13
        #  "NWFVA__03-026__Landwehr__NA" M13
        across(
          c(mass_before, mass_after),
          ~ ifelse(a2b_flag, NA_real_, .))
        # Scenario 3:
        # if mass_before and mass_after are both quite different from bulk_den
        # (b2fm_flag & !a2b_flag), but plausible in comparison with count_rings,
        # could be a mistake from the field mass (bulk_den)
        # No need to take any action.
        # This is the case for:
        # Wijnendale M13
        # IP_10 M36
        # BGD-NP F048 M01
      ) %>%
      select(-any_of(c(
        "before_to_field_moist", "after_to_before",
        "b2fm_flag", "a2b_flag"
      )))

  } # End of "if apply_corrections"






  df_layers <- df_layers %>%
    mutate(
      # Dry mass fraction (dry mass relative to field-moist mass)
      dry_to_moist = case_when(
        # Forest floor layers
        !is.na(dry_to_moist) ~ dry_to_moist,
        # Below-ground undisturbed - default
        !is.na(mass_after) ~ pmin(mass_after / bulk_den, 1),
        TRUE ~ NA_real_
      )) %>%
    # Gap-fill dry_to_moist for records with missing mass_after
    # by taking the average of the dry_to_moist of any layer with undisturbed
    # sample above or below it
    mutate(
      # Only keep mineral layer values
      d2m_mineral_down = ifelse(grepl("^M", code_layer) &
                                  bulk_den_check == "Sample collected",
                                dry_to_moist,
                                NA_real_),
      d2m_mineral_up = ifelse(grepl("^M", code_layer) &
                                bulk_den_check == "Sample collected",
                              dry_to_moist,
                              NA_real_)
    ) %>%
    group_by(plot_code_simple) %>%
    fill(d2m_mineral_down, .direction = "down") %>%
    fill(d2m_mineral_up, .direction = "up") %>%
    ungroup() %>%
    mutate(
      dry_to_moist = case_when(
        !is.na(dry_to_moist) ~ dry_to_moist,
        # Gap-fill dry_to_moist for undisturbed samples with missing mass_after
        grepl("^M", code_layer) &
          bulk_den_check == "Sample collected" &
          is.na(mass_after) ~
          rowMeans(cbind(d2m_mineral_down, d2m_mineral_up),
                   na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) %>%
    select(-any_of(c("d2m_mineral_down", "d2m_mineral_up"))) %>%
    mutate(
      dry_to_moist_central = case_when(
        # Forest floor layers
        !is.na(dry_to_moist_central) ~ dry_to_moist_central,
        # Below-ground undisturbed
        TRUE ~ pmin(mass_after / mass_before, 1)
      ),
      dry_to_moist_local = case_when(
        # Forest floor layers
        !is.na(dry_to_moist_local) ~ dry_to_moist_local,
        # Below-ground undisturbed
        TRUE ~ pmin(mass_before / bulk_den, 1)
      ),
      mass_after_fine_earth = case_when(
        plot_code_simple == "WSL__53__Murgtal__NA" & code_layer == "M61" ~
          mass_after,
        TRUE ~  mass_after - coalesce(mass_cf, 0) - coalesce(mass_roots, 0)
      ),
      # Corrected mass fine earth (dry, in g)
      # Correct for field-moist residu left in original recipient (ziploc bag)
      # from the field after transfering the undisturbed sample to the oven
      # recipient
      mass_after_fine_earth_corr = case_when(
        # When mass_after is NA (no need to correct for residu)
        !is.na(bulk_den) & bulk_den > 0 & is.na(mass_after) ~
          dry_to_moist * bulk_den - coalesce(mass_cf, 0) -
          coalesce(mass_roots, 0),
        # Default correction:
        TRUE ~
          mass_after_fine_earth *
          (bulk_den / (bulk_den - mass_recipient_residu))
      ),
      # Total volume of rings (1 mL = 1 cm3)
      vol_total = count_rings * vol_ring,
      # Volume occupied by fine earth (1 mL = 1 cm3)
      vol_fine_earth = vol_total - coalesce(vol_cf, 0) -
        (coalesce(mass_roots, 0) * 1E3 / density_roots),
      # "bulk density" (kg m-3) refers to the bulk density of fine earth,
      # defined as "the ratio of the oven-dry mass of the fine earth (< 2 mm) to
      # the volume of the fine earth and pore space of the soil"
      # (basically, the bulk density in stone-free areas)
      bulk_density = round(
        1E-3 * mass_after_fine_earth_corr / # convert from g to kg
          (vol_fine_earth * 1E-6)) # convert from cm3 to m3
    ) %>%
    rowwise() %>%
    mutate(bd_unc = list(add_bd_uncertainty(bulk_density))) %>%
    unnest_wider(bd_unc) %>%
    rename(mass_before_undist = mass_before,
           mass_after_undist = mass_after,
           bulk_density_undist = bulk_density,
           bulk_density_undist_min = bulk_density_min,
           bulk_density_undist_max = bulk_density_max) %>%
    mutate(
      mass_before = case_when(
        # Use most important masses only:
        # "dist" for OFH and "undist" for below-ground
        grepl("^M", code_layer) ~ mass_before_undist,
        TRUE ~ mass_before_dist
      ),
      mass_after = case_when(
        grepl("^M", code_layer) ~ mass_after_undist,
        TRUE ~ mass_after_dist
      ),
      bulk_density = coalesce(bulk_density_undist,
                              bulk_density_dist),
      bulk_density_min = coalesce(bulk_density_undist_min,
                                  bulk_density_dist_min),
      bulk_density_max = coalesce(bulk_density_undist_max,
                                  bulk_density_dist_max),
      # Calculate coarse_fragment_vol_ring
      # (1 mL = 1 cm3)
      coarse_fragment_vol_ring = case_when(
        # Murgtal M61, for which the undisturbed sample was split
        # to have a subsample for the analytical lab:
        # vol_cf corresponds with the volume of the entire ring
        plot_code_simple == "WSL__53__Murgtal__NA" & code_layer == "M61" ~
          1E2 * vol_cf / (1 * vol_ring),
        # vol % coarse fragments is known to be unreliable for:
        # - "NWFVA__03-060__Meinsberg__NA" M36 (as reported by partner)
        plot_code_simple == "NWFVA__03-060__Meinsberg__NA" &
          code_layer == "M36" ~ NA_real_,
        # vol % coarse fragments is known to be unreliable for:
        # - undisturbed samples taken using rings smaller than Kopecky (100 cm³)
        #   (i.e., NWFVA used small rings of 5 cm³ when too stony for Kopecky)
        vol_ring < 100 ~ NA_real_,
        # Default calculation
        TRUE ~ 1E2 * vol_cf / vol_total)
    ) %>%
    mutate(
      # Summarise coarse fragments
      # ---
      P1_coaf_2_50mm = P1_coaf_2mm - P1_coaf_50mm,
      coarse_fragment_2_50mm_vol = ifelse(
        grepl("^M", code_layer),
        rowMeans(cbind(coarse_fragment_vol_ring, P1_coaf_2_50mm), na.rm = TRUE),
        NA_real_
      ),
      coarse_fragment_vol = coarse_fragment_2_50mm_vol + P1_coaf_50mm
    ) # %>%
    # rowwise() %>%
    # # TO DO: update based on spatial CF data by VUK
    # mutate(cf_unc = list(add_cf_uncertainty(P1_coaf_2mm,
    #                                         P1_coaf_50mm,
    #                                         coarse_fragment_vol_ring))) %>%
    # unnest_wider(cf_unc) #%>%
  # TO DO: round CF








## 4.3. Calculate vol_perc_fine_earth per plot ----

df_plot_fe <- df_layers %>%
    filter(wp == "WP2") %>%
    mutate(
      thickness_bedrock = ifelse(
        # Below-ground layers above or containing lithic contact
        !is.na(depth_bottom_bedrock) & depth_top >= 0,
        depth_bottom_bedrock - depth_top,
        NA_real_),
      layer_vol_fine_earth = ifelse(
        !is.na(thickness_bedrock),
        thickness_bedrock * (1 - 1E-2 * coarse_fragment_vol),
        NA_real_)
    ) %>%
    group_by(plot_code_simple) %>%
    reframe(
      vol_fine_earth = ifelse(
        any(!is.na(layer_vol_fine_earth)),
        sum(layer_vol_fine_earth, na.rm = TRUE),
        0)
    ) %>%
    mutate(
      vol_perc_fine_earth = round(1E2 * (vol_fine_earth / 100))
    ) %>%
    select(-vol_fine_earth)


















# 5. Prepare output datasets ----

## 5.1. Compile inconsistencies ----

### 5.1.1. Check coordinates ----

if (create_inconsistency_report) {

  ### INCONSISTENCY 15 ----

  rule <- paste0("Reported coordinates (Survey123 app) should be within a ",
                 "reasonable distance from coordinates in HIS metadata file.")

  rule_id <- "15"


  # Records with a problem

  source("./src/functions/as_sf.R")

  his_sf <- his %>%
    rename_with(~ paste0(.x, "_dec"), c(latitude, longitude)) %>%
    # Filter out "extra" plots without HIS coordinates
    filter(!is.na(latitude_dec) & !is.na(longitude_dec)) %>%
    as_sf %>%
    rename_with(~ paste0(.x, "_his"), c(geometry,
                                        latitude_dec, longitude_dec)) %>%
    select(plot_code_simple, ends_with("_his"))

  df_plot_sf <- df_plot %>%
    filter(wp == "WP2") %>%
    as_sf %>%
    rename_with(~ paste0(.x, "_app"), c(geometry,
                                        latitude_dec, longitude_dec)) %>%
    select(plot_code_simple, ends_with("_app"))


  # Join the two dataframes on plot_code_simple
  df_coord <- his %>%
    select(plot_code_simple) %>%
    inner_join(
      his_sf,
      by = "plot_code_simple"
    ) %>%
    inner_join(
      df_plot_sf,
      by = "plot_code_simple"
    ) %>%
    rowwise() %>%
    mutate(
      distance_m =
        round(as.numeric(st_distance(geometry_his, geometry_app)))
    ) %>%
    ungroup()

  distance_plaus <- 20000 # m

  recs_problem <- df_coord %>%
    select(-contains("geometry")) %>%
    filter(distance_m > distance_plaus)

  inc <- recs_problem %>%
    mutate(
      inconsistency_reason = paste0(rule,
                                    " [current distance: ",
                                    distance_m, " m]")) %>%
    mutate(across(everything(), as.character)) %>%
    # pivot longer to get one row per parameter
    pivot_longer(
      cols = c("latitude_dec_his", "longitude_dec_his",
               "latitude_dec_app", "longitude_dec_app"),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    left_join(
      df_plot %>%
        select(-latitude_dec, -longitude_dec),
      by = "plot_code_simple") %>%
    mutate(
      source = case_when(
        grepl("_app$", parameter) ~ "Survey123 app",
        grepl("_his$", parameter) ~ "HIS metadata table"),
      parameter = case_when(
        grepl("lat", parameter) ~ "latitude",
        grepl("lon", parameter) ~ "longitude")) %>%
    left_join(attribute_catalogue %>%
                rename(parameter_description = descr,
                       parameter_unit = unit),
              by = join_by("parameter" == "column_name")) %>%
    mutate(
      parameter_description =
        paste0(parameter_description, " (according to ", source, ")"),
      parameter_unit = "g") %>%
    relocate(parameter_description, parameter_unit, .after = parameter) %>%
    mutate(
      # mark unique_inconsistency = TRUE only for the first parameter per record
      unique_inconsistency =
        (parameter == "latitude" & source == "HIS metadata table"),
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






### 5.1.2. Compile all inconsistency reports ----

if (create_inconsistency_report) {

reports <- list(
  # ---
  # Function: get_app_data()
  # ---
  # INCONSISTENCY 1
  # "Data expected to be numeric should be numeric."
  inconsistencies_app_numeric = res_get_app_data$inconsistencies,
  # ---
  # Function: get_data_local_lab()
  # ---
  # INCONSISTENCY 1
  # "Data expected to be numeric should be numeric."  #
  inconsistencies_local_lab_numeric = res_get_data_local_lab$inconsistencies,
  # ---
  # Function: app_data_long()
  # ---
  # INCONSISTENCY 2
  # "Forest floor mass should be plausible in comparison with thickness
  #  and surface area frame"
  # app_data_long()
  # INCONSISTENCY 3
  # "The total number of reported undisturbed samples from the central pit (P1)
  #  and additional sampling points should not exceed 5 (unlikely). Please
  #  check how many undisturbed samples you took at the given depth."
  # INCONSISTENCY 4
  # "Identical bedrock depth values are reported across sampling points (P2–9).
  #  This suggests the values may have been copied from P1 rather than
  #  independently observed. Please confirm how to interpret this.
  # INCONSISTENCY 5
  # "Bedrock depth is missing (NA) in some sampling points (P1-5), while others
  #  report shallow bedrock. Please confirm that all missing values are
  #  correctly marked as NA (i.e., bedrock below 100 cm)"
  # INCONSISTENCY 6
  # "Volumetric percentage of coarse fragments > 50 mm should not exceed that
  #  > 2 mm. You may have only indicated coarse fragments between 2–50 mm
  #  under > 2 mm."
  # INCONSISTENCY 7
  # "Undisturbed mass should be plausible in comparison with
  #  number and volume of rings"
  # app_data_long()
  # INCONSISTENCY 8
  # "Subsample(s) forest floor for lab cannot weigh more than sum of
  #  all reported forest floor masses across sampling points"
  # INCONSISTENCY 9
  # "For depths below the deepest reported bedrock depth no samples are
  #  expected to be taken"
  # INCONSISTENCY 10
  # "Samples weighing more than 0 g are expected to show 'sample collected'"
  # INCONSISTENCY 11
  # "Reported slope is high and may have been accidentally reported as a
  #  percentage instead of decimal degrees. Please confirm the unit."
  # INCONSISTENCY 12
  # "P1 reports 100 vol% coarse fragments for this layer, while (disturbed)
  #  samples were collected at other points in the same layer, indicating that
  #  fine earth was present. This may reflect spatial variation. The plot-
  #  representative coarse fragment content will be capped at minimum 80 vol%
  #  so that this layer contributes to the carbon stock."
  inconsistencies_app_long = res_app_data_long$inconsistencies,
  # ---
  # Script: preprocessing_and_quality_check.R
  # ---
  # INCONSISTENCY 13
  # "Mass of subsample after drying in local lab is higher than mass of
  #  field-moist subsample (after mixing)."
  # INCONSISTENCY 14
  # "Mass recorded after drying in local lab must roughly correspond with
  #  mass of OFH sample received by central lab"
  # INCONSISTENCY 15
  # "Reported coordinates (Survey123 app) should be within a
  #  reasonable distance from coordinates in HIS metadata file."
  inconsistencies_preprocessing = inconsistencies
) %>%
  # Remove NULL (for functions for which no report is created)
  purrr::compact()




# Bind them together

inc_report <- bind_rows(reports) %>%
  as_tibble %>%
  # Remove WP3 inconsistencies
  left_join(df_plot %>%
              select(globalid, wp),
            by = "globalid") %>%
  filter(wp == "WP2") %>%
  select(-wp) %>%
  rename(plot_code = plot_code_app) %>%
  mutate(
    # Institute that does the soil sampling
    team = case_when(
      team %in% c("BFNP_EXTRA", "NPS") ~ "BFNP",
      team == "LWF" &
        res_id_inst == "x" ~ "BFNP",
      team == "DISAFA - UNITO" ~ "UNITO",
      team == "FVA-BW" ~ "NWFVA",
      team == "INCDS" ~ "UNITBV",
      team %in% c("UNIUD/HSI", "UNIUD/HSI_EXTRA",
                  "UNIUD_EXTRA") ~ "UNIUD",
      team %in% c("URK", "WULS") ~ "IBL",
      team %in% c("HUN-REN Centre for Ecological Research/VUKOZ(VUK)",
                  "VUKOZ(VUK)") ~
        "VUK",
      TRUE ~ team)) %>%
  mutate(
    partner_feedback = NA_character_,
    partner_corrected_value = NA_character_,
    partner_remark = NA_character_) %>%
  arrange(team)

# Remove inconsistency with unexpected deeper samples in code DE-BGD-NP-1-F059-1
# (based on e-mail from partner)

# You can link the inconsistencies with the app records using
# globalid (without curly brackets and lowercase)

} # End of "if create_inconsistency_report"





### 5.1.3. Export ----

if (create_inconsistency_report) {

dir <- paste0("./output/partner_inconsistencies/",
              as.character(format(Sys.Date(), format = "%Y%m%d")), "/")

if (!dir.exists(dir)) {
  dir.create(dir)
}


write.table(inc_report,
            file = paste0(dir,
                          "partner_inconsistency_report_all.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

# Per partner

partners_inc <- unique(inc_report$team)

for (i in seq_along(partners_inc)) {

  inc_report_i <- inc_report %>%
    filter(team == partners_inc[i])

  wb <- createWorkbook()
  addWorksheet(wb, "Inconsistency report")
  addWorksheet(wb, "Attribute catalogue")
  # Ideally, there would have to be a description of all attributes (columns)

  writeData(wb, 1, inc_report_i)
  addFilter(wb, 1, rows = 1, cols = 1:ncol(inc_report_i))
  dataValidation(wb, 1,
                 cols = which(names(inc_report_i) == "partner_feedback"),
                 rows = seq(2, nrow(inc_report_i) + 1),
                 type = "list",
                 value = '"1 - The reported value is correct, 2 - The correct value is added"')

  freezePane(wb, 1, firstActiveRow = 2, firstActiveCol = 1)
  # writeData(wb, 2, xxx) # This should be some table with description of
  # the columns in inc_report

  openxlsx::saveWorkbook(wb,
                         file =
                           paste0(dir,
                                  tolower(gsub("[- ]", "", partners_inc[i])),
                                  "_partner_inconsistency_report.xlsx"),
                         overwrite = TRUE)

} # End of "for i in partners"

} # End of "if create_inconsistency_report"










## 5.2. Compile and tidy pre-processed soil physicochemical data ----

# Remove unnecessary columns + Add lab data

# TO DO: in some cases, it will be necessary to LOCF (or spline?) the analyses
# from the layer above, when no samples were sent to the lab for the lowest
# layer (e.g., M36 of BGD-NP__1__Berchtesgaden National Park__F048)
# Splines (?) for layer with absent disturbed and undisturbed samples,
#  e.g., "DISAFA - UNITO__14__Alpe Cusogna__NA" M61

# TO DO (WBL dist + results RF analytical lab):
# "NWFVA__06-025__Kinzigaue__b": four disturbed below-ground samples contained
# no indication of depth. During lab pretreatment, they were randomly allocated
# to a certain depth, but this needs to be corrected - probably best assuming
# a decreasing SOC with increasing depth...
# Also for 03-083 based on data?

# Note:
# LWF__65_a__Friedergries__NA OFH_carbon has been retaken during the second
# sampling campaign by LWF, as the first sample contained a lot of roots
# (which were removed before lab analysis)
# That original sample is named "DE__LWF__65_a__NA__NA__OFH_carbon" in the
# RF (V-25V006-04), while the new (better) sample is named
# "DE__LWF__65_a__NA__NA__OFH_carbon_b" → better to proceed with these results

# VUK__1__Zofin__b OFH and VUK__1__Zofin__c OFH are switched at central lab
# INBO! (note: forest floor masses already adjusted in section 3.2)

# In the end, you can make the dataframe(s) a bit more neat. You could
# make one dataframe with metadata per plot (e.g., WRB, slope, weather...)
# (like df_plot at this point) and one with data per depth layer.
# Several columns (e.g. those that were used to double-check other values
# are not needed anymore). Basically, you need columns to identify the
# plot and layer, the final calculated values and their uncertainties.
# (similar to "layer 1" of ICP F)

# Do we report those original masses (in addition to relative percentages)?
# select(-mass_dry_leaves, -mass_dry_twigs_medium, -mass_dry_twigs_small)

# TO DO:
# Bulk density using pedotransfer for plots for which no undisturbed samples
# could be taken, e.g., "BGD-NP__1__Berchtesgaden National Park__F016"

# TO DO: add TOC of HF layer Seeliwald








### 5.2.1. Compile df_layers ----

  # Arange and add lab data

  df_layers <- df_layers %>%
    # Filter for WP2
    filter(wp == "WP2") %>%
    # Create columns if missing
    mutate(
      coarse_fragment_vol_min =
        if (!"coarse_fragment_vol_min" %in% names(.)) NA_real_
      else coarse_fragment_vol_min,
      coarse_fragment_vol_max =
        if (!"coarse_fragment_vol_max" %in% names(.)) NA_real_
      else coarse_fragment_vol_max,
    ) %>%
    rename(coarse_fragment_vol_p1 = P1_coaf_2mm,
           coarse_fragment_vol_p1_50mm = P1_coaf_50mm) %>%
    # Add composed_site_id
    left_join(
      df_plot %>%
        select(plot_code_simple, composed_site_id),
      by = "plot_code_simple") %>%
    # Add lab data
    left_join(
      data_lab %>%
        select(-any_of(c("institute_sampling", "sample_code", "sample_id"))),
      by = join_by("plot_code_simple", "code_layer", "sample_id_simple")
    )


  cols_layers <- c(
  )

  # Which of those columns do not exist?
  assertthat::assert_that(
    identical(
      cols_layers[which(!cols_layers %in% names(df_layers))],
      character(0)))

  # Ordered vector
  cols_layers_ordered <- unlist(lapply(cols_layers, function(x) {
    c(
      x,
      paste0(x, "_min"),
      paste0(x, "_max"))
  }))

  # Keep only columns that actually exist in df_layers
  cols_layers_ordered <- cols_layers_ordered[
    cols_layers_ordered %in% names(df_layers)
  ]

  # Select columns in desired order
  wp2_layer_data <- df_layers[, cols_layers_ordered]





### 5.2.2. Compile df_plot ----

  # Arange data

  df_plot <- df_plot %>%
    # Filter for WP2
    filter(wp == "WP2") %>%
    # Create columns if missing
    mutate(
      parent_material =
        if (!"parent_material" %in% names(.)) NA_real_
      else parent_material
    ) %>%
    left_join(
      his %>%
        rename(
          eftc = his_eftc,
          eft = eea_fortype,
          tsa_derived = derived_tsa,
          tsa_class = his_tsaclass) %>%
        select(plot_code_simple, res_id_inst, reserve_name,
               sub_id, plot_id, eftc, eft, his_soil_water, his_soil_nutrient,
               tsa_derived, tsa_class, year_reserve, year_abandonment),
      by = "plot_code_simple") %>%
    # Add vol_perc_fine_earth
    left_join(
      df_plot_fe,
      by = "plot_code_simple") %>%
    # Add particle density
    left_join(
      undist_harm %>%
        group_by(plot_code_simple) %>%
        reframe(
          particle_density = ifelse(
            any(!is.na(pd_plot)),
            round(mean(pd_plot, na.rm = TRUE)),
            NA_real_)),
      by = "plot_code_simple")


cols_plot <- c(
)


# Which of those columns do not exist?
assertthat::assert_that(
  identical(
    cols_plot[which(!cols_plot %in% names(df_plot))],
    character(0)))

# Ordered vector
cols_plot_ordered <- unlist(lapply(cols_plot, function(x) {
  c(
    x,
    paste0(x, "_min"),
    paste0(x, "_max"))
}))

# Keep only columns that actually exist in df_layers
cols_plot_ordered <- cols_plot_ordered[
  cols_plot_ordered %in% names(df_plot)
]

# Select columns in desired order
wp2_plot_data <- df_plot[, cols_plot_ordered]






### 5.2.3. Milestone 12 ----

# - Remove gap-filled data
#   (bulk density based on pedotransfer, TOC based on HF mean for Seeliwald)
# - Remove EXTRA sites
# - Remove "min" and "max" columns?

# layer data

path_m12 <- "./output/project_requests/m12/"

write.table(wp2_layer_data %>%
              filter(!grepl("EXTRA", plot_code_simple)),
            file = paste0(path_m12, "wp2_layer_data.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

source("./src/functions/create_attribute_catalogue.R")
create_attribute_catalogue(wp2_layer_data,
                           path_to_save = paste0(path_m12, "wp2_layer_data_"))

# plot data

write.table(wp2_plot_data %>%
              filter(!grepl("EXTRA", plot_code_simple)),
            file = paste0(path_m12, "wp2_plot_data.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

source("./src/functions/create_attribute_catalogue.R")
create_attribute_catalogue(wp2_plot_data,
                           path_to_save = paste0(path_m12, "wp2_plot_data_"))




#
# write.table(wp3_sites,
#             file = "./data/wp3_sites.csv",
#             row.names = FALSE,
#             na = "",
#             sep = ";",
#             dec = ".")


## 5.3. Compile data flammability analysis ----



## 5.4. Compile metadata for eDNA ----

  # TO DO: metadata (weather) for eDNA are different for three NW-FVA plots
  # (resampling due to loss eDNA samples)
