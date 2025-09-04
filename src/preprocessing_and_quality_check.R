
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
          require("patchwork"))

# Source URLs of sensitive Google Drive links
source("./data/sensitive_metadata/google_drive_links.R")




# 2. Get data ----

# In theory, it should be possible to left_join everything using either
# plot_code_simple (for plot-specific data) or plot_code_simple and code_layer
# (for layer-specific data)

## · Site metadata ----

if (!exists("his", envir = globalenv())) {
  source("./src/functions/get_his.R")
  his <- get_his()
}

if (!exists("wp3_sites", envir = globalenv())) {
  source("./src/functions/get_wp3_sites.R")
  wp3_sites <- get_wp3_sites()
}


# write.table(his,
#             file = "./data/his.csv",
#             row.names = FALSE,
#             na = "",
#             sep = ";",
#             dec = ".")
#
# write.table(wp3_sites,
#             file = "./data/wp3_sites.csv",
#             row.names = FALSE,
#             na = "",
#             sep = ";",
#             dec = ".")




## · Data Survey123 app ----

# Data Survey123 app wide format

source("./src/functions/get_app_data.R")
app_data_wide <- get_app_data()

# Convert data Survey123 app to long format

# This script also generates an inconsistency report with several
# inconsistencies

source("./src/functions/app_data_long.R")
df_layers <- app_data_long(app_data_wide)

# df_plot and inconsistencies will be in the Global Environment







## · Data from local lab ----
# (spreadsheet named “forest_floor_data_[institute]”)

source("./src/functions/get_data_local_lab.R")

df_local_lab <- get_data_local_lab()




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

data_lab <- data_lab %>%
  # Add sample_id_sample, plot_code_simple, institute_sampling,
  # sample_code, code_layer
  # At the moment it's not possible to test this using 2025 lab data, but
  # it should work out (since sample_id should be the same like in the WBLs)
  add_ids_simple()









## · Sample lists ----

source("./src/functions/get_sample_lists.R")
samples <- get_sample_lists()







## · eDNA analyses ----

# Probably not needed in this script, but just to show how to add the columns
# sample_id_simple etc based on the sample_ids (unharmonised) in
# the 'diepvries' list:

# Apply it as follows:
source("./src/functions/add_ids_simple.R")
# [name dataframe] %>%
#       add_ids_simple(orig_harm = FALSE)








# 3. Combine masses for disturbed samples (forest floor) ----

code_layer_levels <- c("OL", "OFH", "M01", "M13", "M36", "M61")


## 3.1. Data pretreatment disturbed ----

glimpse(dist)

columns_to_check <- c("mass_recipient",
                      "mass_before",
                      "mass_after")

source("./src/functions/safe_numeric_convert.R")
source("./src/functions/add_ids_simple.R")

dist_harm <- dist %>%
  rename(sample_id = `sample_id (unieke veldcode uit Survey123 app)`,
         survey_date = `Datum bemonstering`,
         mass_recipient = `Massa (g) recipiënt zonder deksel`,
         mass_before =
           `Massa (g) monster voor (inclusief recipiënt zonder deksel)`,
         mass_after =
           `Massa monster (g) na (inclusief recipiënt zonder deksel)`) %>%
  # Remove records from 2024 test
  filter(!grepl("^WILDCARD2024_", sample_id)) %>%
  select(sample_id, mass_recipient, mass_before, mass_after) %>%
  # Add sample_id_sample, plot_code_simple, institute_sampling,
  # sample_code, code_layer
  add_ids_simple() %>%
  mutate(across(all_of(columns_to_check),
                ~ safe_numeric_convert(.x, cur_column())))

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
      mutate(mass_after = mass_after_net,
             mass_before = mass_before_net) %>%
      select(plot_code_simple,
             code_layer, mass_before, mass_after) %>%
      mutate(
        mass_dry_leaves = NA_real_,
        mass_dry_twigs_medium = NA_real_,
        mass_dry_twigs_small = NA_real_),
    # 2. From INBO
    dist_harm %>%
      # Part 1 INBO: Create OL subtotals
      filter(code_layer == "OL") %>%
      select(plot_code_simple, sample_code, mass_after_net) %>%
      pivot_wider(
        names_from = sample_code,
        values_from = mass_after_net,
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
              any(!is.na(mass_before_net)),
              sum(mass_before_net, na.rm = TRUE),
              NA_real_)),
        by = "plot_code_simple") %>%
      select(plot_code_simple, code_layer, mass_before, mass_after,
             mass_dry_leaves, mass_dry_twigs_medium,
             mass_dry_twigs_small) %>%
      # Part 2 INBO: Bind with non-OL rows
      bind_rows(
        dist_harm %>%
          filter(institute_sampling == "INBO") %>%
          filter(code_layer != "OL") %>%
          filter(!grepl("BackUp", sample_id)) %>%
          mutate(mass_after = mass_after_net,
                 mass_before = mass_before_net) %>%
          select(plot_code_simple,
                 code_layer, mass_before, mass_after) %>%
          mutate(
            mass_dry_leaves = NA_real_,
            mass_dry_twigs_medium = NA_real_,
            mass_dry_twigs_small = NA_real_))
    ) %>%
  full_join(
    # Part 1 other partners: forest floor data local lab
    df_local_lab %>%
      mutate(
        mass_dry_partner =
          rowSums(across(starts_with("mass_dry_")), na.rm = TRUE)) %>%
      select(plot_code_simple, code_layer, mass_dry_partner,
             mass_dry_leaves_ol, mass_dry_twigs_medium_ol,
             mass_dry_twigs_small_ol),
    by = join_by("plot_code_simple", "code_layer")
  ) %>%
  mutate(code_layer = factor(code_layer,
                             levels = code_layer_levels)) %>%
  # Arrange by plot and layer
  arrange(plot_code_simple, code_layer) %>%
  relocate(mass_dry_partner, .before = mass_before) %>%
  mutate(
    mass_dry_leaves = coalesce(mass_dry_leaves,
                               mass_dry_leaves_ol),
    mass_dry_twigs_medium = coalesce(mass_dry_twigs_medium,
                                     mass_dry_twigs_medium_ol),
    mass_dry_twigs_small = coalesce(mass_dry_twigs_small,
                                    mass_dry_twigs_small_ol)) %>%
  # Calculate percentage leaves, twigs medium, twigs small for OL
  mutate(
    perc_dry_leaves = round(1E2 * mass_dry_leaves / mass_dry_partner, 1),
    perc_dry_twigs_medium =
      round(1E2 * mass_dry_twigs_medium / mass_dry_partner, 1),
    perc_dry_twigs_small =
      round(1E2 * mass_dry_twigs_small / mass_dry_partner, 1)
    ) %>%
  select(-mass_dry_leaves_ol, -mass_dry_twigs_medium_ol,
         -mass_dry_twigs_small_ol) %>%
  # We actually don't need those masses also. Maybe better to report the
  # relative percentages.
  select(-mass_dry_leaves, -mass_dry_twigs_medium, -mass_dry_twigs_small)


View(dist_masses)

# Combine with df_layers (layer-specific data Survey123 app)

df_layers <- df_layers %>%
  left_join(dist_masses,
            by = join_by("plot_code_simple", "code_layer")) %>%
  relocate(mass_dry_partner, mass_before, mass_after,
           .after = dist)

# Check if you do not see any weird things in the masses of the forest floor
# (especially OFH):

df_layers %>%
  select(wp, institute_sampling,
         plot_code_simple, code_layer, thickness, tamass, dist,
         mass_dry_partner, mass_before, mass_after, tamass_compiled) %>%
  filter(code_layer %in% c("OL", "OFH")) %>%
  View


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
#   mass_dry_partner and dist
# - Calculate dry areal mass based on tamass, surface_frame, count_ff_frames
#   and the moisture content

# However, there seem to be a lot of exceptions (typically different from
# partner to partner):
# - In general, it seems so far that most of the samples are not yet dry
#   when they arrive at the central lab. This was visible for the UNIUD
#   samples upon arrival. On the other hand, it seems unexpected that the
#   mass at the central lab would still decrease by more than 20-30 %,
#   so maybe something went wrong with mass_after for some OFH samples
#   → maybe reweigh those... If mass_before is quite in line with
#   mass_dry_partner, mass_before and mass_dry_partner are probably fine.
# - BFNP: they only provided local lab masses for OL, not OFH. Maybe ask
#   them if they have OFH masses after drying, and for how long they dried
#   the samples, because some lose a lot of weight in the central lab
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
# - INBO: obviously no "mass_dry_partner". "mass_before" should therefore
#   be similar to "dist". There must be some typos (either app or WBL),
#   e.g. Bos Terrijst.


df_layers <- df_layers %>%
  mutate(
    # Dry mass fraction (dry mass relative to field-moist mass)
    # This will be the easiest to work with
    dry_to_moist = ifelse(
      code_layer %in% c("OL", "OFH"),
      case_when(
        institute_sampling == "INBO" ~ mass_after / dist,
        institute_sampling == "UNIUD" & code_layer == "OL" ~
          # We have to trust the values they gave, although they probably
          # did not dry it sufficiently long
          mass_dry_partner / tamass,
        institute_sampling == "UNIUD" & code_layer == "OFH" ~
          # combination of mass_dry_partner versus tamass and
          # mass_after versus mass_before
          (mass_after / dist) * (mass_dry_partner / tamass),
        institute_sampling == "BFNP" & code_layer == "OL" ~
          mass_dry_partner / dist,
        institute_sampling == "BFNP" & code_layer == "OFH" ~
          # No local lab mass reported
          # In theory, we should work with mass_before here, but mass_after
          # is probably better since probably not totally dry upon arrival
          # based on the data
          mass_after / dist,
        # ADD OTHER EXCEPTIONS HERE
        #
        # Partners with the "normal" sample flow
        code_layer == "OL" ~
          mass_dry_partner / dist,
        code_layer == "OFH" ~
          # In theory, we should work with mass_dry_partner too. However,
          # if samples lose a lot of weight in the central lab, they were
          # probably not entirely dry. On the other hand, this "extra drying"
          # did not happen in OL.
          mass_after / dist),
      NA_real_),
    # t ha-1
    areal_mass = ifelse(
      code_layer %in% c("OL", "OFH"),
      tamass * 1E-6 * # tonnes field-moist material over x sampling frame areas
        dry_to_moist / # g dry material / g field-moist material
        (surface_frame * count_ff_frames * 1E-4), # ha covered by all frames
      NA_real_),
    # We can also calculate the minimum and maximum, based on the variation
    # between sampling points
    areal_mass_min = ifelse(
      code_layer %in% c("OL", "OFH"),
      tamass_min * 1E-6 * # This is for one sampling point (one frame)
        dry_to_moist /
        (surface_frame * 1E-4), # ha covered by one frames
      NA_real_),
    areal_mass_max = ifelse(
      code_layer %in% c("OL", "OFH"),
      tamass_max * 1E-6 * # This is for one sampling point (one frame)
        dry_to_moist /
        (surface_frame * 1E-4), # ha covered by one frames
      NA_real_))


# Check again

df_layers %>%
  select(wp, institute_sampling,
         plot_code_simple, code_layer, thickness, tamass, dist,
         mass_dry_partner, mass_before, mass_after, tamass_compiled,
         dry_to_moist, areal_mass, areal_mass_min, areal_mass_max) %>%
  filter(code_layer %in% c("OL", "OFH")) %>%
  View


# 4. Combine undisturbed info for bulk density and coarse fragments ----

## 4.1. Data pretreatment undisturbed ----

glimpse(undist)

columns_to_check <- c("mass_recipient",
                      "mass_before",
                      "mass_after",
                      "mass_cf",
                      "mass_roots")

source("./src/functions/safe_numeric_convert.R")
source("./src/functions/add_ids_simple.R")

undist_harm <- undist %>%
  rename(
    sample_id = `sample_id (unieke veldcode uit Survey123 app)`,
    survey_date = `Datum bemonstering`,
    mass_recipient = `Massa (g) recipiënt zonder deksel`,
    mass_after =
      `Massa fine earth (g) na (inclusief recipiënt zonder deksel)`,
    mass_cf = `Massa steentjes (> 2 mm) (g) droog zonder recipiënt`,
    mass_roots =
      `Massa wortels (droog; indien aanzienlijk) (g) zonder recipiënt`) %>%
  # Using rename_with because of too long column name
  rename_with(~"mass_before", all_of(paste0("Massa (g) monster voor ",
                                            "(inclusief recipiënt zonder ",
                                            "deksel, steentjes en evt. ",
                                            "label)"))) %>%
  # Remove records from 2024 test
  filter(!grepl("^WILDCARD2024_", sample_id)) %>%
  select(sample_id, survey_date, mass_recipient, mass_before, mass_after,
         mass_cf, mass_roots) %>%
  # Add sample_id_sample, plot_code_simple, institute_sampling,
  # sample_code, code_layer
  add_ids_simple() %>%
  # mutate(
  #   # Correct small typo
  #   mass_before = case_when(
  #     mass_before == "349/90" ~ "349.90",
  #     TRUE ~ mass_before)) %>%
  mutate(across(all_of(columns_to_check),
                ~ safe_numeric_convert(.x, cur_column())))

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




## 4.2. Calculate areal masses ----
#  Bulk density (of fine earth), vol% coarse fragments, areal mass forest floor

df_layers_undist <- undist_harm %>%
  select(-mass_recipient, -mass_before, -mass_after) %>%
  rename(mass_before = mass_before_net) %>%
  rename(mass_after = mass_after_net) %>%
  left_join(df_layers %>%
              select(plot_code_simple, code_layer,
                     vol_ring, count_rings, bulk_den,
                     P1_coaf_2mm, P1_coaf_50mm,
                     contains("depth_bedrock"),
                     wp) %>%
              rename(mass_undist_moist = bulk_den),
            by = join_by("plot_code_simple", "code_layer")) %>%
  relocate(mass_undist_moist, .before = "mass_before") %>%
  mutate(code_layer = factor(code_layer,
                             levels = code_layer_levels)) %>%
  # Arrange by plot and layer
  arrange(institute_sampling, plot_code_simple, code_layer)

View(df_layers_undist)



source("./src/functions/uncertainty_functions.R")

df_layers_undist <- df_layers_undist %>%
  rename(coaf_2mm = P1_coaf_2mm) %>%
  rename(coaf_50mm = P1_coaf_50mm) %>%
  mutate(
    # Dry mass fraction (dry mass relative to field-moist mass)
    dry_to_moist = mass_after / mass_before,
    # Gap-fill mass_cf and mass_roots (net) with 0
    mass_cf = coalesce(mass_cf,
                       0),
    mass_roots = coalesce(mass_roots,
                          0),
    # "bulk density" (kg m-3) refers to the bulk density of fine earth,
    # defined as "the ratio of the oven-dry mass of the fine earth (< 2 mm) to
    # the volume of the fine earth and pore space of the soil"
    # (basically, the bulk density in stone-free areas)
    bulk_density =
      round(1E-3 * mass_after / # convert from g to kg
              (count_rings * vol_ring * 1E-6 - # convert from cm3 to m3
                 (1E-3 * mass_cf / 2650) - # Volume in ring occupied
                 # by stones (< 50 mm) in m3
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
    cf_mass_ring = 1E2 * mass_cf / (mass_after + mass_cf),
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
    # Remark: quite some coaf_2mm are < coaf_50mm really possible. Therefore,
    # assume that coaf_2mm are the coarse fragments in that case, and correct
    # We need this for the uncertainty estimation.
    coaf_2mm = ifelse(
      coaf_2mm < coaf_50mm,
      min(coaf_2mm + coaf_50mm, 100),
      coaf_2mm),
    coarse_fragment_vol = coarse_fragment_vol_ring + coaf_50mm
  ) %>%
  relocate(coaf_2mm, coaf_50mm, .before = coarse_fragment_vol_ring) %>%
  rowwise() %>%
  mutate(cf_unc = list(add_cf_uncertainty(coaf_2mm,
                                          coaf_50mm,
                                          coarse_fragment_vol_ring))) %>%
  unnest_wider(cf_unc)

# Question for Stijn: does mass_after for undisturbed samples include stones?




# 5. Compile and tidy pre-processed data ----

df_layers <- df_layers %>%
  # We renamed those
  select(-any_of(c("P1_coaf_2mm", "P1_coaf_50mm", "bulk_den"))) %>%
  left_join(
    df_layers_undist %>%
      rename(
        mass_before_undist = mass_before,
        mass_after_undist = mass_after,
        dry_to_moist_undist = dry_to_moist) %>%
      # Some variables are still in df_layers
      select(-any_of(c("institute_sampling", "count_rings", "vol_ring",
                       "depth_bedrock", "depth_bedrock_min",
                       "depth_bedrock_max", "wp"))),
    by = join_by("plot_code_simple", "code_layer"))

glimpse(df_layers)

# Also add lab data

# In the end, you can make the dataframe(s) a bit more neat. You could
# make one dataframe with metadata per plot (e.g., WRB, slope, weather...)
# (like df_plot at this point) and one with data per depth layer.
# Several columns (e.g. those that were used to double-check other values
# are not needed anymore). Basically, you need columns to identify the
# plot and layer, the final calculated values and their uncertainties.
# (similar to "layer 1" of ICP F)









# 6. Compile inconsistencies ----

## 6.1. Compile ----

# So far, these are the implemented inconsistency checks, together with
# the name of the inconsistency report in the Global Environment and the name
# of the function in which they are generated:

inc_reports <- c(
  # INCONSISTENCY 1
  # "Data expected to be numeric should be numeric."
  # get_app_data()
  "inconsistencies_app_numeric",
  "inconsistencies_local_lab_numeric",
  # INCONSISTENCY 2
  # "Forest floor mass should be plausible in comparison with thickness
  #  and surface area frame"
  # app_data_long()
  # INCONSISTENCY 3
  # "The total number of reported undisturbed samples from the central pit (P1)
  #  and additional sampling points should not exceed 5 (unlikely). Please
  #  check how many undisturbed samples you took at the given depth."
  # INCONSISTENCY 4
  # "The total number of reported undisturbed samples from the central pit (P1)
  #  and additional sampling points should not be 1 (unlikely). Please check
  #  how many undisturbed samples you took at the given depth."
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
  # "Subsample forest floor for lab cannot weigh more than sum of
  #  all reported forest floor masses across sampling points"
  # INCONSISTENCY 9
  # "For depths below the deepest reported bedrock depth no samples are
  #  expected to be taken"
  # INCONSISTENCY 10
  # "Samples weighing more than 0 g are expected to show 'sample collected'"
  #
  # app_data_long()
  "inconsistencies_app_long"
)


# Possible further inconsistency checks (can also just be visual)

# - Check if OL and OFH mass local lab is lower than "dist" (mass subsample
#   field)
# - Check if OFH mass local lab roughly corresponds with mass_before central lab
# - Check if OFH mass_after does not deviate too much from mass_before anymore
#   for non-INBO partners
# - Check if moisture contents of OL and OFH are plausible
# - Check if lab masses bulk density samples are lower than field-moist bulk
#   density masses
# - Check if trends in lab masses of bulk density samples (after drying)
#   correspond roughly with trends in masses from the field (i.e. similar
#   moisture content)
# - Check if lab masses of bulk density samples (before drying)
#   correspond roughly with masses from the field
# - Check if coarse fragments reported in 2-50 mm class in the field
#   correspond more or less with those from the central lab
# - Check if the undisturbed moisture contents are more or less making sense
# - vol_ring and surface_frame should be > 0
# - Compare sample list with app
# - check distance from HIS metadata table coordinates
# - coordinates should not be 0



# Compile all inconsistency reports from the Global Environment

# Check if all reports exist in the Global environment
# (should be if you ran all above-mentioned functions)
missing_reports <- inc_reports[!sapply(inc_reports, exists, envir = .GlobalEnv)]
assertthat::assert_that(identical(missing_reports, character(0)))


# Bind them together
inc_report <- bind_rows(lapply(inc_reports, get, envir = .GlobalEnv)) %>%
  as_tibble %>%
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



## 6.2. Export ----

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
  addFilter(wb, 1, row = 1, cols = 1:ncol(inc_report_i))
  dataValidation(wb, 1,
                 col = which(names(inc_report_i) == "partner_feedback"),
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






