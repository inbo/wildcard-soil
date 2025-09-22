
# Generate sample lists for WBL, RF and freezer
# ---------------------------------------------

# This R script helps to automatically create sample lists for their
# registration for analysis in the INBO central lab.

# · Freezer: shows all samples in the freezer (for eDNA analysis, for further
#            transfer to ETH Zürich, etc)
# · RF: registration for (physico)chemical analysis at the INBO central lab
#       (linked with LIMS lab management system and data warehouse)
#       These RFs also contain information that will be used to label
#       the recipients in the INBO soil archive
# · WBL disturbed: registration for sample pretreatment (oven-drying, etc)
#                  of the disturbed samples. Belgian records include
#                  OL-leaves, OL-twigs-medium, OL-twigs-small
# · WBL undisturbed: registration for sample processing (oven-drying and
#                    recording masses of dry fine earth and coarse fragments
#                    that used to be in the original undisturbed rings)





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
          require("parsedate"))

# Source URLs of sensitive Google Drive links
source("./data/sensitive_metadata/google_drive_links.R")

# Date of last sample registration
# (samples that arrived after this will be filtered for)
date_last_reg <- parsedate::parse_date("2025-09-16")

# Indicate whether an RF is available already
rf_avail <- TRUE


# HIS

if (!exists("his", envir = globalenv())) {
  source("./src/functions/get_his.R")
  his <- get_his()
}

# Get sample_lists ----

source("./src/functions/get_sample_lists.R")

samples <- get_sample_lists()
glimpse(samples)

# High Intensity sites with sample pretreatment IDs

his_ids <- read_excel("./data/additional_data/his_ids.xlsx")
glimpse(his_ids)

# Data Survey123 app

source("./src/functions/get_app_data.R")
app_data_wide <- get_app_data()

# Get info about the MTA from the partner overview table

file_path <- "./data/additional_data/WILDCARD_partners_overview.xlsx"

drive_download(as_id(id_overview),
               path = file_path,
               overwrite = TRUE)

overview_partners <- read_excel(file_path,
                                sheet = 1) %>%
  filter(!is.na(partner)) %>%
  mutate(
    wp = case_when(
      grepl("WP2", partnerType) ~ "WP2",
      grepl("WP3", partnerType) ~ "WP3"),
    partner = case_when(
      partner == "BGDNP" ~ "BGD-NP",
      partner == "AGRARIA department - UNIRC" ~ "AlberIT - UNIRC",
      partner == "DISAFA - UNITO" ~ "UNITO",
      partner == "UniPa" ~ "UNIPA",
      partner == "VUK" & grepl("WP3", partnerType) ~ "VUKOZ(VUK)",
      TRUE ~ partner),
    MTA_type = coalesce(MTA_type, "not needed"))




# 1. Freezer (diepvries) ----

# This does not use the harmonised sample_ids, but the originally submitted
# sample_ids by the partners (sample lists Annex B and Annex C)

name_update <- readline(prompt = paste0("What is your name? ",
                                        "(i.e. first and family name of person",
                                        " updating the ",
                                        "freezer checklist) "))

s_freezer <- samples %>%
  filter(shipment_type == "cold") %>%
  filter(arrival_date_inbo > date_last_reg) %>%
  left_join(
    overview_partners %>%
      select(partner, MTA_type, wp),
    by = join_by("institute_sampling" == "partner",
                 "wp")) %>%
  mutate(
    sample_status_freezer = case_when(
      sample_code_harm == "OFH_eDNA1" ~ "Pending analysis ETHZ (eDNA1 OFH)",
      sample_code_harm == "M01_eDNA1" ~ "Pending shipment ETHZ (eDNA1 M01)",
      grepl("_eDNA2$", sample_code_harm) & wp == "WP3" ~
        "Pending funding availability (eDNA2 WP3)",
      grepl("_eDNA2$", sample_code_harm) & wp == "WP2" ~
        "Pending INBO eDNA analysis",
      grepl("_eDNA$", sample_code_harm) ~ "Back-up samples Belgium (eDNA)"),
    edna_inbo_batch_id = "",
    extract_fate_after_project = case_when(
      # eDNA2
      grepl("_eDNA2$", sample_code_harm) &
        wp == "WP2" &
        MTA_type %in% c("MTA2", "not needed") ~ "Archive in collection (MTA2)",
      grepl("_eDNA2$", sample_code_harm) &
        wp == "WP2" &
        MTA_type == "MTA1" ~ "Destroy (MTA1)",
      grepl("_eDNA2$", sample_code_harm) &
        (wp == "WP3" | grepl("^VUK", institute_sampling)) &
        MTA_type %in% c("MTA5", "not needed") ~
        "Send to VUK (if extracted) (MTA5)",
      # eDNA1
      grepl("_eDNA1$", sample_code_harm) &
        wp == "WP2" &
        MTA_type %in% c("MTA2", "not needed") ~
        "ETHZ: Send back to INBO to archive in collection (MTA4)",
      grepl("_eDNA1$", sample_code_harm) &
        wp == "WP2" &
        MTA_type == "MTA1" ~ "ETHZ: Destroy (MTA3)",
      grepl("_eDNA1$", sample_code_harm) &
        (wp == "WP3" | grepl("^VUK", institute_sampling)) &
        MTA_type %in% c("MTA5", "not needed") ~
        "ETHZ: Send to VUK (MTA6)"),
    last_update_date = as.Date(Sys.Date()),
    last_update_who = name_update) %>%
  # Sometimes you need to include an extra filter
  # since the capacity of the Windows clipboard is limited.
  # Just make sure that all samples are registered.
  # <= 100 rows are fine.
  # filter(grepl("UNIUD", institute_sampling) &
  #          wp == "WP3") %>%
  select(sample_id, sample_code, material, mass_sample_gross,
         plot_id, country_origin, institute_origin, res_id_inst, sub_id,
         wp, date_survey, date_shipment_departure, shipment_company,
         date_shipment_arrival, institute_receiving, transfer_third_party,
         cold_at_sampling, stored_frozen, shipped_frozen,
         country_code_harm, composed_site_id, sample_code_harm,
         sample_status_freezer, edna_inbo_batch_id, extract_fate_after_project,
         last_update_date, last_update_who)




glimpse(s_freezer)


# Copy to clipboard

write.table(s_freezer, "clipboard", sep = "\t",
            row.names = FALSE, col.names = FALSE)



# 2. Disturbed prep ----

sample_code_dist <- c("OFH_carbon",
                      "M01_carbon",
                      "M13_carbon",
                      "M36_carbon",
                      "M61_carbon")

s_dist_full <- samples %>%
  filter(shipment_type == "normal") %>%
  filter(grepl("carbon", sample_code_harm)) %>%
  filter(arrival_date_inbo > date_last_reg) %>%
  # In the end, we use plot_code_simple to join tables
  # plot_code_simple is basically composed_site_id, with "Transect X"
  # added to it for Bialowieza.
  # (because plot_id changes while surveys are being submitted,
  #  and is only important for Bialowieza's unique plot key)
  left_join(his_ids %>%
              mutate(
                plot_code_simple = ifelse(
                  composed_site_id == "WULS__1__Bialowieza National Park__NA",
                  case_when(
                    grepl("Transect V$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect V", sep = "_"),
                    grepl("Transect IV$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect IV", sep = "_"),
                    grepl("Transect III$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect III", sep = "_"),
                    grepl("Transect II$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect II", sep = "_")),
                  composed_site_id)) %>%
              select(plot_code_simple, id_sample_pretreatment),
            by = join_by("plot_code_simple"))

assertthat::assert_that(all(!is.na(s_dist_full$id_sample_pretreatment)))

s_dist_full <- s_dist_full %>%
  mutate(
    staal_id_pretrt =
      paste0("C", "-",
             str_pad(id_sample_pretreatment, width = 3, pad = "0"), "-",
             sub("_.*$", "", sample_code_harm))) %>%
  mutate(sample_code_harm = factor(sample_code_harm,
                                   levels = sample_code_dist)) %>%
  arrange(institute_sampling, reserve_name,
          plot_code_harm, sample_code_harm)

glimpse(s_dist_full)







# 3. RF ----

if (rf_avail == TRUE) {


  source("./src/functions/get_rf.R")
  # Loads the rf_overview table to the Global Environment.

  # Filters for the RFs you will need, considering the institutes that have
  # sent samples since the last update of the registration forms.

  rf_overview_sel <- get("rf_overview") %>%
    filter(
      mapply(
        function(p) any(grepl(p, unique(s_dist_full$institute_sampling))),
        partners))



  # Extract some metadata from the Survey123 app and HIS metadata
  # in order to fill in in the RFs

  metadata_rf <- s_dist_full %>%
    distinct(plot_code_simple, composed_site_id) %>%
    left_join(
      app_data_wide %>%
        mutate(
          # Calculate the average of the OFH thickness
          # (replacing any NAs by 0)
          ofh_thickness = round(rowMeans(
            across(matches("^P[1-5]_OFH_thickness$"), ~replace_na(., 0))),
            1)) %>%
        rowwise() %>%
        mutate(
          bedrock_vals = list(c_across(matches("^P[1-5]_depth_bedrock$"))),
          n_filled = sum(!is.na(bedrock_vals)),
          all_shallow = all(bedrock_vals[!is.na(bedrock_vals)] <= 30),
          n_missing = sum(is.na(bedrock_vals)),
          flag_suspicious = n_filled >= 3 & all_shallow & n_missing > 0,

          depth_bedrock = if (flag_suspicious) {
            round(mean(bedrock_vals, na.rm = TRUE))  # ignore NAs
          } else {
            round(mean(replace_na(bedrock_vals, 100)))  # assume 100 if NA
          }
        ) %>%
        ungroup() %>%
        select(plot_code_simple, latitude, longitude, ofh_thickness,
               depth_bedrock, date_time),
      by = "plot_code_simple") %>%
    left_join(
      his %>%
        select(plot_code_simple, latitude, longitude) %>%
        rename(latitude_his = latitude,
               longitude_his = longitude),
      by = "plot_code_simple") %>%
    mutate(
      latitude = coalesce(latitude, latitude_his),
      longitude = coalesce(longitude, longitude_his))

  glimpse(metadata_rf)

  if (any(is.na(metadata_rf$ofh_thickness))) {

    cat(paste0("Best to remind VUK to update the Survey123 app data, ",
               "since incomplete for the latest batch of samples that ",
               "arrived.\n"))
  }


  # Prepare a table with samples for the RFs following the RF template

  s_rf <- s_dist_full %>%
    left_join(
      metadata_rf %>%
        select(-composed_site_id),
      by = "plot_code_simple") %>%
    left_join(
      overview_partners %>%
        filter(grepl("WP2", partnerType)) %>%
        select(partner, MTA_type),
      by = join_by("institute_sampling" == "partner")) %>%
    rowwise() %>%
    mutate(
      veld_id = sample_id_harm,
      datum_bemonstering = coalesce(
        as.Date(date_time),
        as.Date(date_survey)),
      monsternemer = institute_sampling,
      archiefstaal = case_when(
        MTA_type %in% c("MTA2", "not needed") ~ "Ja",
        MTA_type %in% c("MTA1") ~ "Nee",
        # VUK wants their WP2 samples to be sent back to them, so probably
        # not needed to put them in the soil archive?
        MTA_type %in% c("MTA5") ~ "Nee"),
      toponym = reserve_name,
      # RFs are using commas as decimal separators
      wgs84lat = gsub("\\.", ",", as.character(round(latitude, 5))),
      wgs84lon = gsub("\\.", ",", as.character(round(longitude, 5))),
      diepte_monster = case_when(
        grepl("OFH", sample_code_harm) ~ paste0("-", ofh_thickness, "-0"),
        grepl("M01", sample_code_harm) ~
          ifelse(!is.na(depth_bedrock),
                 paste0("0-", as.character(min(c(10, depth_bedrock)))),
                 "0-10"),
        grepl("M13", sample_code_harm) ~
          ifelse(!is.na(depth_bedrock),
                 paste0("10-", as.character(min(c(30, depth_bedrock)))),
                 "10-30"),
        grepl("M36", sample_code_harm) ~
          ifelse(!is.na(depth_bedrock),
                 paste0("30-", as.character(min(c(60, depth_bedrock)))),
                 "30-60"),
        grepl("M61", sample_code_harm) ~
          ifelse(!is.na(depth_bedrock),
                 paste0("60-", as.character(min(c(100, depth_bedrock)))),
                 "60-100")),
      # To paste on tab "LABELFORMULIER" column J ("Archief_ID")
      archief_id = paste(
        coalesce(res_id_inst_harm, as.character(res_id_inst)),
        coalesce(sub_id_harm, as.character(sub_id)),
        coalesce(plot_id_harm,
                 "NA"),
        sep = "__")) %>%
    ungroup() %>%
    # prepend a quote, so that Excel and Google Sheets will treat it this
    # column as plain text instead of trying to interpret it as a date
    mutate(diepte_monster = paste0("'", diepte_monster)) %>%
    select(
      sample_code_harm,
      veld_id, datum_bemonstering, monsternemer,
      archiefstaal, toponym, wgs84lat, wgs84lon,
      diepte_monster, archief_id)

  glimpse(s_rf)





  # Runnen tot hier en dan apart copy-pasten



  ## 3.1. Mineral ----

  s_rf_mineral <- s_rf %>%
    filter(grepl("M01|M13|M36|M61", sample_code_harm)) %>%
    select(-sample_code_harm)

  cat(paste0("Paste the data in the following RF (mineral samples):\n",
             rf_overview_sel %>%
               filter(samples != "OFH") %>%
               pull(rf)))


  # Tab "STAALFORMULIER": Firstly paste (ctrl V) this in column E

  write.table(s_rf_mineral %>%
                select(veld_id),
              "clipboard", sep = "\t",
              row.names = FALSE, col.names = FALSE)


  # Tab "STAALFORMULIER": Then paste (crtl V) this in columns Q-W

  write.table(s_rf_mineral %>%
                select(-veld_id, -archief_id),
              "clipboard", sep = "\t",
              row.names = FALSE, col.names = FALSE)

  # Tab "LABELFORMULIER": Finally paste (ctrl V) this in column J

  write.table(s_rf_mineral %>%
                select(archief_id),
              "clipboard", sep = "\t",
              row.names = FALSE, col.names = FALSE)








  ## 3.2. OFH ----

  s_rf_ofh <- s_rf %>%
    filter(grepl("OFH", sample_code_harm)) %>%
    select(-sample_code_harm)

  cat(paste0("Paste the data in the following RF (OFH samples):\n",
             rf_overview_sel %>%
               filter(samples == "OFH") %>%
               pull(rf)))


  # Tab "STAALFORMULIER": Firstly paste (ctrl V) this in column E

  write.table(s_rf_ofh %>%
                select(veld_id),
              "clipboard", sep = "\t",
              row.names = FALSE, col.names = FALSE)


  # Tab "STAALFORMULIER": Then paste (crtl V) this in columns Q-W

  write.table(s_rf_ofh %>%
                select(-veld_id, -archief_id),
              "clipboard", sep = "\t",
              row.names = FALSE, col.names = FALSE)

  # Tab "LABELFORMULIER": Finally paste (ctrl V) this in column J

  write.table(s_rf_ofh %>%
                select(archief_id),
              "clipboard", sep = "\t",
              row.names = FALSE, col.names = FALSE)


} # End of "if rf_avail == TRUE"










# 4. WBL ----
## 4.1. WBL disturbed ----


if (rf_avail == TRUE) {

  # Get the information you just filled in in the RFs, since the IDs are needed

  source("./src/functions/get_rf.R")
  rf_all <- get_rf()
  glimpse(rf_all)

  s_dist <- s_dist_full %>%
    left_join(
      rf_all %>%
        select(lims_project_code, lims_sample_id, sample_id),
      by = join_by("sample_id_harm" == "sample_id"))

}

if (rf_avail == FALSE) {

  s_dist <- s_dist_full %>%
    mutate(
      lims_project_code = "",
      lims_sample_id = "")

}


s_dist <- s_dist %>%
  left_join(
    overview_partners %>%
      select(partner, MTA_type),
    by = join_by("institute_sampling" == "partner")) %>%
  mutate(
    # All international WP2 samples need to be sieved and milled
    sieving_milling = "Ja",
    archiving = case_when(
      MTA_type %in% c("MTA2", "not needed") ~ "Ja",
      MTA_type %in% c("MTA1") ~ "Apart (geen AID)",
      # VUK wants their WP2 samples to be sent back to them, so probably
      # not needed to put them in the soil archive?
      MTA_type %in% c("MTA5") ~ "Apart (geen AID)")) %>%
  select(sample_id_harm,
         reserve_name,
         staal_id_pretrt,
         lims_sample_id,
         sieving_milling,
         archiving,
         lims_project_code)

glimpse(s_dist)


# Copy to clipboard and paste in WBL-geroerd (disturbed)

write.table(s_dist, "clipboard", sep = "\t",
            row.names = FALSE, col.names = FALSE)









## 4.2. WBL undisturbed ----

sample_code_undist <- c("M01_Bulk_Density",
                        "M13_Bulk_Density",
                        "M36_Bulk_Density",
                        "M61_Bulk_Density")

s_undist_full <- samples %>%
  filter(shipment_type == "normal") %>%
  filter(grepl("Bulk", sample_code_harm)) %>%
  filter(arrival_date_inbo > date_last_reg) %>%
  left_join(his_ids %>%
              mutate(
                plot_code_simple = ifelse(
                  composed_site_id == "WULS__1__Bialowieza National Park__NA",
                  case_when(
                    grepl("Transect V$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect V", sep = "_"),
                    grepl("Transect IV$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect IV", sep = "_"),
                    grepl("Transect III$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect III", sep = "_"),
                    grepl("Transect II$", plot_id, ignore.case = TRUE) ~
                      paste(composed_site_id, "Transect II", sep = "_")),
                  composed_site_id)) %>%
              select(plot_code_simple, id_sample_pretreatment),
            by = "plot_code_simple")


assertthat::assert_that(all(!is.na(s_undist_full$id_sample_pretreatment)))

s_undist <- s_undist_full %>%
  mutate(
    staal_id_pretrt =
      paste0("BD", "-",
             str_pad(id_sample_pretreatment, width = 3, pad = "0"), "-",
             sub("_.*$", "", sample_code_harm)),
    # All WP2 undisturbed samples need to be weighed
    weighing = "Ja") %>%
  mutate(sample_code_harm = factor(sample_code_harm,
                                   levels = sample_code_undist)) %>%
  arrange(institute_sampling, reserve_name,
          plot_code_harm, sample_code_harm) %>%
  select(sample_id_harm,
         reserve_name,
         staal_id_pretrt,
         weighing)

glimpse(s_undist)



# Copy to clipboard and paste in WBL-ongeroerd (undisturbed)

write.table(s_undist, "clipboard", sep = "\t",
            row.names = FALSE, col.names = FALSE)


# Don't forget to update the date of the last sample registration
# in the beginning of this script







