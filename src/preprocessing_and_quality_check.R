
# Check partner information
# -------------------------

# This script creates partner inconsistency reports with inconsistencies
# in the data submitted by the partner (Survey123 + forest floor masses)

# This needs to be elaborated. The R scripts contain several "#PIR: ..."
# comments with rules that need to be checked.



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


# Get sample_lists

source("./src/functions/get_sample_lists.R")

samples <- get_sample_lists()

# Get site metadata

if (!exists("his", envir = globalenv())) {
  source("./src/functions/get_his.R")
  his <- get_his()
}

if (!exists("wp3_sites", envir = globalenv())) {
  source("./src/functions/get_wp3_sites.R")
  wp3_sites <- get_wp3_sites()
}


write.table(his,
            file = "./data/his.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

write.table(wp3_sites,
            file = "./data/wp3_sites.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")

# Data Survey123 app

source("./src/functions/get_app_data.R")
app_data <- get_app_data()



# PIR: Compare sample list with app
# PIR: check distance from HIS metadata table coordinates
# PIR: coordinates should not be 0



# PIR: a few bedrock depths forgotten?

app_data %>%
  rowwise() %>%
  mutate(
    bedrock_vals = c_across(matches("^P[1-5]_depth_bedrock$")),
    n_filled = sum(!is.na(bedrock_vals)),
    all_shallow = all(bedrock_vals[!is.na(bedrock_vals)] <= 20),
    n_missing = sum(is.na(bedrock_vals)),
    flag_suspicious = n_filled >= 2 & all_shallow & n_missing > 0
  ) %>%
  ungroup() %>%
  select(-bedrock_vals, -n_filled, -all_shallow, -n_missing)

















df_rings <- app_data %>%
  select(code, plot_code, composed_site_id, wp,
         team_harmonized, institute_sampling,
         # Volume of kopecky ring
         # (typically one ring per depth x sampling point)
         vol_ring,
         starts_with("P")) %>%
  select(code, plot_code, composed_site_id, wp,
         team_harmonized, institute_sampling,
         vol_ring, ends_with("_bulk_density_lyr")) %>%
  pivot_longer(cols = starts_with("P", ignore.case = FALSE),
               names_to = "sampling_point",
               values_to = "code_layer") %>%
  mutate(
    # Keep only "P1", "P2", etc.
    sampling_point = str_extract(sampling_point, "^P\\d+")) %>%
  separate_rows(code_layer, sep = ",") %>%
  # mutate(code_depths = as.integer(str_trim(code_depths))) %>%
  # left_join(d_kopecky_depths,
  #           by = c("code_depths" = "code")) %>%
  filter(!is.na(code_layer)) %>%
  group_by(code, plot_code, composed_site_id, wp,
           team_harmonized, institute_sampling,
           code_layer, vol_ring) %>%
  reframe(
    undist_sampling_points = paste(sort(unique(sampling_point)),
                                   collapse = ","),
    count_rings = n())
















