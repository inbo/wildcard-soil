
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






