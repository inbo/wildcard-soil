
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



# 2. Get data ----

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
app_data_wide <- get_app_data()



# Get sample_lists

source("./src/functions/get_sample_lists.R")

samples <- get_sample_lists()


# PIR: Compare sample list with app
# PIR: check distance from HIS metadata table coordinates
# PIR: coordinates should not be 0

# PIR: double-check rows for which tamass is NA (simply no forest floor?)
# (using tamass_max)



# PIR: a few bedrock depths forgotten?










# calculate coarse fragments etc






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












# Compile inconsistencies ----

inc_files <- c(
  # INCONSISTENCY 1
  # "Data expected to be numeric should be numeric."
  # get_app_data()
  "inconsistencies_app_numeric",
  # INCONSISTENCY 2
  # "Forest floor mass should be plausible in comparison with thickness
  #  and surface area frame"
  # app_data_long()
  # INCONSISTENCY 4
  # "Undisturbed mass should be plausible in comparison with
  #  number and volume of rings"
  # app_data_long()
  # INCONSISTENCY 5
  # "Subsample forest floor for lab cannot weigh more than sum of
  #  all reported forest floor masses across sampling points"
  # app_data_long()
  "inconsistencies_app_long"

)



