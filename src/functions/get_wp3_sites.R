
get_wp3_sites <- function() {

  stopifnot(require("tidyverse"),
            require("googledrive"),
            require("googlesheets4"),
            require("readxl"))

  # Source URLs of sensitive Google Drive links
  source("./data/sensitive_metadata/google_drive_links.R")


  wp3_sites <- read_excel(paste0("./data/",
                                 "WP3_Site_characteristics_WILDCARD.xlsx"),
                          range = "A1:AE1000") %>%
    # Replace spaces with underscores in column names
    # rename_with(~ gsub(" ", "_", .x)) %>%
    rename_with(tolower) %>%
    rename(institute = `responsible team /internal or external`,
           chronosequence_id = `chronosequence id`,
           site_id = `site id`,
           plot_id = `plot id`,
           year_abandonment = `year of abandonment (calendar year, 1-2025)`,
           his_soil_water = `water impact`,
           his_eftc = `forest type (eea)`,
           latitude = `latitude (wgs)`,
           longitude = `longitude (wgs)`) %>%
    filter(`soil c` %in% c("Sampled", "Will be sampled")) %>%
    filter(edna %in% c("Sampled", "Will be sampled")) %>%
    filter(!(region == "Tokaj" & institute == "VUKOZ(VUK)")) %>%
    mutate(
      latitude = case_when(
        latitude %in% c("", " ", NA) ~ NA_real_,
        TRUE ~ suppressWarnings(as.numeric(latitude))),
      longitude = case_when(
        longitude %in% c("", " ", NA) ~ NA_real_,
        TRUE ~ suppressWarnings(as.numeric(longitude)))) %>%
    mutate(
      composed_site_id = paste(
        institute,
        chronosequence_id,
        region,
        site_id,
        sep = "__"),
      site_id = case_when(
        is.na(site_id) | str_trim(site_id) == "" ~ as.character(`no.`),
        TRUE ~ site_id),
      plot_code = paste(
        institute,
        chronosequence_id,
        site_id,
        plot_id,
        sep = "__"))





  wp3_sites <- wp3_sites %>%
    select(
      composed_site_id,
      plot_code,
      institute,
      region,
      chronosequence_id,
      site_id,
      plot_id,
      year_abandonment,
      his_soil_water,
      his_eftc,
      latitude,
      longitude)

  wp3_sites <- wp3_sites %>%
    mutate(
      # Institute that does the soil sampling
      institute_sampling = case_when(
        institute %in% c("HUN-REN Centre for Ecological Research/VUKOZ(VUK)") ~
          "VUKOZ(VUK)",
        TRUE ~ institute),
      plot_code_simple = composed_site_id) %>%
    relocate(institute_sampling, .after = institute) %>%
    relocate(plot_code_simple, .after = plot_code)


  return(wp3_sites)


}
