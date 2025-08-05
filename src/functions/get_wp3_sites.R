
get_wp3_sites <- function() {

  stopifnot(require("tidyverse"),
            require("googledrive"),
            require("googlesheets4"),
            require("readxl"))

  # if (add_plot_ids_from_app == TRUE) {
  #
  #   source("./src/functions/get_app_data.R")
  #   app_data <- get_app_data()
  #
  # }

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
        is.na(site_id) | str_trim(site_id) == "" ~ `no.`,
        TRUE ~ site_id),
      plot_code = paste(
        institute,
        chronosequence_id,
        site_id,
        plot_id,
        sep = "__"))


  # if (add_plot_ids_from_app == TRUE) {
  #
  #   wp3_sites <- wp3_sites %>%
  #     left_join(
  #       app_data %>%
  #         filter(wp == "WP3") %>%
  #         select(composed_site_id, plot_id_harmonized),
  #       by = "composed_site_id")
  #
  #   # Make sure that the number of HIS with known plot_id is the same like
  #   # the number of surveys that has been submitted in the Survey123 app
  #   # for WP2
  #
  #   vec_app <- app_data$composed_site_id[which(app_data$wp == "WP3")]
  #   vec_his <-
  #     wp3_sites$composed_site_id[which(!is.na(wp3_sites$plot_id_harmonized))]
  #
  #   # assertthat::assert_that(
  #   #   length(vec_app) == length(vec_his),
  #   #   msg = paste0("The number of HIS with known plot_id is not the same ",
  #   #                "like the number of WP2 surveys submitted in the ",
  #   #                "Survey123 app. This probably means that something ",
  #   #                "is wrong with the composed_site_id automatically ",
  #   #                "generated in overview (identification) file of the ",
  #   #                "Survey123 submissions. ",
  #   #                "Please check these composed_site_ids:\n",
  #   #                paste(vec_app[which(!vec_app %in% vec_his)],
  #   #                      collapse = ",  ")))
  #
  #
  #
  #   # Harmonise plot_id
  #
  #   wp3_sites <- wp3_sites %>%
  #     mutate(
  #       plot_id = coalesce(plot_id_harmonized, plot_id),
  #       site_id = case_when(
  #         is.na(site_id) | str_trim(site_id) == "" ~ `no.`,
  #         TRUE ~ site_id),
  #       plot_code = paste(
  #         institute,
  #         chronosequence_id,
  #         site_id,
  #         plot_id,
  #         sep = "__")) %>%
  #     select(-plot_id_harmonized) %>%
  #     relocate(plot_id, .after = site_id) %>%
  #     relocate(plot_code, .after = composed_site_id)
  #
  #   assertthat::assert_that(n_distinct(wp3_sites$plot_code) == 129)
  #
  # } else {
  #
  #   # add_plot_ids_from_app == FALSE
  #
  #   wp3_sites <- wp3_sites %>%
  #     mutate(
  #       site_id = case_when(
  #         is.na(site_id) | str_trim(site_id) == "" ~ `no.`,
  #         TRUE ~ site_id),
  #       plot_code = paste(
  #         institute,
  #         chronosequence_id,
  #         site_id,
  #         plot_id,
  #         sep = "__")) %>%
  #     relocate(plot_id, .after = site_id) %>%
  #     relocate(plot_code, .after = composed_site_id)
  #
  #   assertthat::assert_that(n_distinct(wp3_sites$plot_code) == 129)
  #
  #   message("Plot_ids have not been inserted.")
  #
  # } # End of 'add_plot_ids_from_app == FALSE'






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

  return(wp3_sites)


}
