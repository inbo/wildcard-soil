
get_his <- function(include_extra_plots = TRUE,
                    add_plot_ids_from_app = TRUE) {

  stopifnot(require("tidyverse"),
            require("googledrive"),
            require("googlesheets4"),
            require("readxl"))

  if (add_plot_ids_from_app == TRUE) {

    source("./src/functions/get_app_data.R")
    app_data <- get_app_data()

  }

  # Source URLs of sensitive Google Drive links
  source("./data/sensitive_metadata/google_drive_links.R")

  his <- read_sheet(as_id(id_his)) %>%
    rename_with(tolower) %>%
    rename(sub_id = wildcard_sub_id) %>%
    rename(his_soil_water = `confirmed_his_soil-water`,
           his_soil_nutrient = `confirmed_his_soil-nutrient`,
           his_eftc = confirmed_his_fortype,
           his_tsaclass = confirmed_his_tsaclass,
           change_date = `date of last change`) %>%
    filter(his_selected == "Y" | his_selected == "pending") %>%
    filter(!is.na(composed_site_id)) %>%
    mutate(across(where(is.list),
                  ~ map_chr(., ~ ifelse(is.null(.),
                                        NA_character_,
                                        as.character(.))))) %>%
    select(composed_site_id,
           institute,
           res_id_inst,
           reserve_name,
           sub_id,
           latitude,
           longitude,
           his_soil_water,
           his_soil_nutrient,
           confirmed_his_soil,
           his_tsaclass,
           his_eftc,
           eea_fortype,
           derived_tsa,
           year_reserve,
           year_abandonment)


  if (include_extra_plots == TRUE) {

    extra_his <- data.frame(
      composed_site_id = c(
        # BFNP (extra sites as requested and paid for by BFNP)
        "BFNP_EXTRA__59__Recherau__NA",
        "BFNP_EXTRA__56__Haselau__NA",
        # UNIUD (extra baseline sites for managed forests)
        "UNIUD_EXTRA__1__Cansiglio__AF-MN",
        "UNIUD_EXTRA__2__Val Alba__BF-MN",
        "UNIUD/HSI_EXTRA__3__Limsky Kanal__LI-MN",
        # WULS (IBL) (four plots sampled within one his site)
        "WULS__1__Białowieża National Park__Transect II",
        "WULS__1__Białowieża National Park__Transect III",
        "WULS__1__Białowieża National Park__Transect IV",
        "WULS__1__Białowieża National Park__Transect V"))

    cat("\nThis HIS table includes the following sites/plots:\n")
    print(extra_his)
    cat("\nIf you do not want this, set 'include_extra_plots' to FALSE.\n\n")

    extra_his <- extra_his %>%
      separate(composed_site_id,
               into = c("institute", "res_id_inst", "reserve_name", "sub_id"),
               sep = "__", fill = "right", remove = FALSE) %>%
      mutate(
        his_tsaclass = ifelse(
          institute %in% c("UNIUD_EXTRA", "UNIUD/HSI_EXTRA"), "0", NA))

    his <- his %>%
      mutate(site_type = "his") %>%
      # For WULS (IBL) Bialowieza, composed_site_id won't be unique
      # (i.e., there will be four records with the same composed_site_id)
      # However, plot_code will be unique (includes plot_id representing
      # different transects).
      # Remark: composed_site_id does not correspond with the composed_site_id
      # in the service contract attachment for IBL
      # (IBL_Attachment-No4_List-sites_HIS.xlsx)
      mutate(plot_id = case_when(
        grepl("^WULS__1__", composed_site_id) ~ "Transect II",
        # At the moment, no other plot_ids from the app are inserted yet.
        TRUE ~ NA)) %>%
      # Add rows for the other three Bialowieza transects
      bind_rows(
        his %>%
          filter(grepl("^WULS__1__", composed_site_id)) %>%
          mutate(plot_id = "Transect III"),
        his %>%
          filter(grepl("^WULS__1__", composed_site_id)) %>%
          mutate(plot_id = "Transect IV"),
        his %>%
          filter(grepl("^WULS__1__", composed_site_id)) %>%
          mutate(plot_id = "Transect V")) %>%
      # Add other extra sites from BFNP and UNIUD
      bind_rows(
        extra_his %>%
          filter(!institute == "WULS") %>%
          mutate(
            site_type = case_when(
              institute == "BFNP_EXTRA" ~ "bfnp_extra",
              institute %in% c("UNIUD_EXTRA",
                               "UNIUD/HSI_EXTRA") ~ "managed"))) %>%
      arrange(institute, res_id_inst) %>%
      relocate(plot_id, .after = sub_id)

    assertthat::assert_that(
      nrow(his) == 208,
      msg = "There should be 208 records (plots).")

    message(paste0("Metadata (soil water, soil nutrient, EFTC) are unknown ",
                   "for extra sites UNIUD and BFNP."))

    if (add_plot_ids_from_app == TRUE) {

      his <- his %>%
        left_join(
          app_data %>%
            filter(wp == "WP2") %>%
            select(composed_site_id, plot_id_harmonized),
          by = "composed_site_id")

      # Make sure that the number of HIS with known plot_id is the same like
      # the number of surveys that has been submitted in the Survey123 app
      # for WP2

      vec_app <- app_data$composed_site_id[which(app_data$wp == "WP2")]
      vec_his <- his$composed_site_id[which(!is.na(his$plot_id_harmonized))]

      assertthat::assert_that(
        length(vec_app) == length(vec_his),
        msg = paste0("The number of HIS with known plot_id is not the same ",
                     "like the number of WP2 surveys submitted in the ",
                     "Survey123 app. This probably means that something ",
                     "is wrong with the composed_site_id automatically ",
                     "generated in overview (identification) file of the ",
                     "Survey123 submissions. ",
                     "Please check these composed_site_ids:\n",
                     paste(vec_app[which(!vec_app %in% vec_his)],
                           collapse = ",  ")))

      message(paste0("So far, ", length(vec_his), " plot_ids out of 208 are ",
                     "known for WP2. This is derived from the Survey123 app ",
                     "(harmonized) and therefore depends on the number of ",
                     "survey submissions in the app"))

      # Harmonise plot_id

      his <- his %>%
        mutate(
          plot_id = coalesce(plot_id_harmonized, plot_id),
          plot_code = paste(
            institute,
            res_id_inst,
            sub_id,
            plot_id,
            sep = "__")) %>%
        select(-plot_id_harmonized) %>%
        relocate(plot_id, .after = sub_id) %>%
        relocate(plot_code, .after = composed_site_id)

      assertthat::assert_that(n_distinct(his$plot_code) == 208)

    } else {

      # add_plot_ids_from_app == FALSE

      his <- his %>%
        mutate(
          plot_code = paste(
            institute,
            res_id_inst,
            sub_id,
            plot_id,
            sep = "__")) %>%
        relocate(plot_id, .after = sub_id) %>%
        relocate(plot_code, .after = composed_site_id)

      assertthat::assert_that(n_distinct(his$plot_code) == 208)

      message("Plot_ids have not been inserted, except for Bialowieza.")

    } # End of 'add_plot_ids_from_app == FALSE'




  } else {

    # include_extra_plots == FALSE

    his <- his %>%
      mutate(
        plot_id = NA,
        plot_code = paste(
          institute,
          res_id_inst,
          sub_id,
          plot_id,
          sep = "__")) %>%
      relocate(plot_id, .after = sub_id) %>%
      relocate(plot_code, .after = composed_site_id)

    assertthat::assert_that(n_distinct(his$plot_code) == 200)

    cat(paste0("\nThis HIS dataframe only includes the 200 selected ",
               "High Intensity Sites. Extra plots or sites have not been ",
               "inserted.\n\n"))

  } # End of 'include_extra_plots == FALSE'


  his <- his %>%
    mutate(
      # Institute that does the soil sampling
      institute_sampling = case_when(
        institute %in% c("BFNP_EXTRA", "NPS") ~ "BFNP",
        institute == "LWF" &
          reserve_name == "Hoellbachgespreng" ~ "BFNP",
        institute == "DISAFA - UNITO" ~ "UNITO",
        institute == "FVA-BW" ~ "NWFVA",
        institute == "INCDS" ~ "UNITBV",
        institute %in% c("UNIUD/HSI", "UNIUD/HSI_EXTRA",
                         "UNIUD_EXTRA") ~ "UNIUD",
        institute %in% c("URK", "WULS") ~ "IBL",
        TRUE ~ institute)) %>%
    relocate(institute_sampling, .after = institute)


  return(his)
}
