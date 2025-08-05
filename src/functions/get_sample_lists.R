
get_sample_lists <- function() {

  # Compiles a sample list based on the sample lists submitted by the partners
  # (Annex B and Annex C) for which the reported samples have been checked
  # (and possibly corrected) + manually harmonised (adding composed_site_id)
  # by the central lab.
  # This script also harmonises the sample_id.

  # Only for external partners, not for INBO


  stopifnot(require("tidyverse"),
            require("googledrive"),
            require("googlesheets4"),
            require("readxl"),
            require("purrr"),
            require("parsedate"))

  # Source URLs of sensitive Google Drive links
  source("./data/sensitive_metadata/google_drive_links.R")

  # To get plot_ids

  if (exists("his", envir = .GlobalEnv)) {

    his_plot_ids <- get("his") %>%
      distinct(plot_code, .keep_all = TRUE) %>%
      select(composed_site_id, plot_id) %>%
      mutate(wp = "WP2") %>%
      rename(plot_id_harm = plot_id)
  }

  if ((exists("his", envir = .GlobalEnv) && nrow(his_plot_ids) != 208) ||
      !exists("his", envir = .GlobalEnv)) {

    source("./src/functions/get_his.R")
    his_plot_ids <- get_his() %>%
      distinct(plot_code, .keep_all = TRUE) %>%
      select(composed_site_id, plot_id) %>%
      mutate(wp = "WP2") %>%
      rename(plot_id_harm = plot_id)

  }

  if (exists("wp3_sites", envir = .GlobalEnv)) {

    wp3_plot_ids <- get("wp3_sites") %>%
      distinct(plot_code, .keep_all = TRUE) %>%
      select(composed_site_id, plot_id) %>%
      mutate(wp = "WP3") %>%
      rename(plot_id_harm = plot_id)
  }

  if ((exists("wp3_sites", envir = .GlobalEnv) && nrow(wp3_plot_ids) != 129) ||
      !exists("wp3_sites", envir = .GlobalEnv)) {

    source("./src/functions/get_wp3_sites.R")
    wp3_plot_ids <- get_wp3_sites() %>%
      distinct(plot_code, .keep_all = TRUE) %>%
      select(composed_site_id, plot_id) %>%
      mutate(wp = "WP3") %>%
      rename(plot_id_harm = plot_id)

  }


  # Get the list of files (harmonised sample lists) on Google Drive

  sample_lists <- drive_ls(as_id(id_sample_lists)) %>%
    # Filter only .xlsx files or Google Sheets
    filter(
      grepl("\\.xlsx$", name, ignore.case = TRUE) |
        map_chr(drive_resource,
                ~ .x$mimeType) == "application/vnd.google-apps.spreadsheet")

  # Columns to standardise
  required_cols <- c("sample_id", "sample_code", "material",
                     "mass_sample_gross", "plot_id", "country_origin",
                     "institute_origin", "res_id_inst",
                     "sub_id", "wp", "date_survey", "date_shipment_departure",
                     "shipment_company", "date_shipment_arrival",
                     "institute_receiving",
                     "transfer_third_party", "cold_at_sampling",
                     "stored_frozen", "shipped_frozen")

  optional_cols <- c("country_code_harm", "composed_site_id",
                     "sample_code_harm")

  all_cols <- c(required_cols, optional_cols)



  # Function to read and clean one file
  read_and_standardise <- function(file) {

    mime_type <- file$drive_resource[[1]]$mimeType

    # Read file depending on its type
    if (mime_type == "application/vnd.google-apps.spreadsheet") {
      df <- suppressMessages(read_sheet(file$id))
    } else {
      tmp <- tempfile(fileext = ".xlsx")
      suppressMessages(drive_download(file, path = tmp, overwrite = TRUE))
      df <- read_excel(tmp)
    }


    # Ensure all required and optional columns exist
    for (col in all_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA
      }
    }

    # Select and reorder columns
    df <- df %>%
      select(any_of(all_cols)) %>%
      relocate(any_of(all_cols)) %>% # Moves columns to desired order
      mutate(across(c(plot_id, res_id_inst, sub_id),
                    as.character)) %>%
      mutate(mass_sample_gross = as.numeric(mass_sample_gross),
             transfer_third_party = as.character(transfer_third_party),
             shipped_frozen = as.character(shipped_frozen),
             cold_at_sampling = as.character(cold_at_sampling),
             stored_frozen = as.character(stored_frozen)) %>%
      mutate(composed_site_id = case_when(composed_site_id == "" ~ NA,
                                          TRUE ~ composed_site_id)) %>%
      mutate(
        arrival_date_inbo =
          parsedate::parse_date(unlist(strsplit(file$name, "_"))[1]),
        shipment_type = unlist(strsplit(file$name, "_"))[3],
        # Create column only if missing
        # wp =
        #   if (!"wp" %in% names(.)) NA_character_ else wp,
        wp = case_when(
          !is.na(wp) ~ wp,
          shipment_type == "normal" ~ "WP2",
          TRUE ~ NA)) %>%
      relocate(wp, .after = sub_id)



    df <- df %>%
      separate_wider_delim(
        composed_site_id,
        delim = "__",
        names = c("institute_harm", "res_id_inst_harm", "reserve_name",
                  "sub_id_harm"),
        too_few = "align_start",
        cols_remove = FALSE
      ) %>%
      left_join(
        bind_rows(his_plot_ids,
                  wp3_plot_ids),
        by = join_by("composed_site_id", "wp")) %>%
      mutate(
        plot_code_harm = paste(
          coalesce(institute_harm, as.character(institute_origin)),
          coalesce(res_id_inst_harm, as.character(res_id_inst)),
          coalesce(sub_id_harm, as.character(sub_id)),
          coalesce(plot_id_harm,
                   "NA"),
          sep = "__"),
        sample_id_harm = paste(
          country_code_harm,
          coalesce(institute_harm, as.character(institute_origin)),
          coalesce(res_id_inst_harm, as.character(res_id_inst)),
          coalesce(sub_id_harm, as.character(sub_id)),
          coalesce(plot_id_harm,
                   "NA"),
          sample_code_harm,
          # Better to use "__" rather than "-" as in the Survey123 app
          # since several entries contain a "-" already,
          # and since we are making a new harmonised sample_id anyway...
          sep = "__")) %>%
      relocate(plot_code_harm, sample_id_harm, reserve_name,
               .before = sample_id) %>%
      mutate(
        # Institute that does the soil sampling
        institute_sampling = case_when(
          institute_harm %in% c("BFNP_EXTRA", "NPS") ~ "BFNP",
          institute_harm == "LWF" &
            reserve_name == "Hoellbachgespreng" ~ "BFNP",
          institute_harm == "DISAFA - UNITO" ~ "UNITO",
          institute_harm == "FVA-BW" ~ "NWFVA",
          institute_harm == "INCDS" ~ "UNITBV",
          institute_harm %in% c("UNIUD/HSI", "UNIUD/HSI_EXTRA",
                                "UNIUD_EXTRA") ~ "UNIUD",
          institute_harm %in% c("URK", "WULS") ~ "IBL",
          TRUE ~ institute_harm))


    return(df)
  } # End of read_and_standardise



  # Read and combine all files

  all_samples <- map_dfr(seq_len(nrow(sample_lists)), function(i) {
    read_and_standardise(sample_lists[i, ])
  })



  # Remove files from today from temporary directory

  files_temp <- list.files(tempdir(), full.names = TRUE)

  recent_files_temp <-
    files_temp[file.info(files_temp)$mtime >=
                 (Sys.time() - as.difftime(10, units = "mins"))]

  if (!identical(recent_files_temp, character(0))) {
    unlink(recent_files_temp, recursive = TRUE)
  }





  return(all_samples)



}
