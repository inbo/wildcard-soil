
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

    # TO DO: for LWF, the sample list (normal shipment) contains two records
    # referring to the same sample (part 1 and part 2), i.e.,
    # "LWF__65_a__Friedergries__NA" OFH_carbon.
    # Combine these into one record (if not done yet in the reviewed
    # sample list).


    # In case the partner used some code for the plot instead of sample_id
    # in column sample_id: combine with column sample_code

    # To do so, first make some auxiliary column by combining sample_id and
    # sample_code, and check whether it has more unique values than
    # sample_id

    df <- df %>%
      mutate(
        sample_id_aux = paste(sample_id, sample_code_harm, sep = "_"),
        sample_id_harmonised_aux =
          paste(composed_site_id, sample_code_harm, sep = "_"))

    if (n_distinct(df$sample_id_aux) > n_distinct(df$sample_id) &&
        !(n_distinct(df$sample_id) == 1 && is.na(unique(df$sample_id)))) {

      assertthat::assert_that(
        any(df$composed_site_id == "WULS__1__Bialowieza National Park__NA") ||
        n_distinct(df$sample_id_harmonised_aux) == n_distinct(df$sample_id_aux))

      df <- df %>%
        mutate(
          sample_id = sample_id_aux)
    }


    # Select and reorder columns
    df <- df %>%
      select(-sample_id_aux, -sample_id_harmonised_aux) %>%
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
        wp = case_when(
          grepl("Extra samples", wp) ~ "WP2",
          !is.na(wp) ~ wp,
          shipment_type == "normal" ~ "WP2",
          TRUE ~ NA)) %>%
      relocate(wp, .after = sub_id)


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


  all_samples_final <- all_samples %>%
    separate_wider_delim(
      composed_site_id,
      delim = "__",
      names = c("institute_harm", "res_id_inst_harm", "reserve_name",
                "sub_id_harm"),
      too_few = "align_start",
      cols_remove = FALSE
    ) %>%
    left_join(
      bind_rows(his_plot_ids %>%
                  distinct(composed_site_id, .keep_all = TRUE),
                wp3_plot_ids),
      by = join_by("composed_site_id", "wp")) %>%
    mutate(
      # Make sure that the transect of Bialowieza is mentioned in sample_id!
      plot_id_harm = ifelse(
        composed_site_id == "WULS__1__Bialowieza National Park__NA",
        case_when(
          grepl("Transect V$", sample_id, ignore.case = TRUE) ~
            "Transect V",
          grepl("Transect IV$", sample_id, ignore.case = TRUE) ~
            "Transect IV",
          grepl("Transect III$", sample_id, ignore.case = TRUE) ~
            "Transect III",
          grepl("Transect II$", sample_id, ignore.case = TRUE) ~
            "Transect II"),
        plot_id_harm),
      plot_code_simple = ifelse(
        composed_site_id == "WULS__1__Bialowieza National Park__NA",
        # We know now that plot_id_harm contains the correct transect for
        # Bialowieza
        paste(composed_site_id, plot_id_harm, sep = "__"),
        composed_site_id)) %>%
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
    rowwise() %>%
    mutate(
      # sample_id_simple:
      # A sample_id without plot_id to avoid issues with late updates of
      # plot_id, and without country code since redundant and possible source
      # of issues (based on plot_code_simple)
      # Example:
      # INBO__11__NA__OFH_carbon
      # WULS__1__NA__Transect II__M01_Bulk_Density
      sample_id_simple = map_chr(plot_code_simple, ~{
        parts <- str_split(.x, fixed("__"))[[1]]
        if (length(parts) >= 3) {
          paste(c(parts[1:2], parts[-(1:3)], sample_code_harm), collapse = "__")
        } else {
          .x
        }
      })) %>%
    ungroup() %>%
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






  return(all_samples_final)



}
