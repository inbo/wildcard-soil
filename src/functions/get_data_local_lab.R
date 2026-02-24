
get_data_local_lab <- function() {

  # Compiles forest floor masses from the local lab

  # The assumption is that composed_site_id is added at least for WP2 records

  # Only for external partners, not for INBO

  stopifnot(require("tidyverse"),
            require("googledrive"),
            require("googlesheets4"),
            require("readxl"),
            require("purrr"),
            require("parsedate"))


  # Source URLs of sensitive Google Drive links
  source("./data/sensitive_metadata/google_drive_links.R")

  # Get the list of files (harmonised sample lists) on Google Drive

  files_local_lab <- drive_ls(as_id(id_ff_local)) %>%
    # Filter only .xlsx files or Google Sheets
    filter(
      grepl("\\.xlsx$", name, ignore.case = TRUE) |
        map_chr(drive_resource,
                ~ .x$mimeType) == "application/vnd.google-apps.spreadsheet")


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


    assertthat::assert_that(
      all(!is.na(df$sample_code)),
      msg = paste0("Please make sure that column sample_code is filled in,",
                   " and is either 'OL' or 'OFH'. Missing info in file '",
                   file$name, "'."))

    assertthat::assert_that(
      all(df$sample_code %in% c("OL", "OFH")),
      msg = paste0("Please make sure that column sample_code is either 'OL' ",
                   "or 'OFH'. Unharmonised info in file '",
                   file$name, "'."))

    columns_to_check <- c(
      "mass_dry_ofh", "mass_dry_leaves_ol",
      "mass_dry_twigs_medium_ol", "mass_dry_twigs_small_ol")

    assertthat::assert_that(
      all(columns_to_check %in% names(df)),
      msg = paste0("All columns 'mass_dry_ofh', 'mass_dry_leaves_ol', ",
      "'mass_dry_twigs_medium_ol', and 'mass_dry_twigs_small_ol' must",
      " exist. This is not the case in file '", file$name, "'."))

    df <- df %>%
      mutate(across(starts_with("mass_dry_"), as.character)) %>%
      select(sample_id, composed_site_id, sample_code,
             starts_with("mass_dry_"))

    return(df)
  } # End of read_and_standardise



  # Read and combine all files

  all_local_lab <- map_dfr(seq_len(nrow(files_local_lab)), function(i) {
    read_and_standardise(files_local_lab[i, ])
  })


  source("./src/functions/add_ids_simple.R")

  all_local_lab <- all_local_lab %>%
    # Assumption: records without composed_site_id not needed (WP3)
    filter(!is.na(composed_site_id)) %>%
    # Add extra columns
    add_ids_simple(orig_harm = FALSE)


  # Remove files from today from temporary directory

  files_temp <- list.files(tempdir(), full.names = TRUE)

  recent_files_temp <-
    files_temp[file.info(files_temp)$mtime >=
                 (Sys.time() - as.difftime(10, units = "mins"))]

  if (!identical(recent_files_temp, character(0))) {
    unlink(recent_files_temp, recursive = TRUE)
  }



  # INCONSISTENCY 1 ----

  rule <- "Data expected to be numeric should be numeric."
  rule_id <- "1"

  name_export <- "inconsistencies_local_lab_numeric"

  # These columns should be numeric:
  columns_to_check <- c(
    "mass_dry_ofh",
    "mass_dry_leaves_ol", "mass_dry_twigs_medium_ol", "mass_dry_twigs_small_ol")

  # Dataframe to collect problems
  inconsistencies_local_lab_numeric <- data.frame(
    source = character(0),
    team = character(0),
    plot_code_app = character(0),
    res_id_inst = character(0),
    globalid = character(0),
    parameter = character(0),
    parameter_description = character(0),
    parameter_unit = character(0),
    sample_code = character(0),
    value = character(0),
    inconsistency_reason = character(0),
    unique_inconsistency = logical(0),
    rule_id = character(0))

  # Loop through rows and columns
  for (i in seq_len(nrow(all_local_lab))) {
    for (col in columns_to_check) {
      val <- all_local_lab[[col]][i]

      if (!is.na(val)) {
        # Convert comma decimal to dot
        val_clean <- gsub(",", ".", as.character(val))

        # Try converting to numeric
        # Returns NULL if it doesn't give problems
        result <- tryCatch({
          as.numeric(val_clean)
          NULL  # no error
        }, warning = function(w) {
          return(val_clean)
        }, error = function(e) {
          return(val_clean)
        })

        if (!is.null(result)) {
          inconsistencies_local_lab_numeric <-
            rbind(inconsistencies_local_lab_numeric, data.frame(
              source = "Forest floor data local lab",
              team = all_local_lab$institute_sampling[i],
              plot_code_app = all_local_lab$plot_code_simple[i],
              res_id_inst = sub(".*?__(.*?)__.*", "\\1",
                                all_local_lab$plot_code_simple[i]),
              globalid = NA_character_,
              sample_code = case_when(
                grepl("ofh$", col) ~ "OFH",
                grepl("leaves_ol$", col) ~ "OL-leaves",
                grepl("twigs_medium_ol$", col) ~ "OL-twigs-medium",
                grepl("twigs_small_ol$", col) ~ "OL-twigs-small"),
              parameter = col,
              parameter_description = paste0("Net mass of dry subsample ",
                                             "from the given forest floor ",
                                             "fraction from the local lab"),
              parameter_unit = "g",
              value = as.character(val),
              inconsistency_reason = rule,
              unique_inconsistency = TRUE,
              rule_id = rule_id))
        }
      }
    }
  }


  assign(name_export, inconsistencies_local_lab_numeric, envir = .GlobalEnv)
  cat(paste0("Object '", name_export, "' is imported in global environment.\n"))



  # If there are any problems with non-numeric values, correct them here

  all_local_lab <- all_local_lab %>%
    mutate(across(all_of(columns_to_check),
      ~ as.numeric(gsub(",", ".", .x))
    ))

  # Add a record for the newly taken sample of LWF__65_a__Friedergries__NA OFH

  all_local_lab <- bind_rows(
    all_local_lab,
    all_local_lab %>%
      filter(sample_id_simple == "LWF__65_a__NA__OFH_carbon") %>%
      mutate(
        sample_id_simple = "LWF__65_a__NA__OFH_carbon_b",
        mass_dry_ofh = 1150))


  return(all_local_lab)



}
