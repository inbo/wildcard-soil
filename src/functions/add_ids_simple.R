
add_ids_simple <- function(data_frame,
                           # orig_harm indicates whether the original
                           # sample_id column was harmonised (e.g., WBL)
                           # or not (e.g. "diepvries")
                           orig_harm = TRUE) {

  # This function adds five columns:
  # sample_id_simple, plot_code_simple and institute_sampling,
  # sample_code and code_layer

  # sample_id_simple:
  # A sample_id without plot_id to avoid issues with late updates of
  # plot_id, and without country code since redundant and a possible source
  # of issues

    # Example:
    # INBO__11__NA__M01_Bulk_Density
    # UNIUD__2__BF-OG__OFH_eDNA1
    # WULS__1__NA__Transect II__OFH_carbon

  # plot_code_simple
  # A unique code for each plot,
  # i.e., composed_site_id for non-Bialowieza plots and
  # composed_site_id with plot_id (transect) attached, for Bialowieza plots

    # Example:
    # INBO__11__Bos Terrijst__NA
    # UNIUD__2__Val Alba__BF-OG
    # WULS__1__Bialowieza National Park__NA__Transect II

  # institute_sampling
  # Institute that does the soil sampling

  # sample_code
  # Harmonised sample code following app
  # (OFH_carbon, M01_Bulk_Density, eDNA1 etc)
  # If any column sample_code exists already (in case data_frame$sample_id
  # is unharmonised), this new column replaces both sample_code and
  # sample_code_harm.

  # code_layer
  # Code for the layer depth: OL, OFH, M01, M13, M36, M61



  # Make sure that the column "sample_id" is available
  assertthat::assert_that("sample_id" %in% names(data_frame),
                          msg = "There must be a column 'sample_id'")


  # The original columns sample_id usually have the following format:
  # 1. in WBLs, RFs (LIMS): harmonised by central lab
  #    "IT__UNIUD__2__BF-OG__NA__OFH_carbon"
  #    "BE-INBO-11-NA-111000-M01_carbon"
  #    (note: for INBO records, "-" instead of "__" was used as separator)
  # 2. in 'diepvries' (results eDNA), data local lab:
  #    as provided by partner (not harmonised by central lab)
  # 	 "IT-UNIUD-Val_Alba-BF-OG-2-OFH_eDNA1"
  # 	 "BE-INBO-11-NA-111000-M01_eDNA1"


# Original sample_id harmonised ----


  if (orig_harm == TRUE) {

    # Here, everything is based on column "sample_id". No other columns needed.

    assertthat::assert_that(all(!is.na(data_frame$sample_id)))

    # Make sure that harmonised codes follow the theoretical formats
    # (six parts after splitting)

    inbo_sample_ids <-
      data_frame$sample_id[which(grepl("^BE-INBO-", data_frame$sample_id))]
    partner_sample_ids <-
      data_frame$sample_id[which(!grepl("^BE-INBO-", data_frame$sample_id))]

    assertthat::assert_that(
      identical(inbo_sample_ids, character(0)) ||
        all(str_count(inbo_sample_ids, "-") == 5),
      msg = "There must be five '-' in 'sample_id' for INBO records")

    assertthat::assert_that(
      identical(partner_sample_ids, character(0)) ||
        all(str_count(partner_sample_ids, "__") == 5),
      msg = "There must be five '__' in 'sample_id' for partner records")

    # For the Belgian samples, there is no sample list, and we need to get
    # the composed_site_id from:

    plots_belgium <- read.csv("./data/fieldwork_belgium/plots_belgium.csv",
                              sep = ";") %>%
      mutate(
        plot_code = paste(institute, res_id_inst, sub_id, plot_id,
                          sep = "-"),
        composed_site_id = paste(institute, res_id_inst, reserve_name, sub_id,
                                 sep = "__"),
        plot_code_simple = composed_site_id) %>%
      select(plot_code, plot_code_simple, institute)

    # Get samples (with sample lists). We will use this to get plot_code_simple

    if (!exists("samples", .GlobalEnv)) {

      source("./src/functions/get_sample_lists.R")
      samples <- get_sample_lists()

    }




    data_frame_added <- data_frame %>%
      mutate(
        # Get the last part of sample_id based on "-" or "__" as separator
        sample_code = case_when(
          grepl("^BE-INBO-", sample_id) ~ str_extract(sample_id, "[^\\-]+$"),
          TRUE ~ str_extract(sample_id, "(?<=__)(?!.*__).+$")),
        code_layer = str_extract(sample_code, "^[^_-]+"),
        # Make sample_id_simple that excludes plot_id:
        # 1. remove plot_id, except for Bialowieza
        sample_id_simple = case_when(
          grepl("WULS__1__", sample_id) ~ sample_id,
          grepl("^BE-INBO-", sample_id) ~
            str_replace(sample_id, "-[^-]+-(?=[^-]+$)", "-"),
          TRUE ~ str_replace(sample_id,
                             "^(.*)__[^_]+__(.+)$",
                             "\\1__\\2")),
        # 2. Now update the INBO records so that they have "__" as separator
        sample_id_simple = ifelse(
          str_starts(sample_id_simple, "BE-INBO"),
          str_replace_all(sample_id_simple, "-", "__"),
          sample_id_simple),
        # 3. Remove country code
        sample_id_simple = str_remove(sample_id_simple, "^[^_]+__"),
        # Add plot_code for INBO records, which is necessary to link with
        # plots_belgium (in order to make plot_code_simple)
        plot_code = ifelse(
          str_starts(sample_id_simple, "INBO"),
          str_extract(sample_id, "(?<=-)[^-]+(?:-[^-]+)*-[^-]+(?=-)"),
          NA_character_)) %>%
      # Join "samples" (to have plot_code_simple)
      # This will only work for non-INBO records
      left_join(
        samples %>%
          select(sample_id_simple, plot_code_simple, institute_sampling),
        by = "sample_id_simple") %>%
      # Join plots_belgium for INBO plots
      left_join(
        plots_belgium %>%
          rename(plot_code_simple_be = plot_code_simple),
        by = "plot_code") %>%
      mutate(
        plot_code_simple = coalesce(plot_code_simple,
                                    plot_code_simple_be),
        institute_sampling = coalesce(institute_sampling,
                                      institute)) %>%
      select(-plot_code_simple_be, -institute, -plot_code)

  } # End of "orig_harm == TRUE"








# Original sample_id unharmonised ----


  if (orig_harm == FALSE) {

    # Here, we largely ignore the column sample_id (since unharmonised),
    # but assume that there are columns 'composed_site_id' and
    # 'sample_code_harm'. However, for Bialowieza, the transect needs to
    # be mentioned in column sample_id!

    assertthat::assert_that(
      "composed_site_id" %in% names(data_frame),
      msg = "Columns 'composed_site_id' and 'sample_code_harm' must both exist")

    assertthat::assert_that(
      "sample_code_harm" %in% names(data_frame) ||
        # For forest floor data local lab:
        ("sample_code" %in% names(data_frame) &&
           all(!is.na(data_frame$sample_code)) &&
           all(data_frame$sample_code %in% c("OL", "OFH"))),
      msg = "Columns 'composed_site_id' and 'sample_code_harm' must both exist")


    # For forest floor data local lab:

    if (!"sample_code_harm" %in% names(data_frame)) {

      data_frame_added <- data_frame %>%
        rename(code_layer = sample_code) %>%
        mutate(
          sample_code = case_when(
            code_layer == "OL" ~ "OL_flammability",
            code_layer == "OFH" ~ "OFH_carbon")) %>%
        relocate(sample_code, .before = code_layer)


      # For other dataframes with originally unharmonised sample_id
      # (e.g. "diepvries"):

    } else {

      data_frame_added <- data_frame %>%
        # Remove any existing column sample_code
        select(-any_of(c("sample_code"))) %>%
        rename(sample_code = sample_code_harm) %>%
        mutate(
          code_layer = str_extract(sample_code, "^[^_-]+")) %>%
        relocate(sample_code, .before = code_layer)

    }

    data_frame_added <- data_frame_added %>%
      separate_wider_delim(
        composed_site_id,
        delim = "__",
        names = c("institute_harm", "res_id_inst_harm", "reserve_name",
                  "sub_id_harm"),
        too_few = "align_start",
        cols_remove = FALSE
      ) %>%
      mutate(
        # Make sure that the transect of Bialowieza is mentioned in sample_id!
        plot_code_simple = ifelse(
          composed_site_id == "WULS__1__Bialowieza National Park__NA",
          case_when(
            grepl("Transect V$", sample_id, ignore.case = TRUE) ~
              paste(composed_site_id, "Transect V", sep = "__"),
            grepl("Transect IV$", sample_id, ignore.case = TRUE) ~
              paste(composed_site_id, "Transect IV", sep = "__"),
            grepl("Transect III$", sample_id, ignore.case = TRUE) ~
              paste(composed_site_id, "Transect III", sep = "__"),
            grepl("Transect II$", sample_id, ignore.case = TRUE) ~
              paste(composed_site_id, "Transect II", sep = "__")),
          composed_site_id)) %>%
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
            paste(c(parts[1:2], parts[-(1:3)], sample_code),
                  collapse = "__")
          } else {
            .x
          }
        })) %>%
      ungroup() %>%
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
          TRUE ~ institute_harm)) %>%
      select(-any_of(c("institute_harm", "res_id_inst_harm", "reserve_name",
                       "sub_id_harm")))


  } # End of "orig_harm == FALSE"



  return(data_frame_added)





}
