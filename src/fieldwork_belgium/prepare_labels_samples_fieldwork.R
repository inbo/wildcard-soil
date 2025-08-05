
# R script for planning and processing Belgian (INBO) field work data
# -------------------------------------------------------------------

# This script does the following:
# · Format output to paste in Google Calendar (calendar items for sampling)
# · Generate sample labels for Belgian field work
# · Format sample lists to register Belgian eDNA samples in the freezer sample
#   registration spreadsheet
# · WP3: Generate forest_floor_data
# · WP3: Generate normal_shipment_samples_list
#



# TO DO HELEEN: place handover checklist-related and sample pretreatment-related
# code in another script.



stopifnot(require("tidyverse"),
          require("ggplot2"),
          require("gridExtra"),
          require("grid"),
          require("googlesheets4"),
          require("googledrive"),
          require("readxl"),
          require("purrr"),
          require("sf"),
          require("glue"),
          require("openxlsx"))

# Source URLs of sensitive Google Drive links
source("./data/sensitive_metadata/google_drive_links.R")



# List unique plot codes ----

# HIS metadata table (HIS for WP2 soil sampling)

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
  mutate(across(where(is.list), ~ map_chr(., ~ ifelse(is.null(.),
                                                      NA_character_,
                                                      as.character(.)))))


# Save table

his_output <- his %>%
  select(composed_site_id,
         institute,
         res_id_inst,
         reserve_name,
         sub_id,
         latitude,
         longitude,
         his_selected,
         his_soil_sampling,
         his_soil_water,
         his_soil_nutrient,
         confirmed_his_soil,
         his_tsaclass,
         his_eftc,
         eea_fortype,
         derived_tsa,
         year_reserve,
         year_abandonment,
         his_soil_pit_permission,
         his_soil_core_permission,
         contact,
         change_date)

write.table(his_output,
            file = paste0("./data/his_", format(Sys.Date(), "%Y%m%d"), ".csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")



# Belgian WP2 HIS soil sampling records

his_be <- his %>%
  filter(grepl("inbo", institute, ignore.case = TRUE))


# WP3 sites

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
  mutate(
    latitude = case_when(
      latitude %in% c("", " ", NA) ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(latitude))),
    longitude = case_when(
      longitude %in% c("", " ", NA) ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(longitude))))

wp3_sites_be <- wp3_sites %>%
  filter(grepl("INBO", institute, ignore.case = TRUE)) %>%
  select(institute, region, chronosequence_id, site_id, plot_id,
         year_abandonment, his_soil_water, his_eftc,
         latitude, longitude)


# Table with field work Belgium

file_path <- "./data/fieldwork_belgium/planning_sampling_inbo.xlsx"

# Download Excel file on Google Drive to that specific folder
drive_download(as_id(id_be),
               path = file_path,
               overwrite = TRUE)

# Coordinates Everbeekse bossen for gap-filling

plots_belgium <- read_excel(file_path,
                            range = "A1:N100",
                            sheet = 1) %>%
  rename_with(tolower) %>%
  rename(reserve_name = reservaat,
         wrb_ref_soil_group = `wrb ref soil group`,
         wrb_qualifier_1 = `wrb qualifier 1`,
         date = datum)

coord_g2 <- plots_belgium %>%
  filter(grepl("Everbeekse", reserve_name) & sub_id == "G2") %>%
  select(longitude, latitude)

coord_a2 <- plots_belgium %>%
  filter(grepl("Everbeekse", reserve_name) & sub_id == "A2") %>%
  select(longitude, latitude)



plots_belgium <- read_excel(file_path,
                            range = "A1:N100",
                            sheet = 1) %>%
  rename_with(tolower) %>%
  rename(reserve_name = reservaat,
         wrb_ref_soil_group = `wrb ref soil group`,
         wrb_qualifier_1 = `wrb qualifier 1`,
         date = datum) %>%
  filter(!grepl("Staalvoorbereiding", reserve_name, ignore.case = TRUE) &
           !is.na(reserve_name)) %>%
  # Remove trailing .0 from all entries in the plot_id column
  # (because of numeric IDs getting auto-converted to floats)
  mutate(plot_id = gsub("\\.0$", "", plot_id)) %>%
  select(date, reserve_name, sub_id, plot_id, longitude, latitude,
         wrb_ref_soil_group, wrb_qualifier_1) %>%
  # Gap-fill coordinates of Everbeekse bossen
  # (G0 and A0 are immediately next to G2 and A2, respectively)
  mutate(
    longitude = case_when(
      sub_id == "G0" ~ coord_g2$longitude,
      sub_id == "A0" ~ coord_a2$longitude,
      TRUE ~ longitude),
    latitude = case_when(
      sub_id == "G0" ~ coord_g2$latitude,
      sub_id == "A0" ~ coord_a2$latitude,
      TRUE ~ latitude)) %>%
  mutate(country_code = "BE") %>% # Belgium
  # Add WP2 data
  left_join(his_be %>%
              select(reserve_name, sub_id, res_id_inst, institute,
                     # latitude, longitude,
                     his_soil_water, his_soil_nutrient, his_eftc, his_tsaclass,
                     year_abandonment),
            by = c("reserve_name", "sub_id")) %>%
  # Add WP3 data
  left_join(wp3_sites_be %>%
              rename(reserve_name = region,
                     sub_id = site_id) %>%
              mutate(
                year_abandonment = case_when(
                  grepl("Not abandoned", year_abandonment,
                        ignore.case = TRUE) ~ 3000,
                  TRUE ~ suppressWarnings(as.numeric(year_abandonment)))) %>%
              select(reserve_name, sub_id, chronosequence_id, institute,
                     his_soil_water, his_eftc,
                     year_abandonment) %>%
              rename_with(~ paste0("wp3_", .x),
                          .cols = -c(reserve_name, sub_id)),
            by = c("reserve_name", "sub_id")) %>%
  mutate(
    res_id_inst = coalesce(res_id_inst,
                           wp3_chronosequence_id),
    institute = coalesce(institute,
                         wp3_institute),
    his_soil_water = coalesce(his_soil_water,
                              wp3_his_soil_water),
    his_eftc = coalesce(his_eftc,
                        wp3_his_eftc),
    year_abandonment = coalesce(year_abandonment,
                                wp3_year_abandonment)) %>%
  select(-starts_with("wp3_")) %>%
  mutate(his_soil_water = case_when(
    grepl("Wet", his_soil_water, ignore.case = TRUE) ~ "Wet",
    grepl("Mesic", his_soil_water, ignore.case = TRUE) ~ "Mesic")) %>%
  mutate(wp = case_when(
    grepl("Everbeekse", reserve_name) ~ "WILDCARD WP3",
    TRUE ~ "WILDCARD WP2")) %>%
  mutate(
    plot_code = paste0(institute, "__",
                       res_id_inst, "__",
                       sub_id, "__",
                       plot_id),
    plot_code_app = paste0(institute, "-",
                       res_id_inst, "-",
                       sub_id, "-",
                       plot_id)) %>%
  relocate(country_code, institute, wp, .before = date) %>%
  relocate(wp, plot_code, .after = institute) %>%
  relocate(res_id_inst, .after = reserve_name)


# Add parent material

source("./src/functions/overlay_tif.R")
source("./src/functions/as_sf.R")

par_mat_dom1_dictionary <-
  read.csv(paste0("./data/additional_data/shapefiles/",
                  "European Soil Database/",
                  "par_mat_dom1_dictionary.csv"),
           sep = ";") %>%
  mutate(par_mat_dom1 = str_to_sentence(par_mat_dom1))


plots_belgium <- plots_belgium %>%
  rename(latitude_dec = latitude,
         longitude_dec = longitude) %>%
  overlay_tif(path_tif = paste0("./data/additional_data/shapefiles/",
                                "European Soil Database/",
                                "parmado1.tif")) %>%
  st_drop_geometry() %>%
  left_join(par_mat_dom1_dictionary %>%
              rename(parmado1 = PAR.MAT.DOM1) %>%
              rename(parent_material = par_mat_dom1) %>%
              select(parmado1, parent_material),
            by = "parmado1") %>%
  select(-parmado1)


plots_belgium <- plots_belgium %>%
  relocate(year_abandonment, his_tsaclass, .after = parent_material)


# Save table

write.table(plots_belgium,
            file = "./data/fieldwork_belgium/plots_belgium.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")



# 1. Format output for Google calendar ----

ind <- 21

formatted_output <- plots_belgium %>%
  select(
    reserve_name,
    longitude_dec,
    latitude_dec,
    res_id_inst,
    sub_id,
    plot_id,
    plot_code,
    wp,
    wrb_ref_soil_group,
    wrb_qualifier_1,
    year_abandonment,
    his_soil_nutrient,
    his_soil_water,
    his_eftc,
    parent_material)

formatted_output <- formatted_output %>%
  slice(ind) %>%
  pmap_chr(~ {
    vals <- list(...)
    names(vals) <- names(formatted_output)
    glue_collapse(glue("{names(vals)}: {vals}"), sep = "\n")
  })

cat(formatted_output)

cat(plots_belgium$latitude_dec[ind],
    plots_belgium$longitude_dec[ind])





# 2. Checklist handover draft ----

# Vector of new column names
new_cols <- c("OFH", "OFH-eDNA1", "OFH-eDNA2", "M01", "M13", "M36", "M61",
              "BD-M01", "BD-M13", "BD-M36", "BD-M61", "M01-eDNA1", "M01-eDNA2")

checklist_draft <- plots_belgium %>%
  filter(grepl("WP2", wp)) %>%
  mutate(
    contractor = "INBO",
    plot_code = paste0(institute, "__",
                       res_id_inst, "__",
                       sub_id, "__",
                       plot_id)) %>%
  select(contractor, plot_code) %>%
  mutate(!!!setNames(rep(list(NA), length(new_cols)), new_cols)) %>%
  mutate(
    `Data Survey123 app` = NA,
    `Data local lab` = NA)


write.table(checklist_draft,
            file = paste0("./data/fieldwork_belgium/",
                          "wp2_soil_sampling_handover_checklist_draft.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")



# 3. Checklist handover final ----

# Vector of new column names
new_cols <- c("OFH", "M01", "M13", "M36", "M61",
              "M01-BD", "M13-BD", "M36-BD", "M61-BD",
              "OFH-eDNA1", "OFH-eDNA2", "M01-eDNA1", "M01-eDNA2")

# Lists with High Intensity Sites per external contractor
# (attachments to the service contract)

his_lists_gd <- drive_ls(as_id(id_lists), type = "xlsx") %>%
  mutate(contractor = sub("_.*", "", name))

# Create a function to download and read one file

read_his_lists <- function(file_id, contractor) {

  # Download the file to a temp location

  temp_file <- tempfile(fileext = ".xlsx")

  drive_download(as_id(file_id),
                 path = temp_file, overwrite = TRUE, type = "xlsx")

  # Read the excel sheet on Google Drive

  df <- read_excel(temp_file)

  # Remove recent files from temporary location

  files_temp <- list.files(tempdir(), full.names = TRUE)

  recent_files_temp <-
    files_temp[file.info(files_temp)$mtime >=
                 (Sys.time() - as.difftime(10, units = "mins"))]

  if (!identical(recent_files_temp, character(0))) {
    unlink(recent_files_temp, recursive = TRUE)
  }

  # Add contractor column

  df <- df %>%
    filter(!is.na(composed_site_id)) %>%
    mutate(contractor = contractor) %>%
    relocate(contractor, .before = composed_site_id) %>%
    mutate(res_ID_inst = as.character(res_ID_inst)) %>%
    rename(
      payment = any_of("Payment"),
      institute = any_of(grep("^institute", names(.), value = TRUE)))

  return(df)
}

# Use map2 to loop through all files and contractor names
his_external <- map2_dfr(
  his_lists_gd$id,
  his_lists_gd$contractor,
  read_his_lists
)


# Create workbook

checklist_all <- his_external %>%
  select(contractor, composed_site_id) %>%
  mutate(!!!setNames(rep(list(NA), length(new_cols)), new_cols)) %>%
  mutate(
    `Data Survey123 app` = NA,
    `Data local lab` = NA)


wb <- createWorkbook()

for (i in seq_along(unique(his_external$contractor))) {

  addWorksheet(wb, unique(his_external$contractor)[i])

  checklist_i <- checklist_all %>%
    filter(contractor == unique(his_external$contractor)[i])

  writeData(wb, i, checklist_i)
  dataValidation(wb, i, col = which(names(checklist_i) %in% new_cols),
                 rows = 2:(nrow(checklist_i) + 1),
                 type = "list",
                 value = '"1 - Received correctly, 2 - Not received - valid absence (sampling not possible), 3 - Not received correctly - other valid reasons"') # nolint
  dataValidation(wb, i, col = which(grepl("^Data", names(checklist_i))),
                 rows = 2:(nrow(checklist_i) + 1),
                 type = "list",
                 value = '"1 - Received correctly, 2 - Received with known minor issues, 3 - Not received - valid reason"') # nolint
  freezePane(wb, i, firstActiveRow = 2, firstActiveCol = 1)

}

saveWorkbook(wb,
             file = paste0("./output/sample_reception_admin/",
                           "wp2_soil_sampling_handover_checklist.xlsx"),
             overwrite = TRUE)







# 4. Labels fieldwork ----

## List unique sample codes following Survey123 app ----

sample_codes <- data.frame(
  sample_code =
    c(
      "OL_flammability",
      "OFH_carbon",
      "OFH_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "OFH_eDNA1",
      "OFH_eDNA2",
      "OFH_eDNA", # not foreseen in app!!
      "M01_carbon",
      "M01_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "M01_eDNA1",
      "M01_eDNA2",
      "M01_eDNA", # not foreseen in app!!
      "M13_carbon",
      "M13_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "M36_carbon",
      "M36_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "M61_carbon",
      "M61_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "M01_Bulk_density",
      "M13_Bulk_density",
      "M36_Bulk_density",
      "M61_Bulk_density"))


# Save table

write.table(sample_codes,
            file = "./data/fieldwork_belgium/sample_codes.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")






## Expand dataframe with all combinations ----
# plot_code x sample_code (double)

# Extra back-up samples for WP3

data <-
  # All combinations should appear twice since we use double labelling
  bind_rows(
    expand.grid(
      plot_code = plots_belgium$plot_code,
      sample_code = sample_codes$sample_code[
        which(!grepl("BackUp", sample_codes$sample_code))]),
    expand.grid(
      plot_code = plots_belgium$plot_code,
      sample_code = sample_codes$sample_code[
        which(!grepl("BackUp", sample_codes$sample_code))]),
    # Use a back-up for WP3 samples for carbon
    expand.grid(
      plot_code =
        plots_belgium$plot_code[which(grepl("WP3", plots_belgium$wp))],
      sample_code = sample_codes$sample_code[
        which(grepl("BackUp", sample_codes$sample_code))]),
    expand.grid(
      plot_code =
        plots_belgium$plot_code[which(grepl("WP3", plots_belgium$wp))],
      sample_code = sample_codes$sample_code[
        which(grepl("BackUp", sample_codes$sample_code))])) %>%
  # Sort the columns following the order specified in the original lists
  mutate(plot_code = factor(plot_code, levels = plots_belgium$plot_code)) %>%
  mutate(sample_code = factor(sample_code,
                              levels = sample_codes$sample_code)) %>%
  arrange(plot_code, sample_code) %>%
  left_join(
    plots_belgium %>%
      select(plot_code, wp, reserve_name, date),
    by = "plot_code") %>%
  filter(date >= "2025-06-25") # &
         #  date <= "2025-06-05")
  # filter(grepl("__A2__", plot_code) |
  #          grepl("__A0__", plot_code) |
  #          reserve_name == "Wijnendalebos")
   # filter(date <= "2025-05-08")





## Create stickers ----

# Function to create one sticker

make_label <- function(plot_code, reserve_name, sample_code, wp, date) {

  ggplot() +
    theme_void() +
    xlim(0, 1) +  # Define x-axis limits
    ylim(0, 1) +  # Define y-axis limits
    annotate("text", hjust = 0, x = 0.1, y = 0.7,
             label = sample_code,
             size = 2.5,
             fontface = "bold") +
    annotate("text", hjust = 0, x = 0.1, y = 0.6,
             label = plot_code,
             size = 2.5) +
    annotate("text", hjust = 0, x = 0.1, y = 0.5,
             label = reserve_name,
             size = 2.5) +
    annotate("text", hjust = 0, x = 0.1, y = 0.4,
             label = date,
             size = 2.5) +
    annotate("text", hjust = 0, x = 0.1, y = 0.3,
             label = wp,
             size = 2.5) +
    theme(plot.margin = margin(t = 2, r = 2, b = 2, l = 2,
                               "mm"))
}

# Apply function to create all labels

labels <- pmap(data, make_label)



## Position labels on label pages ----

# Define how many stickers per page

labels_per_page <- 24

pages <- split(labels,
               ceiling(seq_along(labels) / labels_per_page))


# Export to PDF

pdf("./output/fieldwork_belgium/labels_15-21.pdf",
    width = 8.27,
    height = 11.69) # A4 size in inches

for (page in pages) {
  grid.arrange(grobs = page,
               ncol = 3,
               nrow = 8) # 3 cols x 8 rows
}

dev.off()



# 5. Labels sample pretreatment ----

his_ids <- read_excel("./data/additional_data/his_ids.xlsx")



sample_codes_pretrt <- data.frame(
  sample_code =
    c("OL_leaves", # Only for Belgian samples
      "OL_leaves", # Only for Belgian samples
      "OL_twigs_medium", # Only for Belgian samples
      "OL_twigs_small", # Only for Belgian samples
      "OFH_carbon",
      "OFH_carbon",
      "OFH_BackUp",
      "M01_carbon",
      "M01_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "M13_carbon",
      "M13_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "M36_carbon",
      "M36_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "M61_carbon",
      "M61_BackUp", # only needed for WP3 in Belgian soil sampling campaign
      "  ",
      "  ",
      "M01_Bulk_density",
      "M13_Bulk_density",
      "M36_Bulk_density",
      "M61_Bulk_density"
      )) %>%
  mutate(
    prefix = case_when(
      grepl("^OL", sample_code) ~ "C",
      grepl("BackUp$", sample_code) ~ "C",
      grepl("carbon$", sample_code) ~ "C",
      grepl("density$", sample_code) ~ "BD",
      TRUE ~ "C"
    ),
    suffix = case_when(
      grepl("^OL", sample_code) ~ sample_code,
      grepl("BackUp$", sample_code) ~ sample_code,
      grepl("carbon$", sample_code) ~ sub("_.*", "", sample_code),
      grepl("density$", sample_code) ~ sub("_.*", "", sample_code),
      TRUE ~ "   "
    ))


## Expand dataframe with all combinations ----

data <-
  bind_rows(
    # Other countries excl. OL and excl. BackUp samples
    expand.grid(
      plot_code =
        his_ids$plot_code[which(his_ids$institute != "INBO")],
      sample_code =
        sample_codes_pretrt$sample_code[
          which(!grepl("BackUp$", sample_codes_pretrt$sample_code) &
                  !grepl("^OL", sample_codes_pretrt$sample_code))])) %>%
  left_join(
    his_ids %>%
      select(plot_code, composed_site_id, id_sample_pretreatment,
             institute, res_id_inst, reserve_name, sub_id),
    by = "plot_code") %>%
  mutate(
    sub_id = coalesce(sub_id, "NA"),
    # Institute that does the soil sampling
    institute_sampling = case_when(
      institute %in% c("BFNP_EXTRA", "NPS") ~ "BFNP",
      institute == "LWF" & reserve_name == "Hoellbachgespreng" ~ "BFNP",
      institute == "DISAFA - UNITO" ~ "UNITO",
      institute == "FVA-BW" ~ "NWFVA",
      institute == "INCDS" ~ "UNITBV",
      institute %in% c("UNIUD/HSI", "UNIUD/HSI_EXTRA",
                       "UNIUD_EXTRA") ~ "UNIUD",
      institute %in% c("URK", "WULS") ~ "IBL",
      TRUE ~ institute)) %>%
  # left_join(
  #   plots_belgium %>%
  #     select(institute, res_id_inst, sub_id, plot_id, reserve_name),
  #   by = c("institute", "res_id_inst", "sub_id")) %>%
  # mutate(
  #   plot_code = case_when(
  #     composed_site_id == "INBO__INBO_2__A0__1" ~ composed_site_id,
  #     institute == "INBO" ~ paste0(institute, "__",
  #                                  res_id_inst, "__",
  #                                  sub_id, "__",
  #                                  plot_id),
  #     TRUE ~ paste0(institute, "__",
  #                      res_id_inst, "__",
  #                      sub_id, "__",
  #                      "??")),
    # plot_code_app = case_when(
    #   composed_site_id == "INBO__INBO_2__A0__1" ~ "INBO-INBO_2-A0-1",
    #   institute == "INBO" ~ paste0(institute, "-",
    #                                res_id_inst, "-",
    #                                sub_id, "-",
    #                                plot_id),
    #   TRUE ~ paste0(institute, "-",
    #                        res_id_inst, "-",
    #                        sub_id, "-",
    #                        "??")),
    # reserve_name = case_when(
    #   composed_site_id == "INBO__INBO_2__A0__1" ~ "Everbeekse bossen",
    #   TRUE ~ reserve_name),
    # institute = case_when(
    #   composed_site_id == "INBO__INBO_2__A0__1" ~ "INBO",
    #   TRUE ~ institute),
    # id_sample_pretreatment = case_when(
    #   composed_site_id == "INBO__INBO_2__A0__1" ~ 7,
    #   TRUE ~ id_sample_pretreatment)) %>%
  left_join(
    sample_codes_pretrt %>%
      distinct(sample_code,
               .keep_all = TRUE),
    by = "sample_code") %>%
  mutate(
    # sample_id = paste0(plot_code_app, "-", sample_code),
    staal_id_pretrt =
      paste0(prefix, "-",
             str_pad(id_sample_pretreatment, width = 3, pad = "0"), "-",
             suffix)) %>%
  # Sort the columns
  mutate(sample_code =
           factor(sample_code,
                  levels = unique(sample_codes_pretrt$sample_code))) %>%
  arrange(id_sample_pretreatment, sample_code)


## Create stickers ----

# Function to create one sticker

make_label_pretrt <- function(staal_id_pretrt, plot_code, reserve_name) {

  ggplot() +
    theme_void() +
    xlim(0, 1) +  # Define x-axis limits
    ylim(0, 1) +  # Define y-axis limits
    annotate("text", hjust = 0, x = 0.1, y = 0.65,
             label = staal_id_pretrt,
             fontface = "bold",
             size = 3) +
    annotate("text", hjust = 0, x = 0.1, y = 0.5,
             label = reserve_name,
             size = 2.5) +
    annotate("text", hjust = 0, x = 0.1, y = 0.4,
             label = plot_code,
             size = 2.5) +
    theme(plot.margin = margin(t = 2, r = 2, b = 2, l = 2,
                               "mm"))
}

## All labels in one pdf ----

# Apply function to create all labels

labels <- pmap(
  data %>%
    select(staal_id_pretrt, plot_code, reserve_name),
  make_label_pretrt)



## Position labels on label pages

# Define how many stickers per page

labels_per_page <- 24

pages <- split(labels,
               ceiling(seq_along(labels) / labels_per_page))


# Export to PDF

pdf("./output/sample_pretreatment/pretreatment_labels.pdf",
    width = 8.27,
    height = 11.69) # A4 size in inches

for (page in pages) {
  grid.arrange(grobs = page,
               ncol = 3,
               nrow = 8) # 3 cols x 8 rows
}

dev.off()



while (dev.cur() > 1) dev.off()



## Different pdfs per partner ----

for (inst in unique(data$institute_sampling)) {

  labels <- pmap(
    data %>%
      filter(institute_sampling == inst) %>%
      select(staal_id_pretrt, plot_code, reserve_name),
    make_label_pretrt)

  pages <- split(labels,
                 ceiling(seq_along(labels) / labels_per_page))


  # Export to PDF

  pdf(paste0("./output/sample_pretreatment/pretreatment_labels_",
             inst, ".pdf"),
      width = 8.27,
      height = 11.69) # A4 size in inches

  for (page in pages) {
    grid.arrange(grobs = page,
                 ncol = 3,
                 nrow = 8) # 3 cols x 8 rows
  }

  dev.off()

  while (dev.cur() > 1) dev.off()

} # End of "for loop over sampling institutes"










# 6. Format for sample list for freezer Belgium ----

be_list_freezer <- crossing(
  plots_belgium %>%
    filter(date > as.Date("2025-06-06")),
  sample_codes %>%
    filter(grepl("eDNA", sample_code))) %>%
  mutate(sample_code = factor(sample_code,
                              levels = sample_codes$sample_code)) %>%
  arrange(date, plot_code, sample_code) %>%
  mutate(
    sample_id = paste0(country_code, "-",
                       plot_code_app, "-",
                       sample_code),
    material = case_when(
      grepl("M01", sample_code) ~ "mineral",
      grepl("OFH", sample_code) ~ "forest floor"),
    mass_sample_gross = NA,
    country_origin = country_code,
    institute_origin = institute,
    wp = case_when(
      grepl("WP2", wp) ~ "WP2",
      grepl("WP3", wp) ~ "WP3"),
    date_survey = date,
    transfer_third_party = case_when(
      grepl("eDNA1$", sample_code, ignore.case = TRUE) ~ "TRUE",
      grepl("eDNA2$", sample_code, ignore.case = FALSE) ~ "FALSE",
      grepl("_eDNA$", sample_code, ignore.case = TRUE) ~ "FALSE"),
    date_shipment_departure = NA,
    shipment_company = "no shipment",
    date_shipment_arrival = NA,
    institute_receiving = "INBO",
    cold_at_sampling = TRUE,
    stored_frozen = TRUE,
    shipped_frozen = NA) %>%
  select(
    sample_id,
    sample_code,
    material,
    mass_sample_gross,
    plot_id,
    country_origin,
    institute_origin,
    res_id_inst,
    sub_id,
    wp,
    date_survey,
    date_shipment_departure,
    shipment_company,
    date_shipment_arrival,
    institute_receiving,
    transfer_third_party,
    cold_at_sampling,
    stored_frozen,
    shipped_frozen)

be_list_freezer <- be_list_freezer %>%
  filter(
    !(sub_id %in% c("A1", "G0", "G1", "A0") &
        material == "forest floor"))

# Copy to clipboard

write.table(be_list_freezer, "clipboard", sep = "\t",
            row.names = FALSE, col.names = FALSE)

# Export

write.table(be_list_freezer,
            file = "./data/fieldwork_belgium/freezer/be_freezer2.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")








# 7. WP3: prepare forest floor mass spreadsheet ----

plots_belgium <- read.csv("./data/fieldwork_belgium/plots_belgium.csv",
                          sep = ";")

wp3_plots_be <- plots_belgium %>%
  filter(grepl("WP3", wp))

dist <- read_sheet(as_id(id_dist),
                   skip = 1)

wp3_ff <- dist %>%
  rename(sample_id = `sample_id (unieke veldcode uit Survey123 app)`,
         date_survey = `Datum bemonstering`,
         mass_recipient = `Massa (g) recipiënt zonder deksel`,
         mass_before =
           `Massa (g) monster voor (inclusief recipiënt zonder deksel)`,
         mass_after =
           `Massa monster (g) na (inclusief recipiënt zonder deksel)`) %>%
  select(sample_id, date_survey, mass_recipient, mass_before, mass_after) %>%
  mutate(
    date_survey = as.Date(parse_date(date_survey))) %>%
  filter(str_detect(sample_id,
                    paste(wp3_plots_be$plot_code_app, collapse = "|"))) %>%
  filter(str_detect(sample_id, "OL|OFH")) %>%
  mutate(
    plot_code_app = str_replace(sample_id, "^[^-]*-", "") %>%
      str_replace("-[^-]*$", ""),
    sample_code = str_extract(sample_id, "[^\\-]+$"),
    code_layer = str_extract(sample_code, "^[^_-]+"),
    mass_recipient = coalesce(mass_recipient,
                              27.6),
    # clean and convert to numeric
    mass_after = sapply(mass_after, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(gsub(",", ".", x))
    }),
    # Calculate net mass
    # mass_before_net = mass_before - mass_recipient,
    mass_after_net = mass_after - mass_recipient) %>%
  select(-mass_recipient, -mass_before) %>%
  # Create a column name based on code_layer and sample_code
  mutate(
    mass_column = case_when(
      code_layer == "OFH" ~ "mass_dry_ofh",
      code_layer == "OL" & sample_code == "OL_leaves" ~
        "mass_dry_leaves_ol",
      code_layer == "OL" & sample_code == "OL_twigs_medium" ~
        "mass_dry_twigs_medium_ol",
      code_layer == "OL" & sample_code == "OL_twigs_small" ~
        "mass_dry_twigs_small_ol")) %>%
  filter(!str_detect(sample_id, "_BackUp")) %>%
  # Pivot wider
  pivot_wider(
    id_cols = c(plot_code_app, date_survey, code_layer),
    names_from = mass_column,
    values_from = mass_after_net,
    values_fill = NA) %>%
  left_join(
    plots_belgium,
    by = "plot_code_app") %>%
  mutate(
    sample_id = case_when(
      code_layer == "OL" ~
        paste0(country_code, "-", plot_code_app, "-", "OL_flammability"),
      code_layer == "OFH" ~
        paste0(country_code, "-", plot_code_app, "-", "OFH_carbon"))) %>%
  mutate(
    sample_code = code_layer) %>%
  select(
    plot_id, sub_id, res_id_inst, institute, date_survey,
    sample_code, sample_id,
    mass_dry_ofh, mass_dry_leaves_ol, mass_dry_twigs_medium_ol,
    mass_dry_twigs_small_ol)


write.table(wp3_ff,
            file = "./output/fieldwork_belgium/forest_floor_data_inbo_wp3.csv",
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")


# 8. WP3: normal_shipment_samples_list ----

plots_belgium <- read.csv("./data/fieldwork_belgium/plots_belgium.csv",
                          sep = ";")

wp3_plots_be <- plots_belgium %>%
  filter(grepl("WP3", wp))

dist <- read_sheet(as_id(id_dist),
                   skip = 1)

undist <- read_sheet(as_id(id_undist),
                     skip = 1)

# Extract field data of undisturbed samples from folders on Google Drive
# (since we didn't weigh them in the lab and since Survey123 app data are
#  not yet available)

wp3_folders <- drive_ls(as_id(id_be_folders), type = "folder") %>%
  filter(grepl("Everbeekse", name)) %>%
  mutate(
    sub_id =
      str_extract(name,
                  "(?<=Everbeekse bossen )\\S+(?= \\d{4}-\\d{2}-\\d{2})"))

# Function to extract Kopecky data from a single sheet
extract_kopecky_data <- function(folder_id, sub_id) {

  # Get files in the folder
  fielddata_file <- suppressMessages({
    drive_ls(as_id(folder_id), type = "spreadsheet") %>%
      filter(str_detect(name, "^fielddata"))
  })

  if (nrow(fielddata_file) == 0) {

    fielddata_file <- drive_ls(as_id(folder_id)) %>%
      filter(str_detect(name, "^fielddata"))
  }

  assertthat::assert_that(nrow(fielddata_file) == 1,
                          msg = paste0("No fielddata file found in folder: ",
                                       sub_id, ".\n"))

  # Read the sheet
  sheet_data <- suppressMessages(
    read_sheet(fielddata_file$id[1], col_types = "c"))

  # Find the row with "Kopecky" in column B
  kopecky_row <- which(sheet_data[[1]] == "Kopecky")

  assertthat::assert_that(!identical(kopecky_row, integer(0)),
                          msg = paste0("Kopecky not found in sheet for: ",
                                       sub_id, ".\n"))

  # Define the target codes
  target_codes <- c("M01", "M13", "M36", "M61")

  # Look for these codes in the rows after Kopecky
  # Usually they appear a few rows below Kopecky
  search_range <- seq((kopecky_row + 1),
                      min(nrow(sheet_data), kopecky_row + 10))

  results <- tibble()

  for (code in target_codes) {

    # Find the row with this code in column B
    code_row <- search_range[which(sheet_data[[1]][search_range] == code)]

    assertthat::assert_that(!identical(code_row, integer(0)),
                            msg = paste0("Code ", code, " not found for ",
                                         sub_id, ".\n"))

    # Extract the value from column D
    mass_value <- sheet_data[[3]][code_row]

    # Add to results
    results <- bind_rows(results,
                         tibble(sub_id = sub_id,
                                code_layer = code,
                                mass = as.numeric(mass_value)))
  }
  return(results)
} # End of extract_kopecky_data()




# Apply the function to all folders
kopecky_fielddata <- wp3_folders %>%
  select(sub_id, id) %>%
  pmap_dfr(~ extract_kopecky_data(.y, .x))


wp3_samples_list <-
  bind_rows(
    # Disturbed samples
    dist %>%
      rename(sample_id = `sample_id (unieke veldcode uit Survey123 app)`) %>%
      filter(str_detect(sample_id,
                        paste(wp3_plots_be$plot_code_app, collapse = "|"))) %>%
      rename(
        mass_recipient = `Massa (g) recipiënt zonder deksel`,
        mass_after =
          `Massa monster (g) na (inclusief recipiënt zonder deksel)`) %>%
      mutate(
        # Assume that the mineral samples all fitted in one black container
        mass_recipient = coalesce(mass_recipient,
                                  27.6),
        # clean and convert to numeric
        mass_after = sapply(mass_after, function(x) {
          if (is.null(x)) return(NA_real_)
          as.numeric(gsub(",", ".", x))
        }),
        # Calculate net mass
        # mass_before_net = mass_before - mass_recipient,
        mass_after_net = mass_after - mass_recipient,
        # The OFH samples from WP3 got lost (two sites, G2 and A2)
        # BE-INBO-INBO_1-G2-1-OFH_carbon
        # BE-INBO-INBO_2-A2-1-OFH_carbon
        # We replaced them by the back-up eDNA2 samples
        # (defrosted and dried at 40 °C)
        mass_after_net = case_when(
          sample_id == "BE-INBO-INBO_1-G2-1-OFH_carbon" ~ 30.80 - 27.6,
          sample_id == "BE-INBO-INBO_2-A2-1-OFH_carbon" ~ 30.66 - 27.6,
          TRUE ~ mass_after_net)) %>%
      select(sample_id, mass_after_net) %>%
      mutate(type = "disturbed"),
    # Undisturbed samples
    undist %>%
      rename(sample_id = `sample_id (unieke veldcode uit Survey123 app)`) %>%
      filter(str_detect(sample_id,
                        paste(wp3_plots_be$plot_code_app, collapse = "|"))) %>%
      select(sample_id) %>%
      mutate(type = "undisturbed")) %>%
  mutate(
    plot_code_app = str_replace(sample_id, "^[^-]*-", "") %>%
      str_replace("-[^-]*$", ""),
    sample_code = str_extract(sample_id, "[^\\-]+$"),
    code_layer = str_extract(sample_code, "^[^_-]+")) %>%
  filter(!grepl("_BackUp", sample_code)) %>%
  filter(code_layer != "OL") %>%
  left_join(
    plots_belgium,
    by = "plot_code_app") %>%
  left_join(
    kopecky_fielddata %>%
      mutate(type = "undisturbed"),
    by = c("code_layer", "sub_id", "type")) %>%
  mutate(
    mass_sample_gross = case_when(
      # mass of one tube
      grepl("-OFH_carbon", sample_id) ~ mass_after_net + 12.7,
      type == "disturbed" ~ mass_after_net + 2 * 25.0, # mass of 2 ziploc bags
      # Note: masses for undisturbed samples are field-moist,
      # while we air-dried them
      type == "undisturbed" ~ mass + 2 * 25.0)) %>%
  select(-mass_after_net, -mass) %>%
  mutate(sample_code = code_layer,
         material = case_when(
           grepl("OFH", sample_id) ~ "forest floor",
           grepl("^M", sample_code) ~ "soil"),
         country_origin = "Belgium",
         date_shipment_departure = NA,
         shipment_company = "bpost",
         date_shipment_arrival = NA,
         institute_receiving = "VUK") %>%
  rename(institute_origin = institute,
         date_survey = date) %>%
  select(sample_id, sample_code, material, mass_sample_gross,
         plot_id, country_origin, institute_origin, res_id_inst,
         sub_id, date_survey, date_shipment_departure, shipment_company,
         date_shipment_arrival, institute_receiving)



write.table(wp3_samples_list,
            file = paste0("./output/fieldwork_belgium/",
                          "inbo_normal_shipment_samples_list_wp3.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")


