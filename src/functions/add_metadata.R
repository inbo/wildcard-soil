
add_metadata <- function(df_plot) {

  # Add:

  # - MAT
  # - MAP
  # - Biogeographical regio
  # - Altitude
  # - Parent material

  # To compare with existing data:
  # - Slope
  # - WRB Reference Soil Group


  assertthat::assert_that(
    any("latitude_dec" %in% names(df_plot)) &&
      any("longitude_dec" %in% names(df_plot)) &&
      all(!is.na(df_plot$latitude_dec)) &&
      all(!is.na(df_plot$longitude_dec)))

  source("./src/functions/as_sf.R")
  source("./src/functions/overlay_tif.R")



  # Prepare source files ----

  path_worldclim <- "./data/additional_data/shapefiles/worldclim/wc2.1_30s_"

  # Import the shapefile with biogeographical regions

  biogeo_sf <-
    read_sf(paste0("./data/additional_data/shapefiles/biogeographical_regions/",
                   "BiogeoRegions2016.shp")) %>%
    # Reduce the file size
    st_simplify(dTolerance = 1000) %>%
    # Remove category "outside"
    filter(.data$short_name != "outside") %>%
    # Rename category "Black Sea"
    mutate(code = ifelse(.data$short_name == "blackSea",
                         "Black Sea", .data$code)) %>%
    rename(biogeographical_region = code) %>%
    select(biogeographical_region, geometry)

  # Calculate slope (in degrees)
  slope <- terra::terrain(
    terra::rast(paste0(path_worldclim,
                       "elev/wc2.1_30s_elev.tif")),
    v = "slope",
    unit = "degrees")

  # Parent material

  df_stratifiers <- read_sheet(as_id(id_stratifiers))



  # Make overlays with shapefiles ----

  ## MAT ----

  # Get climate data (WorldClim version 2.1; 1970-2000)

  df_plot_sf <- df_plot %>%
    as_sf

  # Add average temperatures (for twelve months of the year)
  # in °C

  for (i in seq(1, 12)) {

    path_worldclim_i <- paste0(path_worldclim,
                               "tavg/wc2.1_30s_tavg_",
                               sprintf("%02d", i),
                               ".tif")

    df_plot_sf <- overlay_tif(sf1 = df_plot_sf,
                              path_tif = path_worldclim_i)

  }

  # Select the columns that start with "wc2.1_30s_tavg_"

  tavg_cols <- grep("^wc2.1_30s_tavg_", names(df_plot_sf), value = TRUE)

  df_plot_sf <- df_plot_sf %>%
    st_drop_geometry() %>%
    # Calculate row-wise average over the twelve months
    mutate(mat = round(rowMeans(select(., all_of(tavg_cols)),
                                na.rm = TRUE), 1)) %>%
    # Remove the original columns
    select(-all_of(tavg_cols)) %>%
    as_sf



  ## MAP ----

  # Add cumulative precipitation (for twelve months of the year)
  # in mm

  for (i in seq(1, 12)) {

    path_worldclim_i <- paste0(path_worldclim,
                               "prec/wc2.1_30s_prec_",
                               sprintf("%02d", i),
                               ".tif")

    df_plot_sf <- overlay_tif(sf1 = df_plot_sf,
                           path_tif = path_worldclim_i)

  }

  # Select the columns that start with "wc2.1_30s_prec_"

  prec_cols <- grep("^wc2.1_30s_prec_", names(df_plot_sf), value = TRUE)

  df_plot_sf <- df_plot_sf %>%
    st_drop_geometry() %>%
    # Calculate row-wise average over the twelve months using dplyr
    mutate(map = round(rowSums(select(., all_of(prec_cols)),
                               na.rm = TRUE), 0)) %>%
    # Remove the original columns
    select(-all_of(prec_cols)) %>%
    as_sf


  ## Altitude and slope ----

  # Altitude (in meter) and slope (in degrees) data from WorlClim

  df_plot_sf <-
    overlay_tif(sf1 = df_plot_sf,
                path_tif = paste0(path_worldclim,
                                  "elev/wc2.1_30s_elev.tif")) %>%
    overlay_tif(spat_raster = slope) %>%
    st_drop_geometry() %>%
    rename(altitude = "wc2.1_30s_elev") %>%
    mutate(slope = round(slope, 1)) %>%
    rename(slope_worldclim = slope) %>%
    as_sf


  ## Parent material and WRB RSG ----

  par_mat_dom1_dictionary <-
    read.csv(paste0("./data/additional_data/shapefiles/",
                    "European Soil Database/",
                    "par_mat_dom1_dictionary.csv"),
             sep = ";") %>%
    mutate(
      parent_material_esd = str_to_sentence(par_mat_dom1),
      # Remove everything between brackets
      parent_material_esd = str_remove(parent_material_esd, "\\s*\\([^)]*\\)")
    ) %>%
    rename(parmado1 = PAR.MAT.DOM1) %>%
    select(parmado1, parent_material_esd)

  par_mat_dom2_dictionary <-
    read.csv(paste0("./data/additional_data/shapefiles/European Soil Database/",
                    "par_mat_dom2_dictionary.csv"),
             sep = ";") %>%
    mutate(parent_material_esd_2 = str_to_sentence(par_mat_dom4)) %>%
    rename(parmado2 = PAR.MAT.DOM2) %>%
    select(parmado2, parent_material_esd_2)

  wrb_lev1_dictionary <-
    read.csv(paste0("./data/additional_data/shapefiles/European Soil Database/",
                    "wrb-lev1-dictionary.csv"),
             sep = ";") %>%
    rename(WRBLV1 = WRB.LEV1) %>%
    mutate(
      wrb_ref_soil_group_esd = ifelse(
        grepl("s$", RSG) | grepl("z$", RSG) |
          !is.na(suppressWarnings(as.numeric(WRBLV1))),
        RSG,
        paste0(RSG, "s")
      )) %>%
    select(-RSG)

  df_plot_sf <- df_plot_sf %>%
    overlay_tif(path_tif = paste0("./data/additional_data/shapefiles/",
                                  "European Soil Database/",
                                  "parmado1.tif"),
                values_to_avoid = 0) %>%
    overlay_tif(path_tif = paste0("./data/additional_data/shapefiles/",
                                  "European Soil Database/",
                                  "parmado2.tif"),
                values_to_avoid = 0) %>%
    overlay_tif(path_tif = paste0("./data/additional_data/shapefiles/",
                                  "European Soil Database/",
                                  "wrblv1.tif")) %>%
    left_join(par_mat_dom1_dictionary ,
              by = "parmado1") %>%
    select(-parmado1) %>%
    left_join(par_mat_dom2_dictionary ,
              by = "parmado2") %>%
    select(-parmado2) %>%
    left_join(wrb_lev1_dictionary ,
              by = "WRBLV1") %>%
    select(-WRBLV1) %>%
    left_join(
      df_stratifiers %>%
        select(plot_code_simple, parent_material),
      by = "plot_code_simple"
    )

## Biogeographical region ----

  df_plot_added <- df_plot_sf %>%
    # Add biogeographical region
    st_join(biogeo_sf) %>%
    st_drop_geometry()



return(df_plot_added)


}
