
get_attribute_catalogue_app <- function() {

  # Useful for the inconsistency report for the partners
  # Largely based on "DATABASE OF SOIL SAMPLING PROTOCOL.docx" by VUK

  # For now, I just included numeric columns

  # Step 1: Manually specified attributes

  attribute_catalogue <- tibble::tibble(
    column_name = c(
      "latitude", "longitude", "hor_accuracy", "surface_frame", "vol_ring",
      "air_temperature", "depth_cultivation_cm", "slope_deg"
    ),
    descr = c(
      "Decimal degrees latitude (WGS84)",
      "Decimal degrees longitude (WGS84)",
      "Horizontal accuracy of coordinate measurements",
      "Surface area of forest floor sampling frame",
      "Volume of Kopecky ring used for sampling",
      "Rough field estimate of air temperature",
      "Significant depth of cultivation (e.g. ploughing)",
      "Slope of plot"
    ),
    unit = c("decimal degrees (°)", "decimal degrees (°)", "m", "m²",
             "cm³", "°C", "cm", "°"))


  # Step 2: Automated entries for pattern-based columns


  # Column names from Survey123 app output

  col_names <- c(
    "objectid", "globalid", "logo", "date_time", "country", "team", "region", "site_id", "plot_id", "code",
    "coordinates", "hor_accuracy_aut", "latitude", "longitude", "hor_accuracy_man", "vol_ring", "bulk_den_fraction_prof",
    "actual_weather", "past_weather", "soil_condition", "soil_dist", "depth_cultivation_cm", "other_remarks", "macrorelief",
    "moisture", "slope_type", "slope_deg", "scheme_square", "scheme_triangl", "scheme", "azimuth", "direct_photo_1",
    "direct_photo_2", "direct_photo_3", "direct_photo_4", "num_photo_humus", "P1_humus_form", "P1_ol_thickness",
    "P1_ol_tamass", "P1_ofh_thickness", "P1_ofh_tamass", "P1_remarks_fofloor_sample", "ref_soil_grp", "principal_q_RG",
    "suppl_q_RG", "P1_m01coaf_2mm", "P1_m01coaf_50mm", "P1_m01coaf_lab", "P1_m13coaf_2mm", "P1_m13coaf_50mm",
    "P1_m13coaf_lab", "P1_m36coaf_2mm", "P1_m36coaf_50mm", "P1_m36coaf_lab", "P1_m61coaf_2mm", "P1_m61coaf_50mm",
    "P1_m61coaf_lab", "P1_other_remarks_fragment", "P1_m01dist", "P1_m13dist", "P1_m36dist", "P1_m61dist",
    "P1_depth_bedrock", "P1_notes_dist_samples", "P1_bulk_density", "P1_reason_bulk_den", "P1_notes_bulk_den",
    "P2_expected_pos", "P2_describe_pos", "P2_humus_form", "P2_ol_thickness", "P2_ol_tamass", "P2_ofh_thickness",
    "P2_ofh_tamass", "P2_remarks_fofloor_sample", "P2_m01dist", "P2_m13dist", "P2_m36dist", "P2_m61dist",
    "P2_depth_bedrock", "P2_notes_dist_samples", "P2_bulk_density", "P2_reason_bulk_den", "P2_notes_bulk_den",
    "P3_expected_pos", "P3_describe_pos", "P3_humus_form", "P3_ol_thickness", "P3_ol_tamass", "P3_ofh_thickness",
    "P3_ofh_tamass", "P3_remarks_fofloor_sample", "P3_m01dist", "P3_m13dist", "P3_m36dist", "P3_m61dist",
    "P3_depth_bedrock", "P3_notes_dist_samples", "P3_bulk_density", "P3_reason_bulk_den", "P3_notes_bulk_den",
    "P4_expected_pos", "P4_describe_pos", "P4_humus_form", "P4_ol_thickness", "P4_ol_tamass", "P4_ofh_thickness",
    "P4_ofh_tamass", "P4_remarks_fofloor_sample", "P4_m01dist", "P4_m13dist", "P4_m36dist", "P4_m61dist",
    "P4_depth_bedrock", "P4_notes_dist_samples", "P4_bulk_density", "P4_reason_bulk_den", "P4_notes_bulk_den",
    "P5_expected_pos", "P5_describe_pos", "P5_humus_form", "P5_ol_thickness", "P5_ol_tamass", "P5_ofh_thickness",
    "P5_ofh_tamass", "P5_remarks_fofloor_sample", "P5_m01dist", "P5_m13dist", "P5_m36dist", "P5_m61dist",
    "P5_depth_bedrock", "P5_notes_dist_samples", "P5_bulk_density", "P5_reason_bulk_den", "P5_notes_bulk_den",
    "P6_expected_pos", "P6_describe_pos", "P6_humus_form", "P6_ol_thickness", "P6_ofh_thickness", "P6_remarks_fofloor_sample",
    "P6_m01dist", "P6_m13dist", "P6_depth_bedrock", "P6_notes_dist_samples", "P7_expected_pos", "P7_describe_pos",
    "P7_humus_form", "P7_ol_thickness", "P7_ofh_thickness", "P7_remarks_fofloor_sample", "P7_m01dist", "P7_m13dist",
    "P7_depth_bedrock", "P7_notes_dist_samples", "P8_expected_pos", "P8_describe_pos", "P8_humus_form",
    "P8_ol_thickness", "P8_ofh_thickness", "P8_remarks_fofloor_sample", "P8_m01dist", "P8_m13dist",
    "P8_depth_bedrock", "P8_notes_dist_samples", "P9_expected_pos", "P9_describe_pos", "P9_humus_form",
    "P9_ol_thickness", "P9_ofh_thickness", "P9_remarks_fofloor_sample", "P9_m01dist", "P9_m13dist",
    "P9_depth_bedrock", "P9_notes_dist_samples", "OL_flamm", "OFH_carbon", "OFH_eDNA1", "OFH_eDNA2", "OFH_back_up",
    "remarks_fofloor_sample", "m01_carbon", "m01_edna1", "m01_edna2", "m01_back_up", "m13_carbon", "m13_back_up",
    "m36_carbon", "m36_back_up", "m61_carbon", "m61_back_up", "m01_bulk_den", "m13_bulk_den", "m36_bulk_den",
    "m61_bulk_den", "gen_observtn", "other_observtn", "OL_flamm_check", "OL_flamm_sampleID", "OFH_carbon_check",
    "OFH_carbon_sampleID", "OFH_eDNA1_check", "OFH_eDNA1_sampleID", "OFH_eDNA2_check", "OFH_eDNA2_sampleID",
    "OFH_back_up_check", "OFH_back_up_sampleID", "m01_carbon_check", "m01_carbon_sampleID", "m01_edna1_check",
    "m01_edna1_sampleID", "m01_edna2_check", "m01_edna2_sampleID", "m01_back_up_check", "m01_back_up_sampleID",
    "m13_carbon_check", "m13_carbon_sampleID", "m13_back_up_check", "m13_back_up_sampleID", "m36_carbon_check",
    "m36_carbon_sampleID", "m36_back_up_check", "m36_back_up_sampleID", "m61_carbon_check", "m61_carbon_sampleID",
    "m61_back_up_check", "m61_back_up_sampleID", "m01_bulk_den_check", "m01_bulk_den_sampleID", "m13_bulk_den_check",
    "m13_bulk_den_sampleID", "m36_bulk_den_check", "m36_bulk_den_sampleID", "m61_bulk_den_check", "m61_bulk_den_sampleID",
    "CreationDate", "Creator", "EditDate", "Editor", "humus_form", "X_WGS", "Y_WGS")


  # Helper to create entries
  make_entry <- function(col, descr, unit) {
    tibble::tibble(column_name = col, descr = descr, unit = unit)
  }

  library(tibble)
  library(purrr)
  library(glue)

  make_entry <- function(col, descr, unit) {
    tibble(column_name = col, descr = descr, unit = unit)
  }

  pattern_entries <- map_dfr(col_names, function(col) {
    if (grepl("_thickness$", col)) {
      make_entry(
        col,
        glue("Thickness of the {sub('.*_(.*)_thickness$', '\\\\1', col)} layer at sampling point {sub('(_.*)$', '', col)}"),
        "cm"
      )
    } else if (grepl("_tamass$", col)) {
      make_entry(
        col,
        glue("Net field-moist mass of the {sub('.*_(.*)_tamass$', '\\\\1', col)} layer at sampling point {sub('(_.*)$', '', col)}"),
        "g"
      )
    } else if (grepl("coaf_2mm$", col)) {
      make_entry(
        col,
        glue("Volumetric percentage of coarse fragments >2 mm in layer {sub('.*_m(.*)coaf_2mm$', '\\\\1', col)}"),
        "%"
      )
    } else if (grepl("coaf_50mm$", col)) {
      make_entry(
        col,
        glue("Volumetric percentage of coarse fragments >50 mm in layer {sub('.*_m(.*)coaf_50mm$', '\\\\1', col)}"),
        "%"
      )
    } else if (grepl("_depth_bedrock$", col)) {
      make_entry(
        col,
        glue("Depth of bedrock at sampling point {sub('_depth_bedrock$', '', col)}"),
        "cm"
      )
    } else if (grepl("_flamm$", col)) {
      make_entry(
        col,
        glue("Net mass of the field-moist subsample from layer {sub('_flamm$', '', col)} for flammability analysis"),
        "g"
      )
    } else if (grepl("_carbon$", col)) {
      make_entry(
        col,
        glue("Net mass of the field-moist subsample from layer {sub('_carbon$', '', col)} for carbon analysis"),
        "g"
      )
    } else if (grepl("_(eDNA1|eDNA2|edna1|edna2)$", col)) {
      make_entry(
        col,
        glue("Net mass of the field-moist subsample from layer {sub('_(eDNA1|eDNA2|edna1|edna2)$', '', col)} for {sub('.*_', '', col)} analysis"),
        "g"
      )
    } else if (grepl("_back_up$", col)) {
      make_entry(
        col,
        glue("Net mass of the field-moist subsample from layer {sub('_back_up$', '', col)} to be used as a back-up"),
        "g"
      )
    } else if (grepl("^m\\d{2}_bulk_den$", col)) {
      make_entry(
        col,
        glue("Net mass of the undisturbed field-moist samples from layer {sub('_bulk_den$', '', col)} for bulk density assessment"),
        "g"
      )
    } else {
      # If no pattern matches, return nothing
      NULL
    }
  })


  # Combine


  attribute_catalogue <- bind_rows(attribute_catalogue, pattern_entries)


  return(attribute_catalogue)






}
