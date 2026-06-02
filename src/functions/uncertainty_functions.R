

# Function to add uncertainty ranges for coarse fragments ----

add_cf_uncertainty <- function(coaf_2mm,
                               coaf_50mm,
                               coarse_fragment_vol_ring) {

  # Assert that coaf_2mm >= coaf_50mm

  assertthat::assert_that(
    coaf_2mm >= coaf_50mm,
    msg =  paste0("Volumetric percentage of coarse fragments > 50 mm ",
                  "should not exceed that > 2 mm."))

  # 1. Define uncertainty functions ----

  # 1.1. Uncertainty in field estimates
  # (based on spatial uncertainty + estimation uncertainty)

  uncertainty_field <- function(cf) {

    # Within-site uncertainty
    within_site_sd <- 5 + 0.15 * cf
      # ifelse(cf <= 20, 0.75 * cf, 15)

    # Measurement uncertainty
    measurement_unc <- case_when(
      cf <= 5 ~ 1,
      cf <= 30 ~ 2.5,
      cf <= 50 ~ 5,
      TRUE ~ 12.5)

    # Total uncertainty (simple sum)
    sqrt(within_site_sd^2 + measurement_unc^2)

    }

  # 1.2. Uncertainty in coarse fragment contents determined using kopecky rings

  uncertainty_lab <- function(cf, uncertainty_sd_rel = 0.1) {

    # Lab measurement precision (10 % relative standard deviation)

    # Based on the relative standard deviation between bulk density rings
    # per site x depth (from the test sampling event in 2024)

    # Within-site uncertainty
    return(c(pmax(0, cf - uncertainty_sd_rel * cf),
             pmin(100, cf + uncertainty_sd_rel * cf)))

  }




  # 2. Apply uncertainty functions to different CF size fractions ----

  # 2.1. Uncertainty > 50 mm

  # In case the total amount of coarse fragments is low,
  # we can assume the uncertainty is 0

  coaf_50mm_unc <- ifelse(coaf_50mm == 0 &
                            coaf_2mm <= 5,
                          0,
                          uncertainty_field(coaf_50mm))

  coaf_50mm_unc_range <- c(pmax(0, coaf_50mm - coaf_50mm_unc),
                           pmin(100, coaf_50mm + coaf_50mm_unc))


  # 2.2. Uncertainty 2-50 mm

  # 2.2.1. Field

  coaf_2_50mm <- coaf_2mm - coaf_50mm
  coaf_2_50mm_unc <- uncertainty_field(coaf_2_50mm)
  coaf_2_50mm_unc_range <- c(pmax(0, coaf_2_50mm - coaf_2_50mm_unc),
                             pmin(100, coaf_2_50mm + coaf_2_50mm_unc))

  # 2.2.2. Lab

  coarse_fragment_vol_ring_unc_range <-
    uncertainty_lab(coarse_fragment_vol_ring)

  # 2.2.3. Combined
  # For 2-50 mm, take the widest bound around the field and lab uncertainty
  # ranges

  unc_2_50mm_range <-
    c(pmax(0, min(coaf_2_50mm_unc_range, coarse_fragment_vol_ring_unc_range)),
      pmin(100, max(coaf_2_50mm_unc_range, coarse_fragment_vol_ring_unc_range)))


  # 3. Total uncertainty (>2 mm) ----

  # For the final uncertainty ranges:
  # Sum up the minima of the 2-50 mm and the >50 mm uncertainty ranges,
  # as well as the maxima

  return(list(
    coarse_fragment_vol_min =
      round(pmax(0, coaf_50mm_unc_range[1] + unc_2_50mm_range[1]), 1),
    coarse_fragment_vol_max =
      round(pmin(95, coaf_50mm_unc_range[2] + unc_2_50mm_range[2]), 1)
  ))

}




# Function to add uncertainty ranges for bulk density ----

add_bd_uncertainty <- function(bd, uncertainty_sd = 125) {

  # Within-site uncertainty
  return(list(
    bulk_density_min = round(pmax(0, bd - uncertainty_sd), 1),
    bulk_density_max = round(pmin(2650, bd + uncertainty_sd), 1)
  ))

}
