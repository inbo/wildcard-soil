
# Pedotransfer functions for bulk density estimation + uncertainty

# For mineral soil only!

# This pedotransfer function is based on the systematic Level I monitoring
# network of ICP Forests, and was selected based on the best tradeoff between
# prediction accuracy and (number and availability of) input variables
# (Feb/March 2024). The function was validated using the Level II network,
# for which the uncertainty (160 kg m-3) is also added.

# Expected relationship of bulk density (BD) with total organic carbon (TOC):
# BD = 1511 – (81,1 * sqrt(TOC))


# Bulk density forest floor excl. OL (Level I)

  # Min.     1st Qu.   Median    Mean      3rd Qu.   Max.
  # 42.08333 111.00000 118.00000 116.79218 118.00000 254.66667
  # 2.5%      5%       95%       97.5%      stdev
  # 48.00000  55.00000 190.00000 217.33333  35.94374

# Bulk density peat (Level I)

  # Min.    1st Qu.  Median   Mean     3rd Qu.  Max.
  # 30.1000 105.7000 128.0000 127.2487 129.2750 319.0000
  # 2.5%     5%      95%      97.5%     stdev
  # 37.7125  51.2000 221.3500 257.5250  47.1435




# Bulk density pedotransfer functions ----

bd_ptf <- function(toc,
                   uncertainty_sd = 160) {

  bd <- 1511 - 81.1 * sqrt(toc)

  return(list(
    bulk_density_ptf = round(pmax(bd,
                                  # Unlikely to be smaller than 100 for mineral
                                  100)),

    bulk_density_ptf_min = round(pmax(bd - uncertainty_sd,
                                      50)),

    bulk_density_ptf_max = round(pmax(bd + uncertainty_sd,
                                      100))
  ))
}
