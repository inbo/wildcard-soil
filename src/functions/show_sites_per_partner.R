

#' Show WP2 and WP3 sites per partner
#' This is useful for a quick link with the Survey123 app output
#'
#' @param inst Name of institute following the WP2 or WP3 site metadata table
#'
#' @export
#'
#' @examples show_sites_per_partner(c("VUK", "VUKOZ(VUK)"))


show_sites_per_partner <- function(inst) {

  # WP2:
  # AlberIT - UNIRC BFNP BGD-NP CULS DISAFA - UNITO FVA-BW IBER-BAS INBO
  # INCDS LWF NPS NWFVA UCPH UL UNITBV UNIUD UNIUD/HSI URK USV VUK WR WSL WULS

  # WP3:
  # AU HUN-REN Centre for Ecological Research/VUKOZ(VUK) IBER-BAS INBO
  # TUZVO UNIPA UNITO UNIUD VUKOZ(VUK) ZRC SAZU

  if (!exists("his", .GlobalEnv)) {

    source("./src/functions/get_his.R")
    his <- get_his()
    assign("his", his)

  }

  if (!exists("wp3_sites", .GlobalEnv)) {

    source("./src/functions/get_wp3_sites.R")
    wp3_sites <- get_wp3_sites()
    assign("wp3_sites", wp3_sites)

  }

  filtered_wp2 <- get("his") %>%
    filter(grepl(paste(inst, collapse = "|"), institute)) %>%
    arrange(institute, res_id_inst) %>%
    select(composed_site_id,
           res_id_inst,
           reserve_name,
           sub_id)

  filtered_wp3 <- get("wp3_sites") %>%
    filter(grepl(paste(inst, collapse = "|"), institute)) %>%
    arrange(institute, region, site_id) %>%
    select(
      composed_site_id,
      chronosequence_id,
      region,
      site_id,
      plot_id)

  if (nrow(filtered_wp2) > 0) {

    cat("\n\nWP2\n")

    print(filtered_wp2,
          n = 60)
  }


  if (nrow(filtered_wp3) > 0) {

    cat("\n\nWP3\n")

    print(filtered_wp3,
          n = 60)
  }

}





