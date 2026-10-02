

update_ids <- function(data) {

  # Primary keys of dataset (composed_site_id, reserve_name, res_id_inst,
  # plot_code_simple) were updated relative to the original source data from
  # summer 2025 for some sites.
  # This scripts applies the update.

  if (!file.exists("./data/additional_data/id_conversion_table.csv")) {
    rlang::abort(paste0("The file 'id_conversion_table.csv' does not exist ",
                        "in the specified path."))
  }

  id_conversion_table <-
    read.csv("./data/additional_data/id_conversion_table.csv",
             sep = ";")

  if (any(grepl("Bielowieza", id_conversion_table$composed_site_id))) {
    rlang::abort(paste0("The id_conversion_table.csv file contains the site ",
                        "'Bielowieza'. ",
                        "Update R code to account for transect."))
  }

  # From now on, the assumption is that Bialowieza has not been updated
  # (to simplify the script, because in that case, composed_site_id equals
  # plot_code_simple, so the transect in Bialowieza doesn't need to be
  # added to the composed_site_id)

  id_conversion_table <- id_conversion_table %>%
    mutate(plot_code_simple_new = composed_site_id_new)



  # Update the plot_code_simple column in the data frame if existing

  if ("plot_code_simple" %in% colnames(data)) {

    if (!"composed_site_id" %in% colnames(data)) {

      data <- data %>%
        left_join(id_conversion_table %>%
                    rename(plot_code_simple = composed_site_id) %>%
                    select(plot_code_simple, plot_code_simple_new),
                  by = "plot_code_simple") %>%
        mutate(plot_code_simple = ifelse(!is.na(plot_code_simple_new),
                                         plot_code_simple_new,
                                         plot_code_simple)) %>%
        select(-plot_code_simple_new)

    } else {

      data <- data %>%
        left_join(id_conversion_table %>%
                    select(composed_site_id, plot_code_simple_new),
                  by = "composed_site_id") %>%
        mutate(plot_code_simple = ifelse(!is.na(plot_code_simple_new),
                                         plot_code_simple_new,
                                         plot_code_simple)) %>%
        select(-plot_code_simple_new)
    }


  }

  # Update the res_id_inst column in the data frame if existin

  if ("res_id_inst" %in% colnames(data)) {
    data <- data %>%
      left_join(id_conversion_table %>%
                  select(composed_site_id, res_id_inst_new),
                by = "composed_site_id") %>%
      mutate(res_id_inst = ifelse(!is.na(res_id_inst_new),
                                  res_id_inst_new,
                                  res_id_inst)) %>%
      select(-res_id_inst_new)
  }

  # Update the reserve_name column in the data frame if existing

  if ("reserve_name" %in% colnames(data)) {
    data <- data %>%
      left_join(id_conversion_table %>%
                  select(composed_site_id, reserve_name_new),
                by = "composed_site_id") %>%
      mutate(reserve_name = ifelse(!is.na(reserve_name_new),
                                   reserve_name_new,
                                   reserve_name)) %>%
      select(-reserve_name_new)
  }

  # Update composed_site_id itself if existing

  if ("composed_site_id" %in% colnames(data)) {
    data <- data %>%
      left_join(id_conversion_table %>%
                  select(composed_site_id, composed_site_id_new),
                by = "composed_site_id") %>%
      mutate(composed_site_id = ifelse(!is.na(composed_site_id_new),
                                        composed_site_id_new,
                                        composed_site_id)) %>%
      select(-composed_site_id_new)
  }

  return(data)

}
