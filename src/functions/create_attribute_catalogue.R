
create_attribute_catalogue <- function(data_frame,
                                       path_to_save = NULL) {

  # Import
  dict <- read.csv("./data/attribute_catalogue_dictionary.csv",
                   sep = ";")

  # Required parameters

  assertthat::assert_that("data.frame" %in% class(data_frame))
  required_par <- names(data_frame)

  missing_par <- required_par[which(!required_par %in% dict$column_name)]

  if (!identical(missing_par, character(0))) {

    cat(paste0("\nPlease add the following parameters (with descriptions) to ",
               "'./data/additional_data/attribute_catalogue_dictionary.txt'.",
               "\nSubsequently rerun this function.\n\n"))
    cat(paste0("-   ", sort(missing_par), ""), sep = "\n")


  } else {

    # If all parameters have a description in the dictionary

    attr_cat <- dict %>%
      filter(column_name %in% required_par) %>%
      # Arrange following the exact order of the columns in the dataframe
      arrange(match(column_name, required_par))

    if (is.null(path_to_save)) {

      # Define the path to save the .txt file
      path_to_save <- "./output/attribute_catalogue.csv"

    } else {

      if (!grepl(".csv$", path_to_save)) {

        path_to_save <- paste0(path_to_save, "attribute_catalogue.csv")

      }

    }

    write.table(attr_cat,
                file = path_to_save,
                row.names = FALSE,
                na = "",
                sep = ";",
                dec = ".")

    # Print a message to indicate that the file has been saved
    cat("\nAttribute table saved to", path_to_save, "\n")

  } # End of "if no missing parameters"


}
