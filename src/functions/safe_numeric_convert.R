
#' Safe numeric conversion with cleaning
#'
#' Converts a vector of values to numeric format after cleaning, with
#' comprehensive handling of missing values, empty strings,
#' and conversion issues.
#'
#' @param x A vector of values to be converted to numeric. Can contain mixed
#'  types including character, numeric, logical, NULL values, or the string
#'  "NULL".
#' @param col_name Character string specifying the column name for error
#'  reporting. Defaults to the deparsed expression of x.
#' @param verbose Logical indicating whether to print warning messages for
#'   conversion issues. Default is TRUE.
#'
#' @details
#' The function processes each element of the input vector and:
#' - Converts NULL values, NA values, empty strings, whitespace-only strings,
#'     and the string "NULL" (case-insensitive) to NA_real_
#' - Replaces commas with dots to handle European decimal notation
#' - Attempts numeric conversion using as.numeric()
#' - Returns the original cleaned value if conversion fails
#' - Optionally prints detailed warning messages for problematic conversions
#'
#'
#' @examples
#' test_data <- c("1.5", "2,3", "NULL", "", NA, "invalid", "4.7")
#' safe_numeric_convert(test_data, "test_column")
#'
#' undist_harm %>%
#'   mutate(across(all_of(columns_to_check),
#'                 ~ safe_numeric_convert(.x, cur_column())))
#'

safe_numeric_convert <- function(x,
                                 col_name = deparse(substitute(x)),
                                 verbose = TRUE) {

  sapply(seq_along(x), function(i) {

    val <- x[i]

    # Convert to character first to check for "NULL" string
    val_char <- as.character(val)

    # Handle NULL, NA, empty string cases, and string "NULL"
    if (is.null(val) || is.na(val) ||
        (is.character(val) && nchar(trimws(val)) == 0) ||
        trimws(toupper(val_char)) == "NULL") {
      return(NA_real_)
    }

    # Clean (replace comma with dot)
    val_clean <- gsub(",", ".", val_char)

    # Try converting to numeric
    tryCatch({
      numeric_val <- as.numeric(val_clean)
      if (is.na(numeric_val) && !is.na(val_clean)) {
        if (verbose) {
          cat("Warning: Could not convert to numeric -",
              "Column:", col_name,
              "Row:", i,
              "Value:", val_clean, "\n")
        }
        return(val_clean)
      }
      return(numeric_val)
    }, warning = function(w) {
      if (verbose) {
        cat("Warning: Conversion issue -",
            "Column:", col_name,
            "Row:", i,
            "Value:", val_clean, "\n")
      }
      return(val_clean)
    }, error = function(e) {
      if (verbose) {
        cat("Error: Conversion failed -",
            "Column:", col_name,
            "Row:", i,
            "Value:", val_clean, "\n")
      }
      return(val_clean)
    })
  })
}
