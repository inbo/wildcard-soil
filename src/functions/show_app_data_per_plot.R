
#' Display Survey123 app data per plot
#'
#' This function prints data submitted by project partners via the Survey123
#' field app in a structured and readable format for manual inspection.
#' The function loops through plots stored in the table with high-intensity
#' sites (HIS) and prints the information plot by plot.
#'
#' The output is intended for quality control and quick visual checking
#' of field data submitted by partners.
#'
#' @param plot_code_simple Character string specifying the plot for which
#'   the data should be displayed. If `NULL` (default), the function loops
#'   over all plots. Must be non-NULL when `continue_loop_plots` is FALSE.
#'
#' @param continue_loop_plots Logical. If `TRUE` (default), the function
#'   continues looping through all plots in the column "plot_code_simple" of
#'   the HIS table. If `plot_code_simple` is provided, the loop will start
#'   from that plot and continue with subsequent plots (according to the order
#'   in the HIS table). If `FALSE`, only the specified `plot_code_simple`
#'   will be shown.
#'
#' @param compartment Character string specifying which soil compartment
#'   should be displayed. Options are:
#'   \itemize{
#'   \item `"all"` – show all compartments
#'   \item `"belowground"` – show only belowground data
#'   \item `"aboveground"` – show only aboveground data
#'   }
#'
#' @examples
#' \dontrun{
#' # Show data for all plots
#' show_app_data_per_partner()
#'
#' # Show data starting from a specific plot
#' show_app_data_per_partner(
#'   plot_code_simple = "INBO__11__Bos Terrijst__NA"
#' )
#'
#' # Show only one plot
#' show_app_data_per_partner(
#'   plot_code_simple = "INBO__11__Bos Terrijst__NA",
#'   continue_loop_plots = FALSE
#' )
#'
#' # Show only belowground compartment
#' show_app_data_per_partner(compartment = "belowground")
#' }
#'
#' @seealso
#' \code{\link{cat}}, \code{\link{print}}
#'
#' @export

show_app_data_per_plot <- function(plot_code_simple = NULL,
                                   continue_loop_plots = TRUE,
                                   compartment = c("all",
                                                   "belowground",
                                                   "aboveground")) {

  if (!exists("his", envir = globalenv())) {
    source("./src/functions/get_his.R")
    his <- get_his()
  } else {
    his <- get("his", envir = globalenv())
  }

  # Find the matching plot_code_simple if needed
  # (Sometimes easier if you can just type a part of plot_code_simple)

  ind <- which(plot_code_simple == his$plot_code_simple)

  if (identical(ind, integer(0))) {

    ind <- which(grepl(plot_code_simple, his$plot_code_simple,
                       ignore.case = TRUE))

    assert_that(!identical(ind, integer(0)),
                msg = "No matching 'plot_code_simple' found.")

    assert_that(length(ind) == 1,
                msg = "Multiple matches for 'plot_code_simple' found.")

    plot_code_simple_orig <- plot_code_simple
    plot_code_simple <- his$plot_code_simple[ind]

  }


  for (plot_code_simple_i in unique(his$plot_code_simple)) {

    # If all plots have to be looped (continue_loop_plots == TRUE):
    # if plot_code_simple is not empty, start from plot_code_simple and
    # continue the loop from there (in the HIS list order)

    if (continue_loop_plots == TRUE &&
        !is.null(plot_code_simple) &&
        !identical(plot_code_simple_i, plot_code_simple) &&
        (which(his$plot_code_simple == plot_code_simple_i) <
         which(his$plot_code_simple == plot_code_simple))) {
      next
    }

    # If plot_code_simple is a vector of multiple codes,
    # only check the plots in the vector (regardless of 'continue_loop_plots')

    if (!is.null(plot_code_simple) &&
        length(plot_code_simple) > 1 &
        !(plot_code_simple_i %in% plot_code_simple)) {
      next
    }


    # If all plots do not have to be looped (i.e., just show the results
    # for the input plot_code_simple)

    if (continue_loop_plots == FALSE) {

      assertthat::assert_that(!identical(plot_code_simple, NULL),
                              msg = paste0("Please specify 'plot_code_simple' ",
                                           "for which you want to see the ",
                                           "results."))

      if (!identical(plot_code_simple_i, plot_code_simple)) {
        next
      }
    }

    # Soil compartment ("all", "belowground" or "aboveground")

    compartment <- match.arg(compartment)


    # 1. General plot info ----

    cat("\n\n---------------------------\n")
    cat(paste0(plot_code_simple_i, "\n"))
    cat("---------------------------\n\n")


    # Assert that the source files exist in the Global Environment

    assert_that(exists("df_layers", envir = globalenv()),
                msg = paste0("Dataframe 'df_layers' was not found in the ",
                             "Global Environment. Please run functions ",
                             "'get_app_data()' and 'app_data_long()'."))

    assert_that(exists("df_plot", envir = globalenv()),
                msg = paste0("Dataframe 'df_plot' was not found in the ",
                             "Global Environment. Please run functions ",
                             "'get_app_data()' and 'app_data_long()'."))

    assert_that(exists("df_sampling_points", envir = globalenv()),
                msg = paste0("Dataframe 'df_sampling_points' was not found ",
                             "in the Global Environment. Please run functions ",
                             "'get_app_data()' and 'app_data_long()'."))

    df_layers_i <- get("df_layers", envir = globalenv()) %>%
      filter(plot_code_simple == plot_code_simple_i)

    df_plot_i <- get("df_plot", envir = globalenv()) %>%
      filter(plot_code_simple == plot_code_simple_i)

    df_sampling_points_i <- get("df_sampling_points", envir = globalenv()) %>%
      filter(plot_code_simple == plot_code_simple_i)

    cat(paste0("Site type:           ", df_plot_i$site_type, "\n"))
    cat(paste0("WRB Ref Soil Group:  ", df_plot_i$wrb_ref_soil_group, "\n"))
    cat(paste0("Humus form:          ", df_plot_i$humus_form, "\n\n"))

    cat(paste0("General remarks:\n-", df_plot_i$other_remarks, "\n-",
               df_plot_i$other_observtn, "\n\n"))

    # 2. Above-ground ----
    #    (Forest floor)

    if (compartment %in% c("all", "aboveground")) {

      # TO DO: elaborate for forest floor


    } # End of "aboveground"




    # 3. Below-ground ----

    if (compartment %in% c("all", "belowground")) {

      ## Coarse fragments ----

      cat("\nCOARSE FRAGMENTS\n\n")

      cat(paste0("Notes: ",
                 df_plot_i$P1_other_remarks_fragment, "\n\n"))

      df_layers_i %>%
        select(code_layer, P1_coaf_2mm, P1_coaf_50mm) %>%
        filter(grepl("^M", code_layer)) %>%
        print(n = Inf)


      ## Bedrock depth ----

      cat("\n\nBEDROCK DEPTHS\n\n")

      df_layers_i[1, ] %>%
        select(-"depth_bedrock_compiled") %>%
        select(contains("depth_bedrock")) %>%
        pivot_longer(everything(),
                     names_to = "name",
                     values_to = "value") %>%
        mutate(line = paste0(name, ": ", value)) %>%
        pull(line) %>%
        cat(sep = "\n")

      cat(paste0("Bedrock depths: ",
                 df_layers_i$depth_bedrock_compiled[1], "\n"))


      ## Remarks disturbed samples/properties ----
      # These do also sometimes contain information about depth or stoniness

      cat("\n\nREMARKS DISTURBED / PROPERTIES\n\n")

      df_sampling_points_i %>%
        mutate(line = paste0(p_id, "_notes_dist_samples: ", notes_dist)) %>%
        pull(line) %>%
        cat(sep = "\n")

      df_layers_i %>%
        filter(grepl("^M", code_layer)) %>%
        mutate(line = paste0(tolower(code_layer), "_remarks_dist: ",
                             remarks_sample_dist)) %>%
        pull(line) %>%
        cat(sep = "\n")

      df_layers_i %>%
        filter(grepl("^M", code_layer)) %>%
        mutate(line =
                 paste0(tolower(code_layer), "_dist_check: ", dist_check)) %>%
        pull(line) %>%
        cat(sep = "\n")

      df_layers_i %>%
        select(code_layer, properties_five_pts) %>%
        filter(grepl("^M", code_layer)) %>%
        print(n = Inf)

      ## Bulk density ----

      cat("\n\nBULK DENSITY\n\n")

      df_layers_i %>%
        select(code_layer, bulk_den, count_rings,
               undist_sampling_points, vol_ring,
               P1_num_M36_undist_samples,
               P1_num_M61_undist_samples) %>%
        filter(grepl("^M", code_layer)) %>%
        rename_with(~ gsub("_samples", "", .x)) %>%
        mutate(
          # Quick bulk density calculation for the total (incl. stones etc)
          # and field-moist soil
          bulk_density_moist =
            round(1E-3 * bulk_den / # convert from g to kg
                    (count_rings * vol_ring * 1E-6)) # convert from cm3 to m3
          ) %>%
        print(n = Inf)

      cat(paste0("\nBulk density remark: ",
                 df_plot_i$remarks_bulk_den_sample, "\n\n"))


      df_sampling_points_i %>%
        filter(p_id %in% paste0("P", 1:5)) %>%
        mutate(line = paste0(p_id, "_notes_bulk_den: ", notes_bulk_den)) %>%
        pull(line) %>%
        cat(sep = "\n")

      df_sampling_points_i %>%
        filter(p_id %in% paste0("P", 1:5)) %>%
        mutate(line = paste0(p_id, "_reason_bulk_den: ", reason_bulk_den)) %>%
        pull(line) %>%
        cat(sep = "\n")

      df_layers_i %>%
        filter(grepl("^M", code_layer)) %>%
        mutate(line =
                 paste0(tolower(code_layer), "_bulk_den_check: ",
                        bulk_den_check)) %>%
        pull(line) %>%
        cat(sep = "\n")

    } # End of "belowground"


    cat(paste0("\n\n", plot_code_simple_i, "\n\n"))

    if (continue_loop_plots == TRUE) {

      # Pause and let the user indicate when to continue the loop
      ans <- readline(prompt = "Type 'x' to continue, 'q' to quit: ")
      if (ans == "q") break
    }




  } # End of for loop





} # End of function



