
# This script makes a graph with the results of the interlab comparison
# of the central labs at VUK and INBO


# Interlaboratory comparison WILDCARD

data_vuk <- read_excel(paste0("~/WILDCARD/Lab protocol/Interlab comparison/",
                              "Tahsin_Circular samples results_2025 - ",
                              "comparison.xlsx")) %>%
  select(sample, method,
         `C (%)`, `N (%)`, `calculated anorganic carbon (%)`) %>%
  pivot_longer(
    cols = -c(sample, method),
    names_to = "vuk_col",
    values_to = "vuk_value"
  ) %>%
  mutate(
    vuk_value = case_when(
      method == "TOC" & vuk_col %in% c("C (%)", "N (%)") ~ NA_real_,
      TRUE ~ vuk_value))

data_rt <- suppressMessages(
  read_excel(paste0("~/WILDCARD/Lab protocol/Interlab comparison/",
                    "Tahsin_Circular samples results_2025 - ",
                    "comparison.xlsx"),
             sheet = "Comparison VUK - INBO",
             skip = 18)) %>%
  slice(-1) %>%                     # keep everything but the first row
  # give every column an explicit, unique name
  rename(
    sample     = ...1,
    uitvoerder = ...2,
    TC_XINBO  = `TC (%DS)`,   # columns 3‒5 belong to TC
    TC_SDINBO = ...4,
    TC_XRT    = ...5,
    TN_XINBO  = `TN (%DS)`,   # columns 6‒8 belong to TN
    TN_SDINBO = ...7,
    TN_XRT    = ...8,
    TIC_XINBO = `TIC (%DS)`,  # columns 9‒11 belong to TIC
    TIC_SDINBO = ...10,
    TIC_XRT    = ...11
  ) %>%
  select(-uitvoerder, -`...12`) %>%
  # go from “wide‑ish” to the desired long structure
  pivot_longer(
    cols = -c(sample),
    names_to  = c("parameter", "stat"),     # split at the first “_”
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(                              # give each stat its own column
    names_from  = stat,
    values_from = value
  ) %>%
  mutate(across(c(XINBO, SDINBO, XRT), as.numeric))



# Map parameter names in both datasets
param_map <- tibble(
  parameter = c("TC", "TN", "TIC"),
  vuk_col   = c("C (%)", "N (%)", "calculated anorganic carbon (%)")
)


# Join everything together
plot_data <- data_rt %>%
  left_join(param_map, by = "parameter") %>%
  left_join(data_vuk, by = c("sample", "vuk_col")) %>%
  mutate(
    XRT = case_when(
      !is.na(vuk_value) & is.na(XRT) ~ 0,
      TRUE ~ XRT)) %>%
  filter(!is.na(vuk_value))



p <- ggplot(plot_data, aes(x = XRT)) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  geom_point(aes(y = XINBO,
                 color = "INBO"),
             alpha = 0.2) +
  geom_errorbar(aes(ymin = XINBO - SDINBO,
                    ymax = XINBO + SDINBO,
                    color = "INBO"),
                width = 0.1,
                alpha = 0.2) +
  geom_point(aes(y = vuk_value,
                 color = "VUK"),
             alpha = 0.2) +
  ggrepel::geom_text_repel(
    data = plot_data %>%
      group_by(sample, parameter) %>%
      slice(1) %>%
      ungroup(),
    aes(y = vuk_value, label = sample),
    size = 3,
    alpha = 0.6,
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2,
    segment.color = "grey50"
  ) +
  # geom_text(aes(y = vuk_value, label = sample), hjust = -0.1, vjust = 0,
  #           size = 3, alpha = 0.5) +
  facet_wrap(~parameter,
             scales = "free") +
  scale_color_manual(values = c("INBO" = "blue",
                                "VUK" = "red")) +
  labs(
    x = "Reference value (average ring test)",
    y = "Measurement by\nWILDCARD central labs",
    color = "Data source",
    title = "Comparison of INBO and VUK data per parameter"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))






sample_info <- tribble(
  ~sample,  ~description,
  "FSCC4B",
  paste0("4th FSCC-SoilRT SampleB: sandy loam soil ",
         "from Gontrode, Belgium (ball-milled)"),
  "FSCC6A", "6th FSCC-SoilRT SampleA: loamy soil from Slovakia (ball-milled)",
  "FSCC6D", "6th FSCC-SoilRT SampleD: forest floor from Deurne, Belgium",
  "ISE839", "Wepal: Calcareous clay (Spain)"
)
library(patchwork)


# Create a text-only "plot" with the sample descriptions
desc_text <- sample_info %>%
  mutate(label = paste(sample, "-", description)) %>%
  pull(label) %>%
  paste(collapse = "\n")

desc_plot <- cowplot::ggdraw() +
  cowplot::draw_text(desc_text,
            x = 0.01,  # Near left edge
            y = 0.99,  # Near top
            hjust = 0, vjust = 1,
            size = 9)  # Size in points

# Combine with patchwork
final_plot <- p / desc_plot + plot_layout(heights = c(5, 3))

ggsave(filename = "comparison.png",
       plot = final_plot,
       path = paste0("./output/lab_quality"),
       dpi = 500,
       width = 6.81)
