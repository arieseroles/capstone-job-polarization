# ==============================================================================
# VISUALIZATION FUNCTIONS FROM CAPSTONE PAPER
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggtext)

# ------------------------------------------------------------------------------
# CONFIGURATION: Define task colors and shapes globally
# ------------------------------------------------------------------------------

TASK_COLORS <- c(
  "NRC" = "#FA8072",  # Salmon
  "NRM" = "#3CB371",  # Medium Sea Green
  "RC"  = "#00CED1",  # Dark Turquoise
  "RM"  = "#DA70D6"   # Orchid
)

TASK_SHAPES <- c(
  "NRC" = 22,  # Square
  "NRM" = 24,  # Triangle
  "RC"  = 23,  # Diamond
  "RM"  = 21   # Circle
)

# ------------------------------------------------------------------------------
# FUNCTION 1: plot_trend_indexed()
# ------------------------------------------------------------------------------
# Creates a line plot showing indexed trends over time (baseline = 100)
#
# REQUIRED DATA STRUCTURE:
#   - year: numeric, years of observation
#   - HA_2006: numeric, indexed value (first year = 100)
#   - group_var: factor/character, grouping variable (e.g., "dominant_task")
#   - facet_var: factor/character (optional), for faceting
#
# PARAMETERS:
#   @param data: data.frame with columns: year, HA_2006, and group_var
#   @param group_var: string, name of grouping variable column
#   @param facet_var: string, name of faceting variable (NULL = no facets)
#   @param y_label: string, label for y-axis
#   @param facet_labels: named vector for facet labels (optional)
#   @param baseline_year: numeric, year to use as baseline (default 2006)
#   @param year_breaks: numeric, spacing between x-axis breaks (default 3)
#   @param facet_nrow: numeric, number of rows for facets (default 3)
#   @param colors: named vector of colors (defaults to TASK_COLORS)
#   @param shapes: named vector of shapes (defaults to TASK_SHAPES)
#
# EXAMPLE:
#   plot_trend_indexed(
#     data = dt,
#     group_var = "dominant_task",
#     y_label = "Indexed Employment Level\n(2006 = 100)"
#   )
# ------------------------------------------------------------------------------

plot_trend_indexed <- function(data,
                               group_var,
                               facet_var = NULL,
                               y_label = "Indexed Value (Baseline = 100)",
                               facet_labels = NULL,
                               baseline_year = 2006,
                               year_breaks = 3,
                               facet_nrow = 3,
                               colors = TASK_COLORS,
                               shapes = TASK_SHAPES) {
  
  # Filter out NA values in grouping variable
  data <- data %>%
    filter(!is.na(.data[[group_var]]) & .data[[group_var]] != "NA")
  
  # Base plot
  p <- ggplot(data, aes(x = year, y = HA_2006)) +
    geom_hline(yintercept = 100, linetype = "solid", color = "black") +
    geom_line(aes(group = .data[[group_var]], color = .data[[group_var]]),
              linewidth = 0.6) +
    geom_point(aes(shape = .data[[group_var]], fill = .data[[group_var]]),
               size = 2.5, color = "black") +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = year_breaks),
                       expand = c(0.05, 0.05)) +
    scale_y_continuous(expand = c(0.05, 0.05)) +
    scale_shape_manual(values = shapes, guide = "legend") +
    scale_fill_manual(values = colors, guide = "legend") +
    scale_color_manual(values = colors, guide = "legend") +
    labs(x = "Year", y = y_label,
         fill = str_to_title(gsub("_", " ", group_var)),
         shape = str_to_title(gsub("_", " ", group_var)),
         color = str_to_title(gsub("_", " ", group_var))) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "inches"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 11),
      legend.box = "vertical",
      legend.justification = "left",
      panel.border = element_rect(color = "grey30", linewidth = 1)
    ) +
    guides(
      color = guide_legend(title = str_to_title(gsub("_", " ", group_var)), nrow = 1),
      fill = guide_legend(title = str_to_title(gsub("_", " ", group_var)), nrow = 1),
      shape = guide_legend(title = str_to_title(gsub("_", " ", group_var)), nrow = 1)
    )
  
  # Add faceting if specified
  if (!is.null(facet_var)) {
    if (!is.null(facet_labels)) {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)), 
                          nrow = facet_nrow,
                          labeller = labeller(.default = facet_labels))
    } else {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)), nrow = facet_nrow)
    }
    p <- p + theme(
      panel.spacing = unit(1, "lines"),
      strip.text = element_text(size = 10)
    )
  }
  
  return(p)
}

# ------------------------------------------------------------------------------
# FUNCTION 2: plot_arrow_change()
# ------------------------------------------------------------------------------
# Creates an arrow plot showing change between start and end years
#
# REQUIRED DATA STRUCTURE:
#   - Must have columns for start_year and end_year values
#   - group_var: factor/character, grouping variable (e.g., "dominant_task")
#   - Optional: facet_var for faceting
#
# PARAMETERS:
#   @param data: data.frame with start/end year columns
#   @param group_var: string, name of grouping variable
#   @param start_col: string, column name for start year values
#   @param end_col: string, column name for end year values
#   @param start_year: numeric, label for start year (default 2006)
#   @param end_year: numeric, label for end year (default 2021)
#   @param x_label: string, x-axis label
#   @param y_label: string, y-axis label
#   @param facet_var: string, name of faceting variable (NULL = no facets)
#   @param reverse_y: logical, reverse y-axis order (default TRUE)
#   @param use_markdown: logical, use markdown formatting for y-axis (default FALSE)
#   @param y_labels: named vector for custom y-axis labels (optional)
#   @param show_pct: logical, show percentage in labels (default TRUE)
#
# EXAMPLE:
#   plot_arrow_change(
#     data = dt_delta,
#     group_var = "dominant_task",
#     start_col = "share_2006",
#     end_col = "share_2021",
#     x_label = "Share in total employment (%)"
#   )
# ------------------------------------------------------------------------------

plot_arrow_change <- function(data,
                              group_var,
                              start_col,
                              end_col,
                              start_year = 2006,
                              end_year = 2021,
                              x_label = "Value",
                              y_label = NULL,
                              facet_var = NULL,
                              reverse_y = TRUE,
                              use_markdown = FALSE,
                              y_labels = NULL,
                              show_pct = TRUE) {
  
  # Filter out NA values
  plot_data <- data %>%
    filter(!is.na(.data[[group_var]]) & .data[[group_var]] != "NA")
  
  # Calculate change
  plot_data$change <- plot_data[[end_col]] - plot_data[[start_col]]
  plot_data$direction_increase <- plot_data$change > 0
  
  # Create change label
  if (show_pct) {
    plot_data$change_label <- paste0(
      ifelse(plot_data$change > 0, "+", ""),
      round(plot_data$change, 1), "%"
    )
  } else {
    plot_data$change_label <- paste0(
      ifelse(plot_data$change > 0, "+", ""),
      round(plot_data$change, 1)
    )
  }
  
  plot_data$label_hjust <- ifelse(plot_data$change >= 0, -0.2, 1.2)
  
  # Reverse factor order if specified
  if (reverse_y) {
    plot_data[[group_var]] <- factor(
      plot_data[[group_var]],
      levels = rev(unique(plot_data[[group_var]]))
    )
  }
  
  # Get first row for year labels
  label_row <- plot_data[1, ]
  
  # Base plot
  p <- ggplot(plot_data, aes(y = .data[[group_var]])) +
    geom_segment(
      aes(x = .data[[start_col]], xend = .data[[end_col]],
          yend = .data[[group_var]], color = direction_increase),
      arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 40),
      linewidth = 1.2
    ) +
    geom_point(aes(x = .data[[start_col]]), shape = 124, size = 4, color = "black") +
    geom_text(
      aes(x = .data[[end_col]], label = change_label, hjust = label_hjust),
      vjust = 0.5, size = 3.5
    ) +
    geom_text(
      data = label_row,
      aes(x = .data[[start_col]], y = .data[[group_var]], label = as.character(start_year)),
      vjust = -1.5, hjust = 0.5, size = 3.5, color = "grey"
    ) +
    geom_text(
      data = label_row,
      aes(x = .data[[end_col]], y = .data[[group_var]], label = as.character(end_year)),
      vjust = -1.5, hjust = 0.5, size = 3.5, color = "grey"
    ) +
    scale_color_manual(
      values = c("TRUE" = "darkgreen", "FALSE" = "red"),
      guide = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    labs(x = x_label, y = y_label) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "inches"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  # Apply custom y-axis labels if provided
  if (!is.null(y_labels)) {
    p <- p + scale_y_discrete(labels = y_labels)
  }
  
  # Use markdown if specified
  if (use_markdown) {
    p <- p + theme(axis.text.y = element_markdown(size = 9))
  }
  
  # Add faceting if specified
  if (!is.null(facet_var)) {
    p <- p + facet_grid(as.formula(paste(facet_var, "~ .")),
                        scales = "free_y", space = "free_y")
  }
  
  return(p)
}

# ==============================================================================
# END OF FUNCTIONS
# ==============================================================================