#' add_row_boxes
#' Add boxes around rows in a facet grid plot
#' 
#' @param p A ggplot object.
#' @param rows_to_box A character vector with the row facet variables to box. If NULL, all rows will be boxed.
#' @param box_color The color of the box. Default is "black".
#' @param box_linewidth The width of the box line. Default is 1.
#' @param buffer The buffer space between the box and the data points. Default is 0.5.
#' 
#' @return A list of ggplot layers to add to the plot.
#' @importFrom dplyr arrange distinct group_by pull rename ungroup sym summarize semi_join cur_group_id
#' @importFrom forcats fct_inorder
#' @importFrom rlang :=
#' @importFrom utils head tail
add_row_boxes <- function(p, rows_to_box = NULL, box_color = "black", box_linewidth = 1, buffer = 0.5)
{
  # take care of those pesky no visible binding for global variables warnings
  if(FALSE)
    min_y_val <- max_y_val <- ROW <- min_y_all <- max_y_all <- min_y <- max_y <- xmin <- ymin <- ymax <- xmax <- NULL
  
  # Build the plot
  p_build <- ggplot_build(p)
  
  # Extract necessary information from the plot
  facet_params <- p_build$layout$facet_params
  plot_data <- p_build$plot$data
  layout_data <- p_build$layout$layout
  
  # Dynamically get the y variable
  y_var_name <- as_label(p_build$plot$mapping$y)
  
  # Get the names of the row facet variables
  row_facet_vars <- names(facet_params$rows)
  
  # Determine the rows to box
  if (is.null(rows_to_box)) {
    rows_to_box <- unique(plot_data[row_facet_vars])
  } else {
    rows_to_box <- data.frame(rows_to_box) |>
      rename(!!row_facet_vars := rows_to_box)
  }
  
  # Check if the y variable is a character or factor
  if (is.character(plot_data[[y_var_name]]) || is.factor(plot_data[[y_var_name]])) {
    # Get the unique y values in their plotting order, considering row groups
    y_values <- plot_data |>
      group_by(!!!facet_params$rows) |> # Group by row variable(s)
      distinct(!!sym(y_var_name)) |>
      ungroup() |>
      arrange(!!!facet_params$rows, !!sym(y_var_name)) |> # Sort by rows and then y
      pull(!!sym(y_var_name)) |>
      fct_inorder() |>
      levels()
    
    # Calculate y-axis ranges based on the order of the character/factor levels
    ranges <- plot_data |>
      group_by(!!!facet_params$rows) |>
      mutate(
        min_y_val = head(intersect(y_values, !!sym(y_var_name)), 1),
        max_y_val = tail(intersect(y_values, !!sym(y_var_name)), 1),
        min_y_all = match(min_y_val, y_values),
        max_y_all = match(max_y_val, y_values),
        ROW = cur_group_id()
      ) |>
      ungroup()
    
  } else {
    # Existing code to handle numeric y values
    ranges <- plot_data |>
      group_by(!!!facet_params$rows) |>
      mutate(min_y_all = min(!!sym(y_var_name)), max_y_all = max(!!sym(y_var_name)), ROW = cur_group_id()) |>
      ungroup()
  }
  
  # Filter ranges using semi_join for multiple row variables
  ranges <- ranges |>
    semi_join(rows_to_box, by = row_facet_vars)
  
  # Check if ranges is empty after filtering
  if (nrow(ranges) == 0) {
    warning("No rows matched the specified criteria in rows_to_box. No boxes will be drawn.")
    return(ggplot() + theme_void()) # Return an empty plot
  }
  
  
  # Create a data frame to hold the rectangle coordinates
  rect_data <- ranges |>
    group_by(ROW, !!!facet_params$rows) |>
    summarize(
      xmin = -1,
      xmax = 1,
      max_y = min(min_y_all) * -1, # swap ordering to match panels
      min_y = max(max_y_all) * -1,
      .groups = "drop"
    ) |>
    ungroup() |>
    # mutate(PANEL = as.character(ROW)) |> # convert ROW to character
    # # join additional information needed to map panels to plots
    # inner_join(layout_data, by = c("ROW", row_facet_vars)) |> # Corrected join
    mutate(
      ymin = min_y - buffer,
      ymax = max_y + buffer
    )
  
  
  # Create the separate plot for the boxes using annotate
  box_plot <- 
    ggplot(rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    geom_rect(
      color = box_color,
      fill = NA,
      linewidth = box_linewidth
    ) +
    theme_void()
  
  return(box_plot)
}
