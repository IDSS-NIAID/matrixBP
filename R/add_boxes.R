#' add_row_boxes
#' Add boxes around rows in a facet grid plot
#' 
#' @param rows_to_box A character vector with the row facet variables to box. If NULL, all rows will be boxed.
#' @param box_color The color of the box. Default is "black".
#' @param box_linewidth The width of the box line. Default is 1.
#' @param buffer The buffer space between the box and the data points. Default is 0.5.
#' 
#' @return A list of ggplot layers to add to the plot.
#' @export
add_row_boxes <- function(rows_to_box = NULL, box_color = "black", box_linewidth = 1, buffer = 0.5)
{
  
  # Get the ggplot object from the current plot
  p <- ggplot_build(last_plot())
  
  
  # Extract necessary information from the plot
  facet_params <- p$layout$facet_params
  plot_data <- p$plot$data
  layout_data <- p$layout$layout
  
  # Dynamically get the y variable
  y_var_name <- as_label(p$plot$mapping$y)
  
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
        min_y = match(min_y_val, y_values),
        max_y = match(max_y_val, y_values),
        PANEL = factor(cur_group_id())
      ) |>
      ungroup()
    
  } else {
    # Existing code to handle numeric y values
    ranges <- plot_data |>
      group_by(!!!facet_params$rows) |>
      mutate(min_y = min(!!sym(y_var_name)), max_y = max(!!sym(y_var_name)), PANEL = factor(cur_group_id())) |>
      ungroup()
  }

  
  # Filter ranges using semi_join for multiple row variables
  ranges <- ranges |>
    semi_join(rows_to_box, by = row_facet_vars)
  

  # Create a data frame to hold the rectangle coordinates
  rect_data <- ranges |>
    group_by(PANEL, !!!facet_params$rows) |>
    summarize(
      xmin = -Inf,
      xmax = Inf,
      min_y = min(min_y),
      max_y = max(max_y),
    ) |>
    ungroup() |>
    mutate(PANEL = as.character(PANEL)) |> # convert PANEL to character
    # join additional information needed to map panels to plots
    inner_join(layout_data, by = c("PANEL")) |>
    mutate(
      ymin = ifelse(is.character(plot_data[[y_var_name]]) || is.factor(plot_data[[y_var_name]]), min_y - buffer, min_y - buffer),
      ymax = ifelse(is.character(plot_data[[y_var_name]]) || is.factor(plot_data[[y_var_name]]), max_y + buffer, max_y + buffer)
    )
  
  # Create the geom_rect layer with dynamically calculated coordinates
  rect_layer <- geom_rect(
    data = rect_data,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    color = box_color,
    fill = NA,
    linewidth = box_linewidth,
    inherit.aes = FALSE
  )

  #return(rect_layer)  
  
  # Add a layer that implements scales = "free_y" for facet_grid
  scale_layer <- facet_grid(
    rows = facet_params$rows,
    cols = facet_params$cols,
    scales = "free_y"
  )
  
  return(list(rect_layer, scale_layer))
}
