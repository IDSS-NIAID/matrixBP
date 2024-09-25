#' matrix_barplot
#' Creation of a matrix barplot using ggplot2
#' 
#' @param data The data to be displayed in this layer.
#' @param mapping Set of aesthetic mappings created by `aes`.
#' @param cols A set of aesthetic mappings for the columns of the matrix barplot. Columns will be sorted alphabetically for character variables and numerically for factors.
#' @param rows A set of aesthetic mappings for the rows of the matrix barplot. Rows will be sorted alphabetically for character variables and numerically for factors.
#' @param facet_colors Specifies which colors to use to replace the strip backgrounds (i.e. corresponding to the values in the variables specified in `rows` and `cols`. Either A) a function that returns a color for a given strip label, B) the character name of a function that does the same, C) a named character vector with names matching strip labels and values indicating the desired colors, or D) a data.frame representing a lookup table with columns named "name" (matching strip labels) and "color" (indicating desired colors).
#' @param switch By default, the labels are displayed on the top and right of the plot. If "x", the top labels will be displayed to the bottom. If "y", the right-hand side labels will be displayed to the left. Can also be set to "both".
#' @param show_strip.x.text Logical indicating whether to show the x-axis facet label text inside the colored rectangles associated with `cols`.
#' @param show_strip.y.text Logical indicating whether to show the y-axis facet label text inside the colored rectangles associated with `rows`.
#' @param show_strip.y.background Logical indicating whether to print rectangles for the y-axis facets associated with `rows`.
#' @param ... Other arguments passed on to `ggplot`. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#' 
#' @details This function creates a matrix barplot and expects `x` (e.g. a gene or metabolite) and `y` (a numeric value defining the hight of the bars, e.g. -log10 p-value) to be defined in the mapping.
#' The colors of the bars are defined by the `color` aesthetic.
#' 
#' Fill colors for the categories along the x-axis unless there are more than 12 categories.
#' In this case, or when category labels are desired in lieu of colors, the `label` aesthetic can be used.
#' 
#' @return A ggplot2 object
#' @export
matrix_barplot <- function(data = NULL, mapping = NULL, 
                           rows = vars(), cols = vars(), facet_colors = stages,
                           switch = 'both', show_strip.x.text = FALSE, show_strip.y.text = FALSE,
                           show_strip.y.background = FALSE, ...)
{
  # main figure
  retval <- ggplot(data, mapping, ...) +
    
    geom_segment(stat = "centerBar") +
    
    # remove labels
    scale_x_discrete(labels = NULL, breaks = NULL) +
    scale_y_discrete(labels = NULL, breaks = NULL) +
    labs(x = NULL, y = NULL) +
    
    # add facet grid with custom colors
    facet_grid_color(cols = cols, rows = rows, colors = facet_colors, 
                     switch = 'both', scales = 'free_x', space = 'free') +
  
    # default theme for matrix_barplots
    theme(legend.position = 'top') +
    
    scale_color_gradient2(low = 'blue', high = 'red', midpoint = 0)
  
  
  # hide the strip text by default
  if(!show_strip.x.text)
    retval <- retval + theme(strip.text.x = element_text(color = 'transparent', size = 0))

  if(!show_strip.y.text)
    retval <- retval + theme(strip.text.y = element_text(color = 'transparent', size = 0))

  # hide the y strip background by default
  if(!show_strip.y.background)
    retval <- retval + theme(strip.background.y = element_rect(fill = 'transparent', color = NA))

  
  retval
}


# For specifics on creating a new stat, see [this documentation](https://bookdown.org/rdpeng/RProgDA/building-new-graphical-elements.html#example-normal-confidence-intervals)
stat_centerBar <- function(mapping = NULL, data = NULL, geom = "segment",
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCenterBar, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatCenterBar <- ggproto("StatCenterBar", Stat,
                     compute_group = function(data, scales) {
                         ## Compute the line segment endpoints
                         x <- data$x
                         xend <- data$x
                         y <- data$y
                         yend <- -data$y
                         
                         ## Return a new data frame
                         data.frame(x = x, xend = xend,
                                    y = y, yend = yend)
                       },
                       required_aes = c("x", "y")
)
