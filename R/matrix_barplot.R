#' matrix_barplot
#' Creation of a matrix barplot using ggplot2
#' 
#' @param data The data to be displayed in this layer.
#' @param mapping Set of aesthetic mappings created by `aes`.
#' @param cols A set of aesthetic mappings for the columns of the matrix barplot. Columns will be sorted alphabetically for character variables and numerically for factors.
#' @param rows A set of aesthetic mappings for the rows of the matrix barplot. Rows will be sorted alphabetically for character variables and numerically for factors.
#' @param facet_colors Specifies which colors to use to replace the strip backgrounds (i.e. corresponding to the values in the variables specified in `rows` and `cols`. Either A) a function that returns a color for a given strip label, B) the character name of a function that does the same, C) a named character vector with names matching strip labels and values indicating the desired colors, or D) a data.frame representing a lookup table with columns named "name" (matching strip labels) and "color" (indicating desired colors).
#' @param orientation Character value defining the orientation of the plot. Can be "landscape" or "portrait".
#' @param switch Logical. By default (`switch = FALSE`), the labels are displayed on the bottom and/or left of the plot. If `switch = TRUE`, the labels are displayed on the top and/or right of the plot.
#' @param show_strip.x.text Logical indicating whether to show the x-axis facet label text inside the colored rectangles associated with `cols`.
#' @param show_strip.y.text Logical indicating whether to show the y-axis facet label text inside the colored rectangles associated with `rows`.
#' @param show_strip.y.background Logical indicating whether to print rectangles for the y-axis facets associated with `rows`.
#' @param show_strip.x.background Logical indicating whether to print rectangles for the x-axis facets associated with `cols`.
#' @param remove.x.labels Logical indicating whether to remove the x-axis labels.
#' @param remove.y.labels Logical indicating whether to remove the y-axis labels.
#' @param remove.panel.grid Logical indicating whether to remove the panel grid.
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
#' @importFrom deeptime facet_grid_color
#' @importFrom dplyr select
#' @importFrom ggplot2 element_text element_rect geom_segment ggplot labs scale_color_gradient2 scale_x_discrete scale_y_discrete theme
#' @importFrom stringr str_replace
matrix_barplot <- function(data = NULL, mapping = NULL, 
                           rows = vars(), cols = vars(), facet_colors = deeptime::stages,
                           orientation = 'landscape', switch = FALSE,
                           show_strip.x.text = FALSE, show_strip.y.text = FALSE,
                           show_strip.y.background = orientation == 'portrait',
                           show_strip.x.background = orientation == 'landscape', 
                           remove.x.labels = TRUE, remove.y.labels = TRUE,
                           remove.panel.grid = TRUE, ...)
{
  # for those pesky no visible binding errors
  if(FALSE)
    x <- y <- NULL
  
  # translate `switch` to work with `facet_grid_color`
  if(!switch)
  {
    switch_translation <- c('both')
  }else{
    switch_translation <- NULL
  }
  
  # main figure
  if(orientation == 'landscape')
  {
    retval <- ggplot(data, mapping, ...) +
      
      geom_segment(stat = "centerBar") +
      
      # add facet grid with custom colors
      facet_grid_color(cols = cols, rows = rows, colors = facet_colors, 
                       switch = switch_translation, scales = 'free_x', space = 'free')
      
  }else if(orientation == 'portrait'){
    retval <- ggplot(data, mapping, ...) +
      
      geom_segment(stat = "centerBar2") +
      
      # add facet grid with custom colors
      facet_grid_color(cols = cols, rows = rows, colors = facet_colors, 
                       switch = switch_translation, scales = 'free_y', space = 'free')
      
  }else{
    stop('orientation must be either "landscape" or "portrait"')
  }
  
  # check that we don't have multiple rows or columns plotted to the same position
  data_check <- select(data,
                       unlist(retval$labels), # pull variables called out in `mapping`
                       str_replace(as.character(cols), '~', ''), # pull `cols` variables
                       str_replace(as.character(rows), '~', '')) # pull `rows` variables

     # we should have the same number of unique rows for `x` and `y` (i.e. no duplicate `x` or `y` for any panel)
  if(nrow(unique(select(data_check, x, str_replace(as.character(cols), '~', ''), str_replace(as.character(rows), '~', '')))) !=
     nrow(unique(select(data_check, y, str_replace(as.character(cols), '~', ''), str_replace(as.character(rows), '~', '')))))
    stop('multiple rows or columns plotted to the same position. Did you forget to define `rows` or `cols`?')
   
  
  # remove labels
  if(remove.x.labels)
    retval <- retval + scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = NULL)
  if(remove.y.labels)
    retval <- retval + scale_y_discrete(labels = NULL, breaks = NULL) + labs(y = NULL)
  
  retval <- retval +

    # default theme for matrix_barplots
    theme(legend.position = 'top') +
    
    scale_color_gradient2(low = 'blue', high = 'red', mid = 'grey45')
  
  
  # hide the strip text by default
  if(!show_strip.x.text)
    retval <- retval + theme(strip.text.x = element_text(color = 'transparent', size = 0))

  if(!show_strip.y.text)
    retval <- retval + theme(strip.text.y = element_text(color = 'transparent', size = 0))

  # show/hide the strip backgrounds
  if(!show_strip.y.background)
    retval <- retval + theme(strip.background.y = element_rect(fill = 'transparent', color = NA))

  if(!show_strip.x.background)
    retval <- retval + theme(strip.background.x = element_rect(fill = 'transparent', color = NA))
  
  # remove the panel grid
  if(remove.panel.grid)
    retval <- retval + theme(panel.grid = element_blank())
  
  retval
}


#' stat_centerBar
#' A new stat for ggplot2 that creates a barplot or line segments with bars centered around the x-axis
#' 
#' @param mapping Set of aesthetic mappings created by `aes`.
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlappling points on this layer
#' @param na.rm A logical indicating whether NA values should be removed from the data before plotting
#' @param show.legend A logical indicating whether this layer should be included in the legend
#' @param inherit.aes A logical indicating whether to inherit the default aesthetics
#' @param ... Other arguments passed on to `layer`
#' 
#' @details For specifics on creating a new stat, see [this documentation](https://bookdown.org/rdpeng/RProgDA/building-new-graphical-elements.html#example-normal-confidence-intervals)
#' 
#' @return A ggproto object
#' @export
#' @importFrom ggplot2 layer
stat_centerBar <- function(mapping = NULL, data = NULL, geom = "segment",
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = matrixBP::StatCenterBar, # StatCenterBar defined in data-raw/StatCenterBar.R
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' stat_centerBar2
#' A new stat for ggplot2 that creates a barplot or line segments with bars centered around the y-axis
#' @rdname stat_centerBar
stat_centerBar2 <- function(mapping = NULL, data = NULL, geom = "segment",
                           position = "identity", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = matrixBP::StatCenterBar2, # StatCenterBar2 defined in data-raw/StatCenterBar.R
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
