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
#' @param pval_cap Numeric value indicating the maximum p-value to plot. All p-values above this value will be plotted at this value with an asterisk indicating they are capped.
#' @param legend.position Character value indicating the position of the legend. Can be "top", "bottom", "left", "right", or "none". 
#' @param sig_lines_at Numeric vector indicating the significance levels (i.e. p-values) to mark on the plot.
#' @param sig_line_type Character value indicating the type of line to use for the significance lines (should be the same length as `sig_lines_at` or a multiple). Can be "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash".
#' @param sig_line_labels Character vector indicating the labels to use for the significance lines.
#' @param sig_line_color Character value indicating the color to use for the significance lines.
#' @param sig_line_alpha Numeric value indicating the alpha transparency to use for the significance lines.
#' @param show_strip.x.text Logical indicating whether to show the x-axis facet label text inside the colored rectangles associated with `cols`.
#' @param show_strip.y.text Logical indicating whether to show the y-axis facet label text inside the colored rectangles associated with `rows`.
#' @param show_strip.y.background Logical indicating whether to print rectangles for the y-axis facets associated with `rows`.
#' @param show_strip.x.background Logical indicating whether to print rectangles for the x-axis facets associated with `cols`.
#' @param remove.x.labels Logical indicating whether to remove the x-axis labels.
#' @param remove.y.labels Logical indicating whether to remove the y-axis labels.
#' @param remove.panel.grid Logical indicating whether to remove the panel grid.
#' @param ... Other arguments passed on to `ggplot`. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#' 
#' @details This function creates a matrix barplot and expects `x` (e.g. a gene or metabolite) and `y` (a numeric value defining the height of the bars, e.g. -log10 p-value) to be defined in the mapping.
#' Other parameter defaults are tuned to work well with p-values (y-axis) and fold change statistics (mapped to the color aesthetic).
#' The colors of the bars are defined by the `color` aesthetic.
#' 
#' Fill colors for the categories along the x-axis unless there are more than 12 categories.
#' In this case, or when category labels are desired in lieu of colors, the `label` aesthetic can be used.
#' 
#' When `sig_lines_*` are `NULL`, `matrix_barplot` will attempt to guess appropriate significance levels, line attributes based on the minimum value of `y`.
#' If the length of `sig_line_` `type`, `labels` and `color` are not equal to the length of `sig_lines_at`, `matrix_barplot` will recycle the values provided.
#' To disable plotting of significance lines, set `sig_lines_at = NA`. Note that `sig_line_color` must be a single value.
#' 
#' @return A ggplot2 object
#' @export
#' @importFrom deeptime facet_grid_color
#' @importFrom dplyr select
#' @importFrom ggplot2 element_text element_rect geom_hline geom_segment geom_vline ggplot labs scale_alpha_manual scale_color_gradient2 scale_linetype_manual scale_x_discrete scale_y_discrete theme
#' @importFrom stringr str_replace
matrix_barplot <- function(data = NULL, mapping = NULL, 
                           rows = vars(), cols = vars(), facet_colors = deeptime::stages,
                           orientation = 'landscape', switch = FALSE,
                           pval_cap = 1e-5, legend.position = 'top',
                           sig_lines_at = NULL, sig_line_type = NULL, sig_line_labels = NULL,
                           sig_line_color = 'grey80', sig_line_alpha = NULL,
                           show_strip.x.text = FALSE, show_strip.y.text = FALSE,
                           show_strip.y.background = orientation == 'portrait',
                           show_strip.x.background = orientation == 'landscape', 
                           remove.x.labels = TRUE, remove.y.labels = TRUE,
                           remove.panel.grid = TRUE, ...)
{
  # for those pesky no visible binding errors
  if(FALSE)
    x <- y <- type_label <- at <- alpha <- type <- NULL
  
  # translate `switch` to work with `facet_grid_color`
  if(!switch)
  {
    switch_translation <- c('both')
  }else{
    switch_translation <- NULL
  }
  
  # if p-values are not capped, set cap at minimum p-value
  if(is.na(pval_cap) | is.null(pval_cap))
  {
    if(orientation == 'landscape')
      pval_cap <- min(data[[as_label(mapping$y)]], na.rm = TRUE)
    else
      pval_cap <- min(data[[as_label(mapping$x)]], na.rm = TRUE)
  }
  
  
  # significance lines
  if(is.null(sig_lines_at))
    sig_lines_at <- c(-log10(0.05), seq(from = 2, to = -log10(pval_cap))) # this will truncate at `floor(log10(pval_cap)))`
  
  if(is.null(sig_line_type))
    sig_line_type <- 'solid'
  
  if(is.null(sig_line_alpha))
    sig_line_alpha <- c(1, rep(0.4, length(sig_lines_at) - 1))
  
  if(is.null(sig_line_labels))
    sig_line_labels <- c('p = 0.05', rep('-log10(p) = 2, 3, ...', length(sig_lines_at) - 1))
  
  siglines <- data.frame(at = c(sig_lines_at, -sig_lines_at),
                         type_label = rep(sig_line_labels, length(sig_lines_at))[1:length(sig_lines_at)],
                         type       = rep(sig_line_type  , length(sig_lines_at))[1:length(sig_lines_at)],
                         alpha      = rep(sig_line_alpha , length(sig_lines_at))[1:length(sig_lines_at)]) |>
    mutate(type_label = factor(type_label, levels = unique(type_label)))
  
  
  # main figure
  if(orientation == 'landscape')
  {
    retval <- ggplot(data, mapping, ...)

    # add significance lines (if they are not NA / turned off)    
    if(!any(is.na(sig_lines_at)))
      retval <- retval + 
        geom_hline(data = siglines, mapping = aes(yintercept = at, linetype = type_label, alpha = type_label), 
                   color = sig_line_color) +
        scale_linetype_manual(values = unique(select(siglines, type_label, type))$type,
                              labels = unique(select(siglines, type_label, type))$type_label,
                              aesthetics = 'linetype',
                              name = 'Significance') +
        scale_alpha_manual(values = unique(select(siglines, type_label, alpha))$alpha,
                           labels = unique(select(siglines, type_label, alpha))$type_label,
                           aesthetics = 'alpha',
                           name = 'Significance')
      
    retval <- retval +
      geom_segment(stat = "centerBar") +
      
      # add facet grid with custom colors
      facet_grid_color(cols = cols, rows = rows, colors = facet_colors, 
                       switch = switch_translation, scales = 'free_x', space = 'free')
      
  }else if(orientation == 'portrait'){
    retval <- ggplot(data, mapping, ...)
      
    # add significance lines (if they are not NA / turned off)    
    if(!any(is.na(sig_lines_at)))
      retval <- retval + 
        geom_vline(data = siglines, mapping = aes(xintercept = at, linetype = type_label, alpha = type_label), 
                   color = sig_line_color) +
        scale_linetype_manual(values = unique(select(siglines, type_label, type))$type,
                              labels = unique(select(siglines, type_label, type))$type_label,
                              aesthetics = 'linetype',
                              name = 'Significance') +
        scale_alpha_manual(values = unique(select(siglines, type_label, alpha))$alpha,
                           labels = unique(select(siglines, type_label, alpha))$type_label,
                           aesthetics = 'alpha',
                           name = 'Significance')
    
    retval <- retval +      
      geom_segment(stat = "centerBar2") +
      
      # add facet grid with custom colors
      facet_grid_color(cols = cols, rows = rows, colors = facet_colors, 
                       switch = switch_translation, scales = 'free_y', space = 'free')
      
  }else{
    stop('orientation must be either "landscape" or "portrait"')
  }
  
  
  # check that we don't have multiple rows or columns plotted to the same position
  data_check <- select(data,
                       unlist(retval$labels)[c('x', 'y')],       # pull variables called out in `mapping`
                       str_replace(as.character(cols), '~', ''), # pull `cols` variables
                       str_replace(as.character(rows), '~', '')) # pull `rows` variables

     # we should have the same number of unique rows for `x` and `y` (i.e. no duplicate `x` or `y` for any panel)
  if(nrow(unique(select(data_check, x, str_replace(as.character(cols), '~', ''), str_replace(as.character(rows), '~', '')))) !=
     nrow(unique(select(data_check, y, str_replace(as.character(cols), '~', ''), str_replace(as.character(rows), '~', '')))))
    stop('multiple rows or columns plotted to the same position. Did you forget to define `rows` or `cols`?')
   
  
  # add fold change color scale and legend
  retval <- retval +
    theme(legend.position = legend.position) +
    scale_color_gradient2(low = 'blue', high = 'red', mid = 'grey50')


  # remove labels
  if(remove.x.labels)
    retval <- retval + scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = NULL)
  if(remove.y.labels)
    retval <- retval + scale_y_discrete(labels = NULL, breaks = NULL) + labs(y = NULL)
  
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
