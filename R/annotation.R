#' mBP_legend
#' Add matrix barplot legend to a plot
#' 
#' @param g A ggplot object as returned from `matrix_barplot`.
#' @param legend.position A character string indicating the position of the legend. Default is "bottom".
#' @param ... Parameters passed to `patchwork::plot_layout` for defining the plot layout. See ?plot_layout for more information.
#' 
#' @note This function will throw a warning about removing rows containing missing values or values outside the scale range due to some hidden features that are not plotted.
#' 
#' @return A ggplot object with a legend added.
#' @export
#' @importFrom cowplot get_plot_component
#' @importFrom ggplot2 aes as_label geom_rect ggplot ggplot_build scale_fill_manual theme theme_void
#' @importFrom patchwork area plot_layout wrap_elements
mBP_legend <- function(g, legend.position = 'bottom', ...)
{
  # take care of pesky no visible binding for global variables warnings
  if(FALSE)
      row_grp <- x <- y <- xend <- NULL

  # this is where we'll find most of the legend details
  color_env <- environment(ggplot_build(g)$layout$facet_params$colors)
  
  # fetch relevant information
  labs <- get('name', color_env)
  colors <- get('color', color_env)
  
  # set up the legend
  lgnd <- data.frame(x = 1, y = 1, row_grp = factor(labs, levels = labs)) |>
    
    # this will be a blank plot
    ggplot(aes(x = x, y = y, fill = row_grp)) +
    geom_rect(aes(xmin = x-0.5, xmax = x+0.5, ymin = y-0.5, ymax = y+0.5, fill = row_grp)) +
    scale_fill_manual(values = colors) +
    
    # get the label from `g`
    labs(fill = as_label(ggplot_build(g)$layout$facet$params$cols[[1]])) +
    
    theme_void() +
    theme(legend.position = legend.position)
  
  # pull the legend from the plot
  if(is.null(legend.position))
  {
    lgnd <- get_plot_component(lgnd, 'guide-box')
  }else{
    lgnd <- paste('guide-box', legend.position, sep = '-') |>
      get_plot_component(plot = lgnd)
  }

  # add the legend to the plot
  if(legend.position == 'bottom')
  {
    # if they provided values for plot_layout, use them
    dots <- list(...)
    if(length(dots) == 0)
      dots$heights <- c(1, 0.2) # otherwise use the default
    
    retval <- g / lgnd + do.call(plot_layout, dots)
  }else if(legend.position == 'right'){
    
    # if they provided values for plot_layout, use them
    dots <- list(...)
    if(length(dots) == 0)
      dots$widths <- c(1, 0.2) # otherwise use the default
    
    retval <- g + lgnd + do.call(plot_layout, dots)
  }else if(legend.position == 'left')
  {
    # if they provided values for plot_layout, use them
    dots <- list(...)
    if(length(dots) == 0)
      dots$widths <- c(0.2, 1) # otherwise use the default
    
    retval <- wrap_elements(lgnd) + g + do.call(plot_layout, dots)
  }else if(legend.position == 'top')
  {
    # if they provided values for plot_layout, use them
    dots <- list(...)
    if(length(dots) == 0)
      dots$heights <- c(0.2, 1) # otherwise use the default
    
    retval <- wrap_elements(lgnd) / g + do.call(plot_layout, dots)
  }else{
    stop('Invalid legend position. Must be one of "bottom", "right", "left", or "top".')
  }

  return(retval)
}


#' mBP_col_labels
#' Add matrix barplot labels to a plot
#' 
#' @param g A ggplot object as returned from `matrix_barplot`.
#' @param margin A numeric value to adjust the width of the margin
#' 
#' @return A ggplot object with labels added.
#' @export
#' @importFrom dplyr filter group_by mutate ungroup case_when summarize
#' @importFrom ggh4x facetted_pos_scales
#' @importFrom ggplot2 aes as_label coord_cartesian element_blank element_text facet_grid geom_text ggplot ggplot_build labs scale_color_manual scale_x_continuous scale_x_discrete scale_y_continuous scale_y_discrete theme vars
#' @importFrom patchwork area plot_layout
#' @importFrom stats median
mBP_col_labels <- function(g, margin = 3)
{
  # take care of those pesky no visible binding for global variables warnings
  if(FALSE)
    grp <- just <- lbl <- midpoint <- PANEL <- x <- y <-
      xend <- yend <- colour <- NULL
  
  # this is where we'll find the facet label information we need
  plot_env <- ggplot_build(g)$layout$facet_params$plot_env
  
  # fetch column labels
  cols <- as_label(ggplot_build(g)$layout$facet_params$cols[[1]])
  
  if(!is.factor(plot_env$data[[cols]]))
  {
    cols <- factor(plot_env$data[[cols]]) |> 
      levels() |>
      factor() # kind of hacky, but this preserves the order correctly...
  }else{
    tmp <- levels(plot_env$data[[cols]])
    cols <- tmp[tmp %in% plot_env$data[[cols]]] |> # drop missing values if they were filtered from the data set
      factor(levels = tmp)                         # and convert back to a factor with the correct levels
  }
  
  # set up the labels
  col_labels <- ggplot_build(g)$data[[1]] |>
    group_by(PANEL) |>
    mutate(lbl = cols[PANEL],                       # x-axis labels
           midpoint = median(c(x, xend)),           # midpoint of each group
           print = x == round(midpoint),            # which values to print
           y = 1,                                   # y-axis position
           just = case_when(print        ~ 0.5,     # vertical justification 
                            x < midpoint ~ 0,
                            x > midpoint ~ 1,
                            TRUE         ~ NA),
           x = ifelse(print,                        # update x-axis position for labels
                      round(midpoint),
                      x),
           grp = PANEL) |>                          # group number
    ungroup() |>
    
    dplyr::filter(!is.na(lbl))                      # if there are multiple rows/samples, all but the first row will be NA - drop them
  
  # set up panel label ranges
  tmp <- col_labels |>
    group_by(lbl) |>
    summarize(xmin = min(x), xmax = max(x)) |>
    ungroup()
  
  x_scales <- list()
  for(i in 1:nrow(tmp))
  {
    x_scales[[i]] <- scale_x_continuous(limits = c(tmp$xmin[i], tmp$xmax[i]), labels = NULL, breaks = NULL)
  }
  

  col_label_plot <- filter(col_labels, print) |>
    ggplot(aes(x = midpoint, y = y, label = lbl, vjust = just)) +
    
    # print labels (include all values to maintain spacing, but only color one per group)
    geom_text(angle = 90, hjust = 1, size = 3) +

    facet_grid(cols = vars(grp), rows = vars(), space = 'free', scales = 'free_x', switch = 'both') +
    coord_cartesian(clip = 'off') +
    facetted_pos_scales(x = x_scales) +

    # turn off all labels, axes, and facet grid annotations
    scale_x_discrete(labels = NULL, breaks = NULL) +
    scale_y_discrete(labels = NULL, breaks = NULL) +
    labs(x = NULL, y = NULL) +
    
    theme(legend.position = 'none',
          strip.text = element_text(color = 'transparent', size = 0),
          strip.background = element_blank())
  
  # add the labels to the plot   
  layout <- c(area(t =  1,          l = 1, b = 20 - margin, r = 1),
              area(t = 20 - margin, l = 1, b = 20,          r = 1))
  
  g + col_label_plot + plot_layout(design = layout)
}


#' mBP_row_labels
#' Add matrix barplot row labels to a plot
#' @rdname mBP_col_labels
#' @export
mBP_row_labels <- function(g, margin = 3)
{
  # take care of those pesky no visible binding for global variables warnings
  if(FALSE)
    grp <- just <- lbl <- midpoint <- PANEL <- x <- y <- x_adj <- hjust <- 
      yend <- colour <- group <- xend <- NULL

    
  # this indicates `switch = TRUE` was passed to `matrix_barplot`
  # when `switch = 'both'`, the row labels are on the left side of the plot, otherwise `switch` will be NULL
  left <- !is.null(ggplot_build(g)$layout$facet_params$switch)
  
  # this is where we'll find the facet label information we need
  plot_env <- ggplot_build(g)$layout$facet_params$plot_env
  
  # fetch row labels
  rows <- as_label(ggplot_build(g)$layout$facet_params$rows[[1]])
  
  ncols <- length(unique(ggplot_build(g)$data[[1]]$PANEL)) / length(unique(plot_env$data[[rows]]))
 
  if(!is.factor(plot_env$data[[rows]]))
  {
    rows <- factor(plot_env$data[[rows]]) |> 
      levels() |>
      factor() # kind of hacky, but this preserves the order correctly...
  }else{
    tmp <- levels(plot_env$data[[rows]])
    rows <- tmp[tmp %in% plot_env$data[[rows]]] |> # drop missing values if they were filtered from the data set
      factor(levels = tmp)                         # and convert back to a factor with the correct levels
  }
  
  # set up the labels
  row_labels <- ggplot_build(g)$data[[1]] |>
    mutate(lbl = rep(rows, each = ncols)[PANEL] |>
             factor(levels = rows)) |>              # y-axis labels
    
    group_by(lbl) |>
    mutate(midpoint = median(c(y,yend)),            # midpoint of each group
           print = y == round(midpoint),            # which values to print
           x = 1,                                   # x-axis position
           just = case_when(print        ~ 0.5,     # horizontal justification 
                            y < midpoint ~ 0,
                            y > midpoint ~ 1,
                            TRUE         ~ NA),
           y = ifelse(print,                        # update y-axis position for labels
                      round(midpoint),
                      y),
           grp = PANEL) |>                          # group number
    ungroup() |>
    
    dplyr::select(-PANEL, -colour, -group, -xend, -grp) |>
    unique()                                        # if there are multiple rows/samples, all but the first row will be NA - drop them
    
  row_labels <- row_labels |> 
    mutate(
      hjust = ifelse(left, 1, 0),
      x_adj = ifelse(left, -0.5, 0.5)
    )
  
  # set up panel labels
  tmp <- group_by(row_labels, lbl) |>
    summarize(ymin = min(y), ymax = max(y))
  
  y_scales <- list()
  for(i in 1:nrow(tmp))
  {
    y_scales[[i]] <- scale_y_continuous(limits = c(tmp$ymin[i], tmp$ymax[i]), labels = NULL, breaks = NULL)
  }
  
  
  row_label_plot <- 
    filter(row_labels, print) |>
    ggplot(aes(x = x + x_adj, y = midpoint, label = lbl, vjust = 0, hjust = hjust)) +
    
    # print labels (include all values to maintain spacing, but only color one per group)
    geom_text(angle = 0, size = 3) +
    #scale_color_manual(values = c('transparent', 'black')) +
    
    facet_grid(cols = vars(), rows = vars(lbl), space = 'free', scales = 'free_y', switch = 'both') +
    coord_cartesian(clip = 'off') +
    facetted_pos_scales(y = y_scales) +
    
    # turn off all labels, axes, and facet grid annotations
    scale_x_discrete(labels = NULL, breaks = NULL) +
    labs(x = NULL, y = NULL) +
    
    theme(legend.position = 'none',
          strip.text = element_text(color = 'transparent', size = 0),
          strip.background = element_blank())
  
  # add the labels to the plot (assuming the legend is at the top - need to work on this some more)
  if(left){
    layout <- c(
      area(t = 1, l = 1, b = 20, r = margin),
      area(t = 1, l = margin, b = 20, r = 20)
    )
    final_plot <- row_label_plot + g + plot_layout(design = layout)
  } else {
    layout <- c(
      area(t = 1, l = 1, b = 20, r = 20),
      area(t = 1, l = 20, b = 20, r = margin + 20)
    )
    final_plot <- g + row_label_plot + plot_layout(design = layout)
  }
  
  return(final_plot)
}
