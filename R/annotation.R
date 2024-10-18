#' mBP_legend
#' Add matrix barplot legend to a plot
#' 
#' @param g A ggplot object as returned from `matrix_barplot`.
#' 
#' @note This function will throw a warning about removing rows containing missing values or values outside the scale range due to some hidden featrues that are not plotted.
#' 
#' @return A ggplot object with a legend added.
#' @export
#' @importFrom ggplot2 aes as_label geom_rect ggplot ggplot_build scale_fill_manual theme theme_void
#' @importFrom patchwork area plot_layout
mBP_legend <- function(g)
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
  lgnd <- data.frame(x = NA, y = NA, row_grp = factor(labs, levels = labs)) |>
    
    # this will be a blank plot
    ggplot(aes(x = x, y = y, fill = row_grp)) +
    geom_rect(aes(xmin = x-0.5, xmax = x+0.5, ymin = y-0.5, ymax = y+0.5, fill = row_grp)) +
    scale_fill_manual(values = colors) +
    
    # get the label from `g`
    labs(fill = as_label(ggplot_build(g)$layout$facet$params$cols[[1]])) +
    
    theme_void() +
    theme(legend.position = 'bottom')

  # add the legend to the plot
  layout <- c(area(t = 1, l = 1, b = 19, r = 1),
              area(t = 1, l = 1, b = 20, r = 1))
  
  g + lgnd + plot_layout(design = layout)
}


#' mBP_col_labels
#' Add matrix barplot labels to a plot
#' 
#' @param g A ggplot object as returned from `matrix_barplot`.
#' @param margin A numeric value to adjust the width of the margin
#' 
#' @return A ggplot object with labels added.
#' @export
#' @importFrom dplyr group_by mutate ungroup case_when
#' @importFrom ggplot2 aes as_label coord_cartesian element_blank element_text facet_grid geom_text ggplot ggplot_build labs scale_color_manual scale_x_discrete scale_y_discrete theme vars
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
    cols <- factor(plot_env$data[[cols]]) |> levels()
  }else{
    cols <- levels(plot_env$data[[cols]])
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
    
    dplyr::filter(!is.na(lbl)) |>                   # if there are multiple rows/samples, all but the first row will be NA - drop them

    ggplot(aes(x = x, y = y, label = lbl, vjust = just, color = print)) +
    
    # print labels (include all values to maintain spacing, but only color one per group)
    geom_text(angle = 90, hjust = 1, size = 3) +
    scale_color_manual(values = c('transparent', 'black')) +

    facet_grid(cols = vars(grp), rows = vars(), space = 'free', scales = 'free_x', switch = 'both') +
    coord_cartesian(clip = 'off') +

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
  
  g + col_labels + plot_layout(design = layout)
}


#' mBP_row_labels
#' Add matrix barplot row labels to a plot
#' @rdname mBP_col_labels
#' @export
mBP_row_labels <- function(g, margin = 3)
{
  # take care of those pesky no visible binding for global variables warnings
  if(FALSE)
    grp <- just <- lbl <- midpoint <- PANEL <- x <- y <- 
      yend <- colour <- group <- xend <- NULL
  
  # this is where we'll find the facet label information we need
  plot_env <- ggplot_build(g)$layout$facet_params$plot_env
  
  # fetch row labels
  rows <- as_label(ggplot_build(g)$layout$facet_params$rows[[1]])
  
  ncols <- length(unique(ggplot_build(g)$data[[1]]$PANEL)) / length(unique(plot_env$data[[rows]]))
  
  if(!is.factor(plot_env$data[[rows]]))
  {
    rows <- factor(plot_env$data[[rows]]) |> levels()
  }else{
    rows <- levels(plot_env$data[[rows]])
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
    unique() |>                   # if there are multiple rows/samples, all but the first row will be NA - drop them
    
    ggplot(aes(x = x, y = y, label = lbl, vjust = just, color = print)) +
    
    # print labels (include all values to maintain spacing, but only color one per group)
    geom_text(angle = 0, hjust = 1, size = 3) +
    scale_color_manual(values = c('transparent', 'black')) +
    
    facet_grid(cols = vars(), rows = vars(lbl), space = 'free', scales = 'free_y', switch = 'both') +
    coord_cartesian(clip = 'off') +
    
    # turn off all labels, axes, and facet grid annotations
    scale_x_discrete(labels = NULL, breaks = NULL) +
    scale_y_discrete(labels = NULL, breaks = NULL) +
    labs(x = NULL, y = NULL) +
    
    theme(legend.position = 'none',
          strip.text = element_text(color = 'transparent', size = 0),
          strip.background = element_blank())
  
  # add the labels to the plot (assuming the legend is at the top - need to work on this some more)
  layout <- c(area(t = 1, l =      1, b = 20, r = margin),
              area(t = 1, l = margin, b = 20, r =     20))
  
  row_labels + g + plot_layout(design = layout)
}
