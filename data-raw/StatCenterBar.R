# StatCenterBar.R
# This contains the generation of `StatCenterBar` which is required for `stat_centerBar` to work. This is a new stat for ggplot2 that creates a barplot or line segments with bars centered around the x-axis. This is useful for creating matrix barplots. The `compute_group` function is used to compute the line segment endpoints.

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

usethis::use_data(StatCenterBar, overwrite = TRUE)
