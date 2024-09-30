# matrixBP


``` r
library(matrixBP)
```

    Loading required package: deeptime

    Loading required package: ggplot2

    Loading required package: patchwork

``` r
library(RColorBrewer)
library(cowplot)
```


    Attaching package: 'cowplot'

    The following object is masked from 'package:patchwork':

        align_plots

``` r
theme_set(theme_cowplot())

root <- here::here()

# Load the data
data(metlip)

# colors for facet_grid labels
grp_colors <- levels(metlip$Group) |>
  length() |>
  brewer.pal('Set3')
names(grp_colors) <- levels(metlip$Group)


# plot with colored facets
matrix_barplot(metlip, aes(x = Lipid, y = neg_log_p, color = log2FC),
               cols = vars(Group), rows = vars(sample), facet_colors = grp_colors) |>
  mBP_legend()
```

    Warning: Removed 11 rows containing missing values or values outside the scale range
    (`geom_rect()`).

![](README_files/figure-commonmark/unnamed-chunk-1-1.png)

``` r
# plot with labeled facets doesn't want to plot to the preview window
# the labels are also messed up and need some help
matrix_barplot(metlip, aes(x = Lipid, y = neg_log_p, color = log2FC),
               cols = vars(Acyl_1), rows = vars(sample)) |>
  mBP_labels()
```

    Warning: Removed 578 rows containing missing values or values outside the scale range
    (`geom_text()`).

![](README_files/figure-commonmark/unnamed-chunk-1-2.png)

``` r
# adding text labels when coloring the x-axis of facet_colors also has the same issue
matrix_barplot(metlip, aes(x = Lipid, y = neg_log_p, color = log2FC),
               cols = vars(Group), rows = vars(sample), facet_colors = grp_colors) |>
  mBP_labels()
```

    Warning: Removed 578 rows containing missing values or values outside the scale range
    (`geom_text()`).

![](README_files/figure-commonmark/unnamed-chunk-1-3.png)

``` r
# trying to plot multiple samples on a single row causes the colors not to work
# add a catch that will notify the user if they try to do this
matrix_barplot(metlip, aes(x = Lipid, y = neg_log_p, color = log2FC),
               cols = vars(Group), facet_colors = grp_colors) |>
  mBP_legend()
```

    Warning: The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?

    Warning: The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    The following aesthetics were dropped during statistical transformation:
    colour.
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?

    Warning: Removed 11 rows containing missing values or values outside the scale range
    (`geom_rect()`).

![](README_files/figure-commonmark/unnamed-chunk-1-4.png)

``` r
# try adding `na.rm = TRUE` to `scale_...` to see if that gets rid of warnings about NA values
```
