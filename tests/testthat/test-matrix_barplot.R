library(testthat)
library(ggplot2)
library(matrixBP)

# Create a basic data frame for testing
df <- data.frame(
  analyte = paste0("analyte ", 1:5),
  pval = c(0.01, 0.04, 0.001, 0.06, 0.04),
  fold_change = c(1.2, -0.8, 2.5, -1.5, 0.5),
  category = c("A", "B", "A", "B", "C")
)

# landscape orientation
test_that("matrix_barplot works with duplicate p-values", {
  p <- matrix_barplot(df,
                      aes(x = analyte, y = -log10(pval), color = fold_change),
                      rows = vars(category))
  expect_s3_class(p, "ggplot")
})

# portrait orientation
test_that("matrix_barplot works with duplicate p-values", {
  p <- matrix_barplot(df,
                      aes(y = analyte, x = -log10(pval), color = fold_change),
                      rows = vars(category),
                      orientation = "portrait")
  expect_s3_class(p, "ggplot")
})


test_that("matrix_barplot throws an error with duplicate samples", {
  df_dup <- rbind(df, df[1, ])
  expect_error(matrix_barplot(df_dup,
                              aes(x = analyte, y = -log10(pval), color = fold_change),
                              rows = vars(category)),
               "multiple rows or columns plotted to the same position. Did you forget to define `rows` or `cols`?")
})

test_that("matrix_barplot works with basic input", {
  p <- matrix_barplot(df,
                      aes(x = analyte, y = -log10(pval), color = fold_change),
                      rows = vars(category))
  expect_s3_class(p, "ggplot")
})
