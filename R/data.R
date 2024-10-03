#' Sample metlip data
#' 
#' A simulated dataset of 4 samples and 200 metabolites.
#' 
#' @format ## `metlip`
#' A data frame with 7 columns and 768 rows
#' \describe{
#'   \item{Lipid}{Lipid ID}
#'   \item{Group}{Lipid group}
#'   \item{Acyl_1}{First acyl chain}
#'   \item{Acyl_2}{Second acyl chain}
#'   \item{sample}{Sample ID}
#'   \item{log2FC}{Log2 fold change}
#'   \item{neg_log_p}{-log10 p-value}
#' }
#' @source Simulated data
"metlip"


#' StatCenterBar
#' 
#' A new stat for ggplot2 that creates a barplot or line segments with bars centered around the x-axis
"StatCenterBar"