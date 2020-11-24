#' SSEC time series data.
#'
#' A dataset of 1,009 rows containing the prices and Date attributes of
#' Shanghai Composite Index from July 1, 1997 to June 29, 2001.
#'
#' @format A data frame with Date, Price variables:
#' \describe{
#'   \item{Date}{Trading day}
#'   \item{Close}{Closing price}
#' }
#' @source \url{https://finance.yahoo.com/}
#' @export
"SSEC"

library(readr)
SSEC <- read_csv("data/ssec.csv")
usethis::use_data(SSEC, overwrite = TRUE)
