#' NIKKEI time series data.
#'
#' A dataset of 1043 rows containing the prices and Date attributes of
#' NIKKEI INDEX from July 1, 1987 to June 28, 1991.
#'
#' @format A data frame with Date, Price variables:
#' \describe{
#'   \item{Date}{Trading day}
#'   \item{Close}{Closing price}
#' }
#' @source \url{https://finance.yahoo.com/}
#' @export
"NIKKEI"

library(readr)
NIKKEI <- read_csv("data/nikkei.csv")
usethis::use_data(NIKKEI, overwrite = TRUE)
