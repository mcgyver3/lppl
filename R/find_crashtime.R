#' lppl.find_crashtime
#'
#' This is the summation to find the critical point in the entered world thermal data.
#'
#'
#' @param tradedate Use a "Date" type vector as the transaction date.
#' @param price Closing price numeric data vector
#' @param Right_days It implements that the peak should drop by 25\% during the right_days (60 days) period, and the default value is 60.
#' @param Right_scale set the percentage of drop price from peak price. (Default : 0.8)
#' @param Left_days The peak of critical time is higher than any peak prior to 262 days (Default : 262)
#'
#' @keywords lppl, crashtime
#' @return
#'
#'
#' @details
#' There are three conditions at the critical point.
#' During the 262 days prior to the first critical point, the highest price must be less than the current price.
#' Prices must drop by 25\% or more within 60 days prior to the second critical point.
#' Third, there should be no price higher than the serial price for 60 days after the critical point.
#'
#' @references D.S. Brée and N. L. Joseph, “Testing for financial crashes using the log-periodic power law model,” International Review of Financial Analysis, vol. 30, pp. 287–297, 2013.
#'
#' @examples
#'
#' library(lppl)
#'
#' tradedate <- lppl::NASDAQ$Date
#' price <- lppl::NASDAQ$Close
#'
#' dfCRASH <- lppl.find_crashtime(tradedate =tradedate, price = price)
#'

lppl.find_crashtime <- function(
     tradedate
    ,price
    ,Left_days=262
    ,Right_days=60
    ,Right_scale=0.8
    ,showplot=TRUE
    )
{
    id <- c(1:length(tradedate))
    df <- data.frame(id,tradedate,price)
    df["PERCENT_DROP"]=0
    df["logprice"]=log(price)
    idx_end <- nrow(df)

    for (i in 1:idx_end){

        i_price <- df$price[i]

        {
            if(i-Left_days>0){
                max_prior <- max(df$price[(i-Left_days):(i-1)])
            }else if (i>1){
                max_prior <- max(df$price[1:i-1])
            }else{
                max_prior <- max(df$price[i])
            }
        }

        {
            if (i+Right_days<idx_end){
                max_post <- max(df$price[(i+1):(i+Right_days)])
                drop_post <- subset(df,df$price[(i+1):(i+Right_days)] < Right_scale*df$price[i],select=id)
                min_post <- min(df$price[(i+1):(i+Right_days)])
            }else if (i+1<idx_end){
                max_post <- max(df$price[(i+1):idx_end])
                drop_post <- subset(df,df$price[(i+1):idx_end] < Right_scale*df$price[i],select=id)
                min_post <- min(df$price[(i+1):idx_end])
            }else{
                max_post <- max(df$price[idx_end])
                drop_post <- subset(df,df$price[idx_end] < Right_scale*df$price[i],select=id)
                min_post <- min(df$price[idx_end])
            }
        }

        if ((max_prior< price[i]) && (length(drop_post)>0) && (max_post<price[i])){
            df$PERCENT_DROP[i] <- (1 - min_post/df$price[i])*100;
        }
    }

    dfCRASH <- subset(df,df$PERCENT_DROP >= (1-Right_scale)*100)

    crashplot <- ggplot2::ggplot(data=df, ggplot2::aes(x=tradedate,y=price))
    crashplot <- crashplot + ggplot2::geom_line(color="blue")
    crashplot <- crashplot + ggplot2::geom_vline(xintercept = as.numeric(dfCRASH$tradedate) ,color = "darkorange2", linetype = 4, size=1)
    if(showplot){
        print(crashplot)
        print.data.frame(dfCRASH)
        return(crashplot)
    } else{
        return(dfCRASH)
    }
}

