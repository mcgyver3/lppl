
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Log Periodic Power Low(LPPL)

<!-- badges: start -->

<!-- badges: end -->

The package implements Log-Periodic-Power-Low(LPPL) models proposed to
test for financial bubbles.

## Installation

You can install the released version of lppl from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("lppl")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mcgyver3/lppl")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lppl)

tradedate <- lppl::NASDAQ$Date
price <- lppl::NASDAQ$Close

Result <- lppl.validates(tradedate =tradedate
                         ,price = price
                         ,mb=c(1:2)
                         ,MovingWindows=c(5)
                         ,gaRunCnt=100
                         ,gaGenerations=200
                         ,gaPopulation=50)
---------------------------------------------------------------------------
2000-03-10 
---------------------------------------------------------------------------
Which date you want to choice among above critical times? (enter the date as 2007-10-31): 2000-03-10
#> MB :  1  Start  02:13:21 
#> MB :  1 , MW :  5  Total Time difference of : 9.937357 secs
#> MB :  1  Total Time difference of : 0.1656393 mins
#> 
#> MB :  2  Start  02:13:31 
#> MB :  2 , MW :  5  Total Time difference of : 10.1944 secs
#> MB :  2  Total Time difference of : 0.1693024 mins
```

``` r
Result
#> $indexInfo
#> $indexInfo$LogPrice
#>      id  tradedate logprice
#> 1     1 1997-07-01 7.271182
#> 2     2 1997-07-02 7.283180
#> 3     3 1997-07-03 7.291390
#> 4     4 1997-07-07 7.293521
#> 5     5 1997-07-08 7.303237
#> ..... Middle omission
#> 
#> $indexInfo$DT_SRTPT
#> [1] "1998-10-08"
#> 
#> $indexInfo$id_SRTPT
#> [1] 322
#> 
#> $indexInfo$DT_crash
#> [1] "2000-03-10"
#> 
#> $indexInfo$id_crash
#> [1] 680
#> 
#> $MB1
#> $MB1$i_mb
#> [1] 1
#> 
#> $MB1$id_END
#> [1] 660
#> 
#> $MB1$DT_END
#> [1] "2000-02-10"
#> 
#> $MB1$Best_order
#>             A           B       Tc      beta           C     omega        phi       RMSE MW
#> 97   8.803858 -0.07387887 381.9614 0.4837064  0.10274675  5.000000 0.51897390 0.04635148  5
#> 84   8.838666 -0.09312517 377.4844 0.4487356  0.09567146  5.000000 0.63611557 0.04752825  5
#> 41   8.867712 -0.10665418 376.5062 0.4286420  0.08956313  5.121065 6.28318531 0.04887738  5
#> ....Omitted below
```
