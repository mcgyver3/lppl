#' Estimation of the LPPL model using a nonlinear optimization
#'
#' This function estimates a LPPL model using a nonlinear optimization
#'
#' @param tradedate Use a "Date" type vector as the transaction date.
#' @param price Closing price numeric data vector
#' @param mb The LPPL model is estimated using only the data from the critical point to the specified number of days (mb x 20 days). The data mb has a vector type, and the estimated data for each mb specified is returned. The default value is 1 to 3 months.
#' @param MovingWindows This is the size of the window used to detect the peak point in the PeakDetection operation, and input as vector data. The default value is 5 to 10 days.
#' @param Right_days It implements that the peak should drop by 25\% during the right_days (60 days) period, and the default value is 60.
#' @param Right_scale set the percentage of drop price from peak price. (Default : 0.8)
#' @param Left_days The peak of critical time is higher than any peak prior to 262 days (Default : 262)
#' @param h Parameter for peak detection. Parameter is a positive coefficient, meaning that the higher the value of h, the tighter the peak is detected. h is generally set at 1 <= h <= 3. According to Palshikar (2009), the default value was set to 1.5.
#' @param gaRunCnt GA Execution Count (Default : 100)
#' @param gaGenerations the maximum number of iterations to run before the GA search is halted. (Default : 500)
#' @param gaPopulation the population size. (Default : 200)
#' @param gaElitism the number of best fitness individuals to survive at each generation.(Default : 0.5)
#' @param gaMutation the probability of mutation in a parent chromosome.Usually mutation occurs with a small probability, and by default is set to 0.1.
#' @param gaCrossover the probability of crossover between pairs of chromosomes. Typically this is a large value and by default is set to 0.8.
#'
#' @keywords lppl
#'
#'
#'
#' @return
#' LogPrice : Log price of input time series data
#'
#' id_SRTPT : Starting index of the data used for LPPL estimation as the lowest point after the previous crash time
#'
#' id_crash : Index of real crash time
#'
#' id_END   : Last index of data used for crash time estimation
#'
#' DT_SRTPT : Date corresponding to id_SRTPT
#'
#' DT_crash : Date of real crash time
#'
#' DT_END   : Last date of data used for crash time estimation
#'
#' Best_order : crash time estimation result (RMSE-based sorting data)
#'
#' @importFrom stats C
#' @importFrom stats sd
#' @importFrom stats runif
#' @importFrom stats lm
#' @importFrom foreach %dopar%
#'
#' @examples
#'
#' library(lppl)
#'
#' tradedate <- lppl::NASDAQ$Date
#' price <- lppl::NASDAQ$Close
#'
#' Result <- lppl.validates(tradedate =tradedate
#'                        ,price = price
#'                        ,mb=c(1)
#'                        ,MovingWindows=c(5)
#'                        ,gaRunCnt=100
#'                        ,gaGenerations=200
#'                        ,gaPopulation=50)
#'
#' @export

lppl.validates <- function(
  tradedate
  ,price
  ,mb=c(1:3)
  ,MovingWindows=c(5:10)
  ,Right_days=60
  ,Right_scale=0.8
  ,Left_days=262
  ,h=1.5
  ,gaRunCnt=100
  ,gaGenerations=500
  ,gaPopulation=200
  ,gaElitism=0.5
  ,gaMutation=0.1
  ,gaCrossover=0.8)
{

  if (class(tradedate) != "Date") {
    tradedate <- as.Date(tradedate)
  }

  #Parallel check
  {
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE"){
      cores <- 2L
      parallel = FALSE
    } else {
      parallel = TRUE
      cores = parallel::detectCores()
    }

    if(parallel == TRUE){

      cores = parallel::detectCores()

      #env <- foreach:::.foreachGlobals
      #rm(list=ls(name=env), pos=env)
      #rm(env)

      base::tryCatch({
        cluster <- parallel::makeCluster(cores-1)
        doParallel::registerDoParallel(cluster)}
        ,error = function(e) {
          cluster <- NULL}
        ,warning = function(w){
          print(cluster)}
        ,finally = NULL)
    }
  }


  # function
  stopParallel <- function(cluster, ...){
    # Stop parallel computing for GA package
    parallel::stopCluster(cluster)
    foreach::registerDoSEQ()
    base::invisible()
  }
  fitness <- function(a1,a2,a3,a4,a5,a6,a7){

    y <- df_LogPrice$logprice     #종속변수
    x <- 1:length(y)  #시간 축
    Y <-a1+(a2*(a3-x+0i)^a4)*(1+a5*cos(a6*log(a3-x+0i)+a7))
    D <- y-Re(Y)         #%residual
    return(((1/length(y))*sum(D^2))^0.5) #RMSE
  }
  PeakWeight <- function(pk,L_min){
    w1 <- L_min*rep(1,length(pk))-pk
    w2 <- 1./w1
    pw <- w2/sum(w2)
    return(pw)
  }
  PeakDetect <- function(LogPrice,MW,h){
    S <- rep(0,length(LogPrice)) #Score
    Peaks_Orig <- rep(0,length(LogPrice))  #original peaks

    for (idx in (MW+1):(length(LogPrice)-MW)){
      lmax <- LogPrice[idx]-LogPrice[idx-1]
      rmax <- LogPrice[idx]-LogPrice[idx+1]

      for (j in 1:MW) {
        if (LogPrice[idx]-LogPrice[idx-j] > lmax) {
          lmax <- LogPrice[idx]-LogPrice[idx-j]
        }
        if (LogPrice[idx]-LogPrice[idx+j] > rmax) {
          rmax <- LogPrice[idx]-LogPrice[idx+j]
        }
      }
      S[idx] <- 0.5*(lmax+rmax)
    }
    S_Positive <- S[S>0]
    mean_PS <- mean(S_Positive)
    s_PS <- sd(S_Positive)

    for (l in 1:(length(LogPrice)-MW)){
      if (S[l]>0 && (S[l]-mean_PS)>(h*s_PS)) {
        Peaks_Orig[l] <- LogPrice[l]
      }
    }
    pk <- c()

    for (n in (1+MW):(length(LogPrice)-MW-1)){
      d <-0
      for (m in 1:MW){
        if (Peaks_Orig[n] <= Peaks_Orig[(n-m)] || Peaks_Orig[n] <= Peaks_Orig[(n+m)]){
          break;
        }
        else{
          d <- d+1
        }

        if (d==MW){
          pk <- c(pk, n)
        }
      }
    }
    return(pk)
  }
  PeakSelect <- function(pk,pw,n){
    # Cumulative weights
    CumWeight <- cumsum(pw)

    # generated from uniform distribution 'n'
    #n <- stats::runif(1,0,1) # random number
    for (j in 1:length(pk)){
      if (n <= CumWeight[j]){
        m <- j
        break
      }
    }

    if (m <= 2){
      ps <- pk[1:3]
    }
    else{
      ps <- pk[(m-2):m];
    }
    return(ps)
  }
  Populations <- function(ip,lower,upper){
    Range=35
    nvars <- length(lower)
    XOUT <- unlist(ip[1,])

    LB <- lower
    UB <- upper

    lbound   <- XOUT - LB >= 0
    ubound   <- XOUT - UB <= 0

    feasible = all(lbound) && all(ubound)
    if(!feasible){
      XOUT[!lbound] <- LB[!lbound]
      XOUT[!ubound] <- UB[!ubound]
    }


    #Default Range -35 ~ 35
    lowerRange <- rep(-Range,7)
    upperRange <- rep(Range,7)

    #Get the length of each variable's range
    rangeExtent <- abs(upperRange - lowerRange)

    # Find any non-existant limits in the range and bounds
    finiteLB = is.finite(LB);
    finiteUB = is.finite(UB);

    # Ranges given bound values where finite, and default range values otherwise.
    lowerRange[finiteLB] <- LB[finiteLB]
    upperRange[finiteUB] <- UB[finiteUB]

    # In the case of 1-sided bounds, set opposite range to the finite bound
    # offset by the span of default range. NOTE: this guarantees consistency in the range.
    onlyLbFinite <- finiteLB & (!finiteUB)
    onlyUbFinite <- finiteUB & (!finiteLB)

    lowerRange[onlyUbFinite] <- upperRange[onlyUbFinite] - rangeExtent[onlyUbFinite]
    upperRange[onlyLbFinite] <- lowerRange[onlyLbFinite] + rangeExtent[onlyLbFinite]

    lowerbounds <- lowerRange
    upperbounds <- upperRange

    nCnt <- gaPopulation-1

    Population <-
      (matrix(runif(nCnt*7,0,1),nrow = nCnt,ncol = 7) *
         matrix((upperRange - lowerRange), nrow = nCnt,ncol = 7, byrow = T)) +
      matrix(lowerRange,nrow = nCnt,ncol = 7, byrow = T)

    colnames(Population) <- c("A", "B","Tc","beta","C","omega","phi")

    populations <- rbind(Population,XOUT)

    object <- list(populations,lowerbounds,upperbounds)
    names(object) = c("population","lower","upper")
    return(object)
  }
  InitialPopulation <- function(PeakList,PeakWeight,LogPrice,datalength,randomvalue){

    beta=1
    C=0
    Result <- data.frame(A=double(),B=double(),Tc=double(),beta=double(),C=double(),omega=double(),phi=double())
    ps <- PeakSelect(PeakList,PeakWeight,randomvalue)

    i <- ps[1]
    j <- ps[2]
    k <- ps[3]

    rho <- (j-i)/(k-j)

    if (rho > 1){
      base_Tc <- (rho*k-j)/(rho-1)
      base_omega <- 2*pi/log(rho)
      base_phi <- pi-base_omega*log(base_Tc-k)

      Tc <- Re(base_Tc)
      omega <- Re(base_omega)
      phi <- Re(base_phi)

      a <- (1:length(LogPrice))

      X <- data.frame(rep(1,length(a)),Tc*1-a)

      b <- lm(formula = LogPrice~.,data=X)
      A <- b$coefficients[1]
      B <- b$coefficients[3]

      row <- data.frame(A,B,Tc,beta,C,omega,phi)
      colnames(row) <- c("A","B","Tc", "beta","C","omega","phi")
      Result <- rbind(Result, row)
    }
    return(Result)
  }

  #IndentifyStartingPoint
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

    if(nrow(dfCRASH) <= 1)
    {
      stop("Not found Crash Time")
    }

    cat("---------------------------------------------------------------------------\n")
    for (i in 2:nrow(dfCRASH)) {
      cat(format(dfCRASH$tradedate[i], "%Y-%m-%d"), "\n")
    }
    cat("---------------------------------------------------------------------------\n")
    Crash_DT <-as.Date(readline('Which date you want to choice among above critical times? (enter the date as 2007-10-31): '),"%Y-%m-%d")

    idx_Crash <- 0
    idx_Crash <- which(dfCRASH$tradedate == Crash_DT)

    if (idx_Crash == 0 || length(idx_Crash) == 0) {
      cat("You cannot proceed further with the wrong input value.\n")
      return(NULL);
    }

    id_crash <- dfCRASH[idx_Crash,1]
    DT_crash <- dfCRASH[idx_Crash,2]

    id_priorcrash <- dfCRASH[(idx_Crash-1),1]
    I <- which.min(df$price[id_priorcrash:id_crash])

    id_SRTPT <- id_priorcrash+I-1
    DT_SRTPT <- df[id_SRTPT,2]

  }

  indexInfo <- list(subset(df,select=c("id","tradedate","logprice")),DT_SRTPT,id_SRTPT,DT_crash,id_crash)
  names(indexInfo) = c("LogPrice","DT_SRTPT","id_SRTPT","DT_crash","id_crash")

  Result <- list()
  Result[[1]] <- indexInfo
  lstnm <- c("indexInfo")

  for (i_mb in mb) {

    sTime2 <- Sys.time()
    cat('MB : ',i_mb," Start ", format(Sys.time(), "%d %X") ,"\n")

    id_END <- id_crash-20*i_mb
    DT_END <- df[id_END,2]

    df_LogPrice <- subset(df,select=base::c("id","tradedate","logprice"),subset=id >= id_SRTPT & id <= id_END)

    #minimum length of data used in estimation
    WS_max <- 30
    L_min <- nrow(df_LogPrice) - WS_max

    MWData <- data.frame(A=double(),B=double(),Tc=double(),beta=double(),C=double(),omega=double(),phi=double(),RMSE=double(), MW=integer())
    for (i_mw in MovingWindows)
    {
      sTime1 <- Sys.time()

      LogPrice <- df_LogPrice[1:(L_min+i_mw),3]
      LogPrice_DT <- as.Date(df_LogPrice[1:(L_min+i_mw),2])

      # select a series of peaks
      {

        pk <- PeakDetect(LogPrice,i_mw,h)
        pk_DT <- LogPrice_DT[pk]

        pw <- PeakWeight(pk,L_min)
      }

      if(length(pk) > 2){
        lowerbounds <- base::c(A = max(LogPrice), B = -Inf, Tc = length(LogPrice), beta = 0.1, C = -1, omega = 5, phi = 0)
        upperbounds <- base::c(A = Inf, B = 0, Tc = Inf, beta = 0.9, C = 1,  omega = 15,  phi = 2*pi)
        Best <- data.frame(A=double(),B=double(),Tc=double(),beta=double(),C=double(),omega=double(),phi=double(),RMSE=double(), MW=integer())
        {
          if(parallel == TRUE){
            Best <- foreach::foreach(i=1:gaRunCnt, .combine=rbind, .packages = "GA" ) %dopar%
            {
              trycount <- 0
              while(TRUE){
                randomvalue <- runif(1,0,1)
                ip <- InitialPopulation(pk,pw,LogPrice, L_min, randomvalue)

                trycount <- trycount+1
                if (nrow(ip) > 0 | trycount == 50){
                  break
                }
              }

              if (nrow(ip) > 0){
                GASetting <- Populations(ip,lowerbounds,upperbounds)

                # fitness 함수의 기본이 MAX값 추출인 관계로 -fitness를 사용하여 최소값을 찾계끔 처리한다.
                GA <- GA::ga(type = "real-valued"
                         , fitness =  function(x) -fitness(x[1],x[2],x[3],x[4],x[5],x[6],x[7])
                         , lower = GASetting$lower
                         , upper = GASetting$upper
                         , suggestions = GASetting$population
                         , popSize = gaPopulation
                         , pcrossover = gaCrossover
                         , elitism = gaPopulation*gaElitism
                         , pmutation = gaMutation
                         , maxiter = gaGenerations
                         , run = gaGenerations
                         , monitor = FALSE
                         #, population = "Population_R"
                         #, crossover = Crossover_R
                         #, selection = Crossover_R
                         #, mutation = mutation_r
                         , optim = TRUE
                )

                solution <- as.data.frame(GA@solution)
                solution["RMSE"] = -GA@fitnessValue
                solution["MW"] = i_mw
                colnames(solution) <- c("A", "B","Tc","beta","C","omega","phi","RMSE","MW")

                return(solution)
              }
            }
          }
          else{
            for (variable in 1:gaRunCnt) {
              trycount <- 0
              while(TRUE){
                randomvalue <- runif(1,0,1)
                ip <- InitialPopulation(pk,pw,LogPrice, L_min, randomvalue)

                trycount <- trycount+1
                if (nrow(ip) > 0 | trycount == 50){
                  break
                }
              }

              if (nrow(ip) > 0){
                GASetting <- Populations(ip,lowerbounds,upperbounds)
                GA <- GA::ga(type = "real-valued"
                         , fitness =  function(x) -fitness(x[1],x[2],x[3],x[4],x[5],x[6],x[7])
                         , lower = GASetting$lower
                         , upper = GASetting$upper
                         , suggestions = GASetting$population
                         , popSize = gaPopulation
                         , pcrossover = gaCrossover
                         , elitism = gaPopulation*gaElitism
                         , pmutation = gaMutation
                         , maxiter = gaGenerations
                         , run = gaGenerations
                         , monitor = FALSE
                         #, population = "Population_R"
                         #, crossover = Crossover_R
                         #, selection = Crossover_R
                         #, mutation = mutation_r
                         , optim = TRUE
                )
                solution <- as.data.frame(GA@solution)
                solution["RMSE"] = -GA@fitnessValue
                solution["MW"] = i_mw
                colnames(solution) <- c("A", "B","Tc","beta","C","omega","phi","RMSE","MW")

                if(nrow(solution) > 0){
                  Best <- rbind.data.frame(Best,solution)
                }
              }
            }
          }
        }

        if(parallel == TRUE && !is.null(cluster)){
          stopParallel(cluster)
          rm(cluster)
        }

        MWData <- rbind(MWData,Best)
        eTime1 <- Sys.time()
        cat('MB : ',i_mb,', MW : ',i_mw,' Total Time difference of :', difftime(eTime1, sTime1, units = "secs"),"secs\n")
      }

    }

    Best_order <- MWData[order(MWData$RMSE),]
    mblst <- list(i_mb,id_END,DT_END,Best_order);
    names(mblst) = c("i_mb","id_END","DT_END","Best_order")
    Result[[(i_mb+1)]] <- mblst
    lstnm <- c(lstnm,paste("MB",i_mb,sep=""))
    eTime2 <- Sys.time()
    cat('MB : ',i_mb,' Total Time difference of :', difftime(eTime2, sTime2, units = "mins"),"mins\n")
  }

  names(Result) <- lstnm

  return(Result)
}
