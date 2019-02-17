"Reconstructing the Analysis done in JMulti in R to
add a final bootstrap and deal with non-normal errors
from estimated dVAR models
" 

#Function Definitions for Utility functions
#create function to simplify creation of appropriate ACF resid plots
acfplot <- function(var, cmain) {

    fname <- paste0(cmain, 'acfplot.jpg')

    a <- acf(var)

    #build the plot
    jpeg(fname)
    plot(a$acf[2:13],
       type = 'h',
       main = cmain,
       xlab = 'Lag',
       ylab = 'ACF',
       ylim = c(-0.5, 0.5),
       las = 1,
       xaxt = 'n')

    #add some extra features (0 line and conf)
    abline(h = 0)
    x <- c(1:12)
    y <- c(1:12)
    axis(1, at = x, labels = y)

    critval <- qnorm(1.95 / 2) / sqrt(length(var))
    #add approximated 5% critical levels
    abline(h = c(
    (critval), - (critval)), lty = c(2, 2))

    dev.off()

}

#Takes the predictions for the variables in var
preidctionsignificance <- function(vardata, basepred, scenariopred) {
    

    #Prepare Dataframe to be returned later
    WstatResult <- data.frame(matrix(nrow = length(colnames(vardata)), ncol = 2))
    colnames(WstatResult) <- c('Test Statistic', 'p-Value')
    rownames(WstatResult) <- colnames(vardata)

    for (i in 1:length(colnames(vardata))) {

        varname <- colnames(vardata)[i]
        test <- wilcox.test(basepred$fcst[[varname]][, 1], scenariopred$fcst[[varname]][, 1], paired = TRUE)

        WstatResult[i,] <- c(test[['statistic']][['V']], test[['p.value']])
    }

    return(WstatResult)
}


# Bind libraries
library(vars)  # for basic VAR functionality
library(ggplot2) # for advanced plotting
#library(tsDyn) # for more advanced VAR operations and bootstraps

data <- readRDS('moritadata.rds')

" Convert the data into time series, take their logs (except inflation and CPI) 
 and combine them into a data frame (correcting for data point lost due to lag)
"
tsESR <- ts(data$`excess stock returns`,
            start = c(1980, 1), frequency = 4)

tsGDP <- diff(log(ts(data$GDP,
            start = c(1980, 1), frequency = 4)), lag = 1)

tsCON <- diff(log(ts(data$consumption,
            start = c(1980, 1), frequency = 4)), lag = 1)

tsINV <- diff(log(ts(data$investment,
            start = c(1980, 1), frequency = 4)), lag = 1)

tsGSP <- diff(log(ts(data$`gov. spending`,
            start = c(1980, 1), frequency = 4)), lag = 1)

tsCPI <- ts(data$`CPI inflation`,
            start = c(1980, 1), frequency = 4)

tsEndo <- cbind(tsGDP, tsINV, window(tsESR, c(1980,2)), tsCON, window(tsCPI, c(1980,2)))

#need the time series again as data frame --> undo after taking logs

EndoVars <- as.data.frame(window(tsEndo, c(1980,2), c(2013,2)))



colnames(EndoVars) <- c("GDP", "Investment", "ESR", "Consumption", "CPI") #also make them readable



# Also construct column vectors for alternative scenarios to attach them to the data frame
tsGSP_Base <- window(tsGSP, end =  c(2013,2))
GSP_Base <- as.data.frame(tsGSP_Base)
GSP_ForeB <- as.data.frame(window(tsGSP, c(2013,3)))
GSP_ForeS1 <- as.data.frame(window(tsGSP, c(2013,3))*1.1)
GSP_ForeS2 <- as.data.frame(window(tsGSP, c(2013,3))*1.25)

#tsGSP_Scen1 <- ts(c(window(tsGSP, c(1980,2), c(2013,2)), 
#                     window(tsGSP, c(2013, 3))*1.1), start = c(1980,2), frequency = 4)
#tsGSP_Scen2 <- ts(c(window(tsGSP, c(1980,2), c(2013,2)), 
#                 window(tsGSP, c(2013, 3))*1.25), start = c(1980,2), frequency = 4)

# Estimate a linear Var model with log-differenced time series with 3 lags
# and no determnisitic terms, using the base scenario as exogenous. Use level since we have already done
# logdiffs
#dVarB <- lineVar(tsEndo, 3, r = 1, include = "none",
#                 model = "VAR", I = "level", beta = NULL, estim = "2OLS", LRinclude = "none",
#                 exogen = tsGSP_Base)

dVar <- VAR(EndoVars, p = 3, type = "none", exogen = GSP_Base, ic = "FPE")


normality.test(dVar)

predBase <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeB)
predS1 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS1)
predS2 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS2)





#WstatS2 <- data.frame(matrix(nrow = length(colnames(EndoVars)), ncol = 2))
#colnames(WstatS2) <- c('Test Statistic', 'p-Value')
#rownames(WstatS1) <- colnames(EndoVars)

#for (i in 1:length(colnames(EndoVars))){
  
  ##Grab Variable Name
  #varname <- colnames(EndoVars)[i]
  
  #test <- wilcox.test(predBase$fcst[[varname]][,1], predS2$fcst[[varname]][,1], paired = TRUE)
  
  #WstatS2[i,] <- c(test[['statistic']][['V']],test[['p.value']])
  
  
  
#}

#TODO: Call Autocorrelation Plot function
#Build residual autocorrelation plot
resids <- dVar[['varresult']][[varname]][['residuals']]
acfplot(resids, varname)
#Prepare dataframes for plot

##Could easily be extended with sanity checks and be refactored
nrows <- nrow(EndoVars)
for (i in 1:length(colnames(EndoVars)))
{
  varname <- colnames(EndoVars)[i]
  startDate <- as.Date("2011/1/1")
  endDate <- as.Date("2015/4/1")
  dates <- seq(startDate, endDate, by = "quarter")
  actual <- EndoVars[(nrows - length(dates) + 1):nrows,
                     varname]
  
  actualdf <- data.frame(dates, actual,
                       check.rows = TRUE)
  colnames(actualdf) = c('Dates','value')
  
  
  startDate <- as.Date("2013/7/1")
  dates <- seq(startDate, endDate, by = "quarter")
  
  basefcst <- predBase$fcst[[varname]][,1]
  lower <- predBase$fcst[[varname]][,2]
  upper <- predBase$fcst[[varname]][,3]
  S1fcst <- predS1$fcst[[varname]][,1]
  S2fcst <- predS1$fcst[[varname]][,1]

  fcstdf <- data.frame(dates, basefcst, lower, upper, S1fcst,
                         S2fcst,
                         check.rows = TRUE)
  colnames(fcstdf) =  c('Dates','value', 'lower', 'upper',
      'Forecast - Scenario 1',
      'Forecast - Scenario 2')
  
  #Baseline Plot
  #baseplotdf <- merge(actualdf[actualdf$Dates < min(fcstdf$Dates),], fcstdf[,1:4], by ='Dates', all=TRUE)
  #baseplotdf$Row.names <- as.Date.factor(baseplotdf$Row.names, format='%Y-%m-%d')
  
  # baseplot <- ggplot(baseplotdf[baseplotdf$Dates < min(fcstdf$Dates), c('Dates', 'Actual')], aes(x= Dates, y = Actual)) +
  #   geom_line(color='blue') + geom_smooth(aes(x= Dates, y = 'Forecast - Base', 
  #                                             ymax = upper, ymin = lower), color = 'red',
  # data = baseplotdf[baseplotdf$Dates >= min(fcstdf$Dates), c('Dates','Forecast - Base', 'lower','upper')], stat = 'identity') + scale_x_date() + scale_y_continuous()
  actualdf$id <- 'actual'
  # actualdf$lower <- NA
  # actualdf$upper <- NA
  basefcstdf <- fcstdf[, c('Dates', 'value'#, 'upper', 'lower'
                           )]
  basefcstdf$id <- 'base'
  
  #TODO: consider how to make forecast bands look prettier
  #TODO: also make look of graphs consistent
  
  upperdf <- fcstdf[, c('Dates', 'upper')]
  upperdf$id <- 'upper'
  colnames(upperdf) <- c('Dates', 'value', 'id') 
  
  lowerdf <- fcstdf[, c('Dates', 'lower')]
  lowerdf$id <- 'lower'
  colnames(lowerdf) <- c('Dates', 'value', 'id')
  
  baseplotdf <- rbind(actualdf#[actualdf$Dates < min(basefcstdf$Dates),]
                      , basefcstdf, upperdf, lowerdf)
  baseplot <- ggplot() + geom_line(data =  baseplotdf[baseplotdf$id == 'actual', c(1,2)], 
                                   color = 'black', aes(x = Dates, y = value)) +
    geom_line(data = baseplotdf[baseplotdf$id == 'base', c(1, 2)], color = 'black', aes(x = Dates, y = value), linetype = 'dashed') +
    geom_line(data = baseplotdf[baseplotdf$id == 'upper', c(1, 2)], color = 'black', aes(x = Dates, y = value), linetype = 'dotted') +
    geom_line(data = baseplotdf[baseplotdf$id == 'lower', c(1, 2)], color = 'black', aes(x = Dates, y = value), linetype = 'dotted') +
    scale_x_date(breaks = seq(min(baseplotdf$Dates), max(baseplotdf$Dates), by = 'quarter')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_bw() + theme_linedraw()
  
  baseplot

   #TODO: export into file
}