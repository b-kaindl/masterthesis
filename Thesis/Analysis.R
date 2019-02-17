"Reconstructing the Analysis done in JMulti in R to
add a final bootstrap and deal with non-normal errors
from estimated dVAR models
" 

#clear working space from objects
rm(list = ls())

# Bind libraries and source utility file
library(vars)  # for basic VAR functionality
library(ggplot2) # for advanced plotting
source('utilityfunc.R')
#library(tsDyn) # for more advanced VAR operations and bootstraps

#Read dataset
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

tsGSPBase <- ts(data$`gov. spending`,
    start = c(1980, 1), frequency = 4)

tsGSPS1 <- ts(data$`gov. spending`,
    start = c(1980, 1), frequency = 4) * 1.1

tsGSPS2 <- ts(data$`gov. spending`,
    start = c(1980, 1), frequency = 4) * 1.25


#tsGSPBase <- diff(log(ts(data$`gov. spending`,
            #start = c(1980, 1), frequency = 4)), lag = 1)

#tsGSPS1 <- diff(log(ts(data$`gov. spending`,
            #start = c(1980, 1), frequency = 4) * 1.1), lag = 1)

#tsGSPS2 <- diff(log(ts(data$`gov. spending`,
            #start = c(1980, 1), frequency = 4) * 1.25), lag = 1)

tsCPI <- ts(data$`CPI inflation`,
            start = c(1980, 1), frequency = 4)

tsEndo <- cbind(tsGDP, tsINV, window(tsESR, c(1980,2)), tsCON, window(tsCPI, c(1980,2)))

#need the time series again as data frame --> undo after taking logs

EndoVars <- as.data.frame(window(tsEndo, c(1980,2), c(2013,2)))



colnames(EndoVars) <- c("GDP", "Investment", "ESR", "Consumption", "CPI") #also make them readable



# Also construct column vectors for alternative scenarios to attach them to the data frame
tsGSP_Base <- window(tsGSPBase, end = c(2013, 2))
GSP_Base <- as.data.frame(window(tsGSP_Base, c(1980,2)))
GSP_ForeB <- as.data.frame(window(tsGSPBase, c(2013,3)))
GSP_ForeS1 <- as.data.frame(window(tsGSPS1, c(2013, 3)))
GSP_ForeS2 <- as.data.frame(window(tsGSPS2, c(2013, 3)))

dVar <- VAR(EndoVars, p = 3, type = "none", exogen = GSP_Base, ic = "FPE")

predBase <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeB)

predictList <- list()
predictList[[1]] <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS1)
predictList[[2]] <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS2)

#predS1 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS1)
#predS2 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS2)


#Test for Normality (JP, Skew, and Kurtosis)
normtest <- normality.test(dVar)

for (i in 1:length(colnames(EndoVars))) {
    name <- colnames(EndoVars)[i]
    resids <- dVar[['varresult']][[name]][['residuals']]
    acfplot(resids, name)
}

#Run Wilcoxon test to check for significant difference between scenarios and BL
WTestS1 <- predictionsignificance(EndoVars, predBase, predictList[[1]])
WTestS2 <- predictionsignificance(EndoVars, predBase, predictList[[2]])

axisStart <- as.Date('2012/10/01')
axisEnd <- as.Date('2015/04/01')
predictStart <- as.Date('2013/07/01')



buildBaseGraphs(EndoVars, dVar, predBase, axisStart, axisEnd, predictStart)

buildScenarioGraphs(EndoVars, dVar, predBase, predictList, as.Date('2013/04/01'), axisEnd, predictStart)
#resids <- dVar[['varresult']][[varname]][['residuals']]
#acfplot(resids, varname)
#Prepare dataframes for plot


# Legacy for debugging

###Could easily be extended with sanity checks and be refactored
#nrows <- nrow(EndoVars)
#for (i in 1:length(colnames(EndoVars)))
#{
  #varname <- colnames(EndoVars)[i]
  #startDate <- as.Date("2011/1/1")
  #endDate <- as.Date("2015/4/1")
  #dates <- seq(startDate, endDate, by = "quarter")
  #actual <- EndoVars[(nrows - length(dates) + 1):nrows,
                     #varname]
  
  #actualdf <- data.frame(dates, actual,
                       #check.rows = TRUE)
  #colnames(actualdf) = c('Dates','value')
  
  
  #startDate <- as.Date("2013/7/1")
  #dates <- seq(startDate, endDate, by = "quarter")
  
  #basefcst <- predBase$fcst[[varname]][,1]
  #lower <- predBase$fcst[[varname]][,2]
  #upper <- predBase$fcst[[varname]][,3]
  #S1fcst <- predS1$fcst[[varname]][,1]
  #S2fcst <- predS1$fcst[[varname]][,1]

  #fcstdf <- data.frame(dates, basefcst, lower, upper, S1fcst,
                         #S2fcst,
                         #check.rows = TRUE)
  #colnames(fcstdf) =  c('Dates','value', 'lower', 'upper',
      #'Forecast - Scenario 1',
      #'Forecast - Scenario 2')
  
  ##Baseline Plot
  ##baseplotdf <- merge(actualdf[actualdf$Dates < min(fcstdf$Dates),], fcstdf[,1:4], by ='Dates', all=TRUE)
  ##baseplotdf$Row.names <- as.Date.factor(baseplotdf$Row.names, format='%Y-%m-%d')
  
  ## baseplot <- ggplot(baseplotdf[baseplotdf$Dates < min(fcstdf$Dates), c('Dates', 'Actual')], aes(x= Dates, y = Actual)) +
  ##   geom_line(color='blue') + geom_smooth(aes(x= Dates, y = 'Forecast - Base', 
  ##                                             ymax = upper, ymin = lower), color = 'red',
  ## data = baseplotdf[baseplotdf$Dates >= min(fcstdf$Dates), c('Dates','Forecast - Base', 'lower','upper')], stat = 'identity') + scale_x_date() + scale_y_continuous()
  #actualdf$id <- 'actual'
  ## actualdf$lower <- NA
  ## actualdf$upper <- NA
  #basefcstdf <- fcstdf[, c('Dates', 'value'#, 'upper', 'lower'
                           #)]
  #basefcstdf$id <- 'base'
  
  ##TODO: consider how to make forecast bands look prettier
  ##TODO: also make look of graphs consistent
  
  #upperdf <- fcstdf[, c('Dates', 'upper')]
  #upperdf$id <- 'upper'
  #colnames(upperdf) <- c('Dates', 'value', 'id') 
  
  #lowerdf <- fcstdf[, c('Dates', 'lower')]
  #lowerdf$id <- 'lower'
  #colnames(lowerdf) <- c('Dates', 'value', 'id')
  
  #baseplotdf <- rbind(actualdf#[actualdf$Dates < min(basefcstdf$Dates),]
                      #, basefcstdf, upperdf, lowerdf)
  #baseplot <- ggplot() + geom_line(data =  baseplotdf[baseplotdf$id == 'actual', c(1,2)], 
                                   #color = 'black', aes(x = Dates, y = value)) +
    #geom_line(data = baseplotdf[baseplotdf$id == 'base', c(1, 2)], color = 'black', aes(x = Dates, y = value), linetype = 'dashed') +
    #geom_line(data = baseplotdf[baseplotdf$id == 'upper', c(1, 2)], color = 'black', aes(x = Dates, y = value), linetype = 'dotted') +
    #geom_line(data = baseplotdf[baseplotdf$id == 'lower', c(1, 2)], color = 'black', aes(x = Dates, y = value), linetype = 'dotted') +
    #scale_x_date(breaks = seq(min(baseplotdf$Dates), max(baseplotdf$Dates), by = 'quarter')) +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_bw() + theme_linedraw()
  
  #baseplot

   ##TODO: export into file
#}