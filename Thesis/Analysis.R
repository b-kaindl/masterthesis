"Reconstructing the Analysis done in JMulti in R to
add a final bootstrap and deal with non-normal errors
from estimated dVAR models
" 

#clear working space from objects
rm(list = ls())

# Bind libraries and source utility file
library(vars)  # for basic VAR functionality
library(ggplot2) # for advanced plotting
library(gridExtra) # to construct arranged multiplots
library(stargazer) # for LaTeX table outputs
library(broom) # to get tables from statistics
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

#Produce results for variables
mod_stargazer('VarResults.tex', dVar$varresult$GDP, dVar$varresult$Investment, dVar$varresult$ESR,
              dVar$varresult$Consumption, dVar$varresult$CPI, align = TRUE, title = 'Regression Results for Dependent Variables in the dVar(3) Model')

test <- 0

predBase <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeB)

predictList <- list()
predictList[[1]] <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS1)
predictList[[2]] <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS2)

#predS1 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS1)
#predS2 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS2)


#Test for Normality (JP, Skew, and Kurtosis)
normtest <- normality.test(dVar)

mod_stargazer('JBtest.tex', tidy(normtest$jb.mul$JB), title = 'Jarque-Bera Test Statistics for the dVar(3) Model')
mod_stargazer('kurtosis.tex', tidy(normtest$jb.mul$Kurtosis),
              #tidy(normtest$jb.mul$Skewness),
              title = 'Kurtosis Test Statistics for Normality for the dVar(3) Model')
mod_stargazer('skewness.tex', tidy(normtest$jb.mul$Skewness),
              title = 'Skewness Test Statistics for Normality for the dVar(3) Model')


if (length(colnames(EndoVars)) %% 2 == 0) {
    plotrows <- length(colnames(EndoVars)) / 2
} else {
    plotrows <- (length(colnames(EndoVars))+1) / 2
}

jpeg('acfplot.jpg')
par(mfrow = c(plotrows,2))
for (i in 1:length(colnames(EndoVars))) {
    name <- colnames(EndoVars)[i]
    resids <- dVar[['varresult']][[name]][['residuals']]
    acfplot(resids, name)
}
dev.off()

#Run Wilcoxon test to check for significant difference between scenarios and BL
WTestS1 <- predictionsignificance(EndoVars, predBase, predictList[[1]])
WTestS2 <- predictionsignificance(EndoVars, predBase, predictList[[2]])

mod_stargazer('WTest.tex', WTestS1, title = 'Results of Wilcoxon Signed Ranktest for Scenario 1 vs Baseline')
mod_stargazer('WTest.tex', WTestS2, title = 'Results of Wilcoxon Signed Ranktest for Scenario 1 vs Baseline')




axisStart <- as.Date('2012/10/01')
axisEnd <- as.Date('2015/04/01')
predictStart <- as.Date('2013/07/01')



buildBaseGraphs(EndoVars, dVar, predBase, axisStart, axisEnd, predictStart)

buildScenarioGraphs(EndoVars, dVar, predBase, predictList, as.Date('2013/04/01'), axisEnd, predictStart)


#resids <- dVar[['varresult']][[varname]][['residuals']]
#acfplot(resids, varname)
#Prepare dataframes for plots

