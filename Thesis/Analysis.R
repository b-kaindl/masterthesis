"Reconstructing the Analysis done in JMulti in R to
add a final bootstrap and deal with non-normal errors
from estimated dVAR models
" 

#clear working space from objects
rm(list = ls())

#No scientific notation
options(scipen = 999)

# Bind libraries and source utility file
library(vars)  # for basic VAR functionality
library(ggplot2) # for advanced plotting
library(grid)
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
#tsESR <- diff(ts(data$`excess stock returns`,
            #start = c(1980, 1), frequency = 4), lag = 1)

#tsESR <- ts(data$`excess stock returns`,
            #start = c(1980, 1), frequency = 4)

tsE <- ts(data$`excess stock returns`,
 start = c(1980, 1), frequency = 4)

tsESR <- diff(sign(tsE)*log1p(abs(tsE)), lag = 1)

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

tsEndo <- cbind(tsGDP, tsINV, #window(tsESR, c(1980, 2)), 
tsESR, 
tsCON, window(tsCPI, c(1980,2)))



EndoVars <- as.data.frame(window(tsEndo, c(1980,2), c(2013,2)))




colnames(EndoVars) <- c("GDP", "Investment", "ESR", "Consumption", "CPI") #also make them readable



# Also construct column vectors for alternative scenarios to attach them to the data frame
tsGSP_Base <- window(tsGSPBase, start = c(1980,2), end = c(2013, 2))
tsGSP_BaseLag <- lag(window(tsGSPBase, end = c(2013, 1)), -1)


#tsGSP_BaseLag2 <- lag(window(tsGSPBase, end = c(2012, 4)), -2)
#tsGSP_BaseLag3 <- lag(window(tsGSPBase, end = c(2012, 3)), -3)
#tsGSP_BaseLag3 <- lag(window(tsGSPBase, end = c(2012, 2)), -4)

GSP_Base <- as.data.frame(cbind(tsGSP_Base, tsGSP_BaseLag#, tsGSP_BaseLag2, tsGSP_BaseLag3
))
colnames(GSP_Base) <- c('GSP', 'Lag'#, 'Lag2', 'Lag3', 'Lag4'
)

GSP_ForeBLag <- lag(window(tsGSPBase, c(2013, 2), c(2015, 1)), -1)
#GSP_ForeBLag2 <- lag(window(tsGSPBase, c(2013, 1), c(2014, 4)), -2)
#GSP_ForeBLag3 <- lag(window(tsGSPBase, c(2012, 4), c(2014, 3)), -3)
#GSP_ForeBLag4 <- lag(window(tsGSPBase, c(2012, 3), c(2014, 2)), -4)
GSP_ForeB <- as.data.frame(cbind(window(tsGSPBase, c(2013, 3)), GSP_ForeBLag #, GSP_ForeBLag2, GSP_ForeBLag3, GSP_ForeBLag4
))
colnames(GSP_ForeB) <- colnames(GSP_Base)



GSP_ForeS1Lag <- lag(window(tsGSPS1, c(2013, 2), c(2015, 1)), -1)
#GSP_ForeS1Lag2 <- lag(window(tsGSPS1, c(2013, 1), c(2014, 4)), -2)
#GSP_ForeS1Lag3 <- lag(window(tsGSPS1, c(2012, 4), c(2014, 3)), -3)
#GSP_ForeS1Lag4 <- lag(window(tsGSPS1, c(2012, 3), c(2014, 2)), -4)
GSP_ForeS1 <- as.data.frame(cbind(window(tsGSPS1, c(2013, 3)), GSP_ForeS1Lag#, GSP_ForeS1Lag2, GSP_ForeS1Lag3, GSP_ForeS1Lag4
))
colnames(GSP_ForeS1) <- colnames(GSP_Base)

#Spread 10% increase linearly increase lag column only after 2nd entry
S1_lin_effect <- sum(GSP_ForeS1 - GSP_ForeB) / nrow(GSP_ForeS1)
GSP_Fore_S1L <- as.data.frame(cbind((GSP_ForeB[, 1] + S1_lin_effect), (GSP_ForeB[, 2] + c(0, rep(S1_lin_effect, 7)))))
colnames(GSP_Fore_S1L) <- colnames(GSP_Base)

#Spend all GSP at once and then go back
GSP_Fore_S1I <- as.data.frame(cbind(GSP_ForeB[, 1] + c(GSP_ForeB[1, 1] * 0.1, 0, 0, 0, 0, 0, 0, 0), GSP_ForeB[, 2] + c(0, GSP_ForeB[1, 1] * 0.1, 0, 0, 0, 0, 0, 0)))
colnames(GSP_Fore_S1I) <- colnames(GSP_Base)

#Linear Reduction
#GSP_Fore_S1LR <- as.data.frame(cbind(GSP_ForeB[, 1] + c(GSP_ForeB[1, 1] * 0.1, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7,
                               #GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7,
                               #GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7,
                               #GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7),
                #GSP_ForeB[, 2] + c(0, GSP_ForeB[1, 1] * 0.1, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1 / 7, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1 / 7,
                #GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1/7,
                #GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1 / 7, GSP_ForeB[1, 1] * 0.1 - GSP_ForeB[1, 1] * 0.1 / 7)))

GSP_Fore_S1LR <- as.data.frame(cbind(c(GSP_ForeB[1, 1] * 1.1, GSP_ForeB[1, 1] * 1.1 - GSP_ForeB[1, 1] * 0.1 / 7,
                               GSP_ForeB[1, 1] * 1.1 - 2*GSP_ForeB[1, 1] * 0.1 / 7, GSP_ForeB[1, 1] * 1.1 - 3*GSP_ForeB[1, 1] * 0.1 / 7,
                               GSP_ForeB[1, 1] * 1.1 - 4*GSP_ForeB[1, 1] * 0.1 / 7, GSP_ForeB[1, 1] * 1.1 - 5*GSP_ForeB[1, 1] * 0.1 / 7,
                               GSP_ForeB[1, 1] * 1.1 - 6*GSP_ForeB[1, 1] * 0.1 / 7, GSP_ForeB[1, 1] * 1.1 - 7*GSP_ForeB[1, 1] * 0.1 / 7),
               c(GSP_ForeB[1, 2], GSP_ForeB[1, 1] * 1.1, GSP_ForeB[1, 1] * 1.1 - GSP_ForeB[1, 1] * 0.1 / 7,
                               GSP_ForeB[1, 1] * 1.1 - 2 * GSP_ForeB[1, 1] * 0.1 / 7, GSP_ForeB[1, 1] * 1.1 - 3 * GSP_ForeB[1, 1] * 0.1 / 7,
                               GSP_ForeB[1, 1] * 1.1 - 4 * GSP_ForeB[1, 1] * 0.1 / 7, GSP_ForeB[1, 1] * 1.1 - 5 * GSP_ForeB[1, 1] * 0.1 / 7,
                               GSP_ForeB[1, 1] * 1.1 - 6 * GSP_ForeB[1, 1] * 0.1 / 7)))

colnames(GSP_Fore_S1LR) <- colnames(GSP_Base)

GSP_ForeS2Lag <- lag(window(tsGSPS2, c(2013, 2), c(2015, 1)), -1)
#GSP_ForeS2Lag2 <- lag(window(tsGSPS2, c(2013, 1), c(2014, 4)), -2)
#GSP_ForeS2Lag3 <- lag(window(tsGSPS2, c(2012, 4), c(2014, 3)), -3)
#GSP_ForeS2Lag4 <- lag(window(tsGSPS2, c(2012, 3), c(2014, 2)), -4)
GSP_ForeS2 <- as.data.frame(cbind(window(tsGSPS2, c(2013, 3)), GSP_ForeS2Lag#, GSP_ForeS2Lag2, GSP_ForeS2Lag3, GSP_ForeS2Lag4
))

colnames(GSP_ForeS2) <- colnames(GSP_Base)

#Spread 25% increase linearly
S2_lin_effect <- sum(GSP_ForeS2 - GSP_ForeB) / nrow(GSP_ForeS2)
GSP_Fore_S2L <- as.data.frame(cbind((GSP_ForeB[, 1] + S2_lin_effect), (GSP_ForeB[, 2] + c(0, rep(S2_lin_effect, 7)))))
colnames(GSP_Fore_S2L) <- colnames(GSP_Base)

#Spend all GSP at once and then go back
GSP_Fore_S2I <- as.data.frame(cbind(GSP_ForeB[, 1] + c(GSP_ForeB[1, 1] * 0.25, 0, 0, 0, 0, 0, 0, 0), GSP_ForeB[, 2] + c(0, GSP_ForeB[1, 1] * 0.25, 0, 0, 0, 0, 0, 0)))
colnames(GSP_Fore_S2I) <- colnames(GSP_Base)

#Spend all GSP at once and then reuduce linearly
#GSP_Fore_S2LR <- as.data.frame(cbind(GSP_ForeB[, 1] + c(GSP_ForeB[1, 1] * 0.25, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7,
                               #GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7,
                               #GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7,
                               #GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7),
                #GSP_ForeB[, 2] + c(0, GSP_ForeB[1, 1] * 0.25, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25 / 7, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25 / 7,
                #GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7,
#GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7, GSP_ForeB[1, 1] * 0.25 - GSP_ForeB[1, 1] * 0.25/7)))

GSP_Fore_S2LR <- as.data.frame(cbind(c(GSP_ForeB[1, 1] * 1.25, GSP_ForeB[1, 1] * 1.25 - GSP_ForeB[1, 1] * 0.25 / 7,
                               GSP_ForeB[1, 1] * 1.25 - 2 * GSP_ForeB[1, 1] * 0.25 / 7, GSP_ForeB[1, 1] * 1.25 - 3 * GSP_ForeB[1, 1] * 0.25 / 7,
                               GSP_ForeB[1, 1] * 1.25 - 4 * GSP_ForeB[1, 1] * 0.25 / 7, GSP_ForeB[1, 1] * 1.25 - 5 * GSP_ForeB[1, 1] * 0.25 / 7,
                               GSP_ForeB[1, 1] * 1.25 - 6 * GSP_ForeB[1, 1] * 0.25 / 7, GSP_ForeB[1, 1] * 1.25 - 7 * GSP_ForeB[1, 1] * 0.25 / 7),
               c(GSP_ForeB[1, 2], GSP_ForeB[1, 1] * 1.25, GSP_ForeB[1, 1] * 1.25 - GSP_ForeB[1, 1] * 0.25 / 7,
                               GSP_ForeB[1, 1] * 1.25 - 2 * GSP_ForeB[1, 1] * 0.25 / 7, GSP_ForeB[1, 1] * 1.25 - 3 * GSP_ForeB[1, 1] * 0.25 / 7,
                               GSP_ForeB[1, 1] * 1.25 - 4 * GSP_ForeB[1, 1] * 0.25 / 7, GSP_ForeB[1, 1] * 1.25 - 5 * GSP_ForeB[1, 1] * 0.25 / 7,
                               GSP_ForeB[1, 1] * 1.25 - 6 * GSP_ForeB[1, 1] * 0.25 / 7)))
colnames(GSP_Fore_S2LR) <- colnames(GSP_Base)

dVar <- VAR(EndoVars, p = 3, type = "none", exogen = GSP_Base, ic = "FPE")

#Produce results for variables
mod_stargazer('VarResults.tex', dVar$varresult$GDP, dVar$varresult$Investment, dVar$varresult$ESR,
              dVar$varresult$Consumption, dVar$varresult$CPI, align = TRUE, title = 'Regression Results for Dependent Variables in the dVar(3) Model',
             column.labels = colnames(EndoVars))

predBase <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeB)

predictList <- list()
predictList[[1]] <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS1)
predictList[[2]] <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS2)

#predS1 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS1)
#predS2 <- predict(dVar, n.ahead = 8, ci = 0.95, dumvar = GSP_ForeS2)


#Test for Normality (JP, Skew, and Kurtosis)
normtest <- normality.test(dVar)

mod_stargazer('JBtest.tex', data.frame(tidy(normtest$jb.mul$JB)), title = 'Jarque-Bera Test Statistics for the dVar(3) Model', summary = FALSE, rownames = FALSE, digits = 4,
              dep.var.labels = c('Statistic','p-Value', 'Parameter', 'Method'))
mod_stargazer('kurtosis.tex', data.frame(tidy(normtest$jb.mul$Kurtosis)),
              #tidy(normtest$jb.mul$Skewness),
              title = 'Kurtosis Test Statistics for the dVar(3) Model', summary = FALSE, rownames = FALSE, digits = 4, align = TRUE,
              dep.var.labels = c('Statistic', 'p-Value', 'Parameter', 'Method'))
mod_stargazer('skewness.tex', data.frame(tidy(normtest$jb.mul$Skewness)),
              title = 'Skewness Test Statistics for the dVar(3) Model', summary = FALSE, rownames = FALSE, digits = 4, align = TRUE,
              dep.var.labels = c('Statistic', 'p-Value', 'Parameter', 'Method'))


if (length(colnames(EndoVars)) %% 2 == 0) {
    plotrows <- length(colnames(EndoVars)) / 2
} else {
    plotrows <- ((length(colnames(EndoVars)))+1) / 2
}

acftests <- list()
for (n in 1:length(colnames(EndoVars))) {
    name <- colnames(EndoVars)[n]
    resids <- dVar[['varresult']][[name]][['residuals']]
    acftests[[n]] <- acf(resids)
}

png('acfplot.png', height = 800, width = 1200)
par(mfrow = c(plotrows, 2))

for (m in 1:length(colnames(EndoVars))) {
    acfplot(acftests[[m]], colnames(EndoVars)[m])
}
dev.off()


#Run Wilcoxon test to check for significant difference between scenarios and BL
WTestS1 <- predictionsignificance(EndoVars, predBase, predictList[[1]])
#WTestS2 <- predictionsignificance(EndoVars, predBase, predictList[[2]])

mod_stargazer('WTest.tex', WTestS1, title = 'Results of Wilcoxon Signed Ranktest for Scenarios vs Baseline', summary = FALSE)
#mod_stargazer('WTest.tex', WTestS2, title = 'Results of Wilcoxon Signed Ranktest for Scenario 1 vs Baseline')




axisStart <- as.Date('2012/10/01')
axisEnd <- as.Date('2015/04/01')
predictStart <- as.Date('2013/07/01')



buildBaseGraphs(EndoVars, dVar, predBase, axisStart, axisEnd, predictStart)

buildScenarioGraphs(EndoVars, dVar, predBase, predictList, as.Date('2013/04/01'), axisEnd, predictStart)


#resids <- dVar[['varresult']][[varname]][['residuals']]
#acfplot(resids, varname)
#Prepare dataframes for plots

