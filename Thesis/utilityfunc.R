#Function Definitions for utility functions

#create function to simplify creation of appropriate ACF resid plots
#plot critical values according to original acf() function
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

#TODO: Test after refactoring
#Takes the predictions for the variables in var
predictionsignificance <- function(vardata, basepred, scenariopred) {


    #Prepare Dataframe to be returned later
    WstatResult <- data.frame(matrix(nrow = length(colnames(vardata)), ncol = 2))
    colnames(WstatResult) <- c('Test Statistic', 'p-Value')
    rownames(WstatResult) <- colnames(vardata)

    for (i in 1:length(colnames(vardata))) {

        varname <- colnames(vardata)[i]
        test <- wilcox.test(basepred$fcst[[varname]][, 1], scenariopred$fcst[[varname]][, 1], paired = TRUE)

        WstatResult[i,] <- c(test[['statistic']][['V']], round(test[['p.value']], 2))
    }

    return(WstatResult)
}

#TODO: Test after refactoring
#Build graphs for comparison of baseline forecast vs actual values
buildBaseGraphs <- function(vardata, model, pred, graphStart, graphEnd, predStart) {
    nrows <- nrow(vardata)
    for (i in 1:length(colnames(vardata))) {
        varname <- colnames(vardata)[i]
        
        fname <- paste0(varname, '_BasePred.jpeg')

        #startDate <- as.Date("2011/1/1")
        #endDate <- as.Date("2015/4/1")
        graphDates <- seq(graphStart, graphEnd, by = "quarter")
        actual <- vardata[(nrows - length(graphDates) + 1):nrows,
                     varname]

        actualdf <- data.frame(graphDates, actual,
                       check.rows = TRUE)

        colnames(actualdf) = c('Dates', 'value')
        lastNonPredDate <- max(actualdf[actualdf$Dates < predStart, 'Dates'])

        #startDate <- as.Date("2013/7/1")
        predDates <- seq(predStart, graphEnd, by = "quarter")
        

        basefcst <- pred$fcst[[varname]][, 1]
        lower <- pred$fcst[[varname]][, 2]
        upper <- pred$fcst[[varname]][, 3]
        #S1fcst <- predS1$fcst[[varname]][, 1]
        #S2fcst <- predS1$fcst[[varname]][, 1]

        fcstdf <- data.frame(predDates, basefcst, lower, upper, check.rows = TRUE)
        colnames(fcstdf) = c('Dates', 'value', 'lower', 'upper')

        #Baseline Plot
        #baseplotdf <- merge(actualdf[actualdf$Dates < min(fcstdf$Dates),], fcstdf[,1:4], by ='Dates', all=TRUE)
        #baseplotdf$Row.names <- as.Date.factor(baseplotdf$Row.names, format='%Y-%m-%d')

        # baseplot <- ggplot(baseplotdf[baseplotdf$Dates < min(fcstdf$Dates), c('Dates', 'Actual')], aes(x= Dates, y = Actual)) +
        #   geom_line(color='blue') + geom_smooth(aes(x= Dates, y = 'Forecast - Base', 
        #                                             ymax = upper, ymin = lower), color = 'red',
        # data = baseplotdf[baseplotdf$Dates >= min(fcstdf$Dates), c('Dates','Forecast - Base', 'lower','upper')], stat = 'identity') + scale_x_date() + scale_y_continuous()
        actualdf$id <- 'actual'
        actualdf$type <- 'actual'
        # actualdf$lower <- NA
        # actualdf$upper <- NA
        basefcstdf <- fcstdf[, c('Dates', 'value' #, 'upper', 'lower'
                           )]
        basefcstdf$id <- 'base'
        basefcstdf$type <- 'basepred'
        basefcstdf[nrow(basefcstdf) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$Dates == lastNonPredDate, 'value'], 'base', 'basepred')

        #TODO: consider how to make forecast bands look prettier
        #TODO: also make look of graphs consistent

        upperdf <- fcstdf[, c('Dates', 'upper')]
        upperdf$id <- 'upper'
        upperdf$type <- 'predband'
        colnames(upperdf) <- c('Dates', 'value', 'id', 'type')
        upperdf[nrow(upperdf) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$Dates == lastNonPredDate, 'value'], 'upper', 'predband')

        lowerdf <- fcstdf[, c('Dates', 'lower')]
        lowerdf$id <- 'lower'
        lowerdf$type <- 'predband'
        colnames(lowerdf) <- c('Dates', 'value', 'id', 'type')
        lowerdf[nrow(lowerdf) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$Dates == lastNonPredDate, 'value'], 'lower', 'predband')

        baseplotdf <- rbind(actualdf #[actualdf$Dates < min(basefcstdf$Dates),]
                      , basefcstdf, upperdf, lowerdf)

        baseplot <- ggplot(data = data.frame(baseplotdf), aes(x = Dates, y = value, linetype = type, shape = id)) +
        geom_line() +  scale_x_date(breaks = graphDates) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
        +ggtitle(varname)
        

        #Write to File
        jpeg(fname, width = 639, height = 396, units = 'px', quality = 100)
        print(baseplot)
        dev.off()


    }
}


# Plots baseline (handed over as single pred) and scenarios (handed over as list of preds)
buildScenarioGraphs <- function(vardata, model, basePred, predList, graphStart, graphEnd, predStart) {
    nrows <- nrow(vardata)
    for (i in 1:length(colnames(vardata))) {
        varname <- colnames(vardata)[i]

        fname <- paste0(varname, '_Scenarios.jpeg')

        graphDates <- seq(graphStart, graphEnd, by = "quarter")
        actual <- vardata[(nrows - length(graphDates) + 1):nrows,
                         varname] #could actually use dates here, but I dislike this due to the data type

        actualdf <- data.frame(graphDates, actual,
                           check.rows = TRUE)
        actualdf$type <- 'actual'
        colnames(actualdf) = c('dates', 'value', 'type')

        predDates <- seq(predStart, graphEnd, by = "quarter")
        lastNonPredDate <- max(actualdf[actualdf$dates < predStart, 'dates'])

        basefcst <- data.frame(predDates, basePred$fcst[[varname]][, 1])
        basefcst$type <- 'Baseline'
        colnames(basefcst) = c('dates', 'value', 'type')
        basefcst[nrow(basefcst) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$dates == lastNonPredDate, 'value'], 'Baseline')

        #Initialize DF for scenario forecasts
        scenFcstList <- list()

        for (j in 1:length(predList)) {
            #Prepare DF for scenario forecast
            fcstSeries <- data.frame(predDates, predList[[j]]$fcst[[varname]][, 1])
            fcstSeries$type <- paste('Scenario', j)
            colnames(fcstSeries) = c('dates', 'value', 'type')

            #Store scenario forecast in list 
            scenFcstList[[j]] <- fcstSeries
        }

        #Initialize plot DF with actual and baseline values
        scenPlotDf <- rbind(actualdf, basefcst)

        #Add scenario forecasts
        for (k in 1:length(scenFcstList)) {
            scenFcstList[[k]][nrow(scenFcstList[[k]]) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$dates == lastNonPredDate, 'value'], paste('Scenario', j))
            scenPlotDf <- rbind(scenPlotDf, scenFcstList[[k]])
        }

        
 
        
        scenplot <- ggplot(data = scenPlotDf, aes(x = dates, y = value, shape = type, linetype = type)) +
            geom_line() + geom_point(size=3)

        #Make the graph look nice
        scenplot <- scenplot + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
        + ggtitle(varname)

        #Write to File
        jpeg(fname, width = 639, height = 396, units = 'px', quality = 100)

        print(scenplot)

        dev.off()

        

    }
}