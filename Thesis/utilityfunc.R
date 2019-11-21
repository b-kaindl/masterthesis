#Function Definitions for utility functions

#create function to simplify creation of appropriate ACF resid plots
#plot critical values according to original acf() function
acfplot <- function(var, cmain){


    a <- var

    #build the plot
    plot(a$acf[2:13],
       type = 'h',
       main = cmain,
       xlab = 'Lag',
       ylab = 'ACF',
       ylim = c(-0.5, 0.5),
       las = 1,
       xaxt = 'n',
        cex.lab = 1.5,
        cex.axis = 1.5,
        cex.main = 2,
        cex.sub = 2)


    abline(h = 0)
    x <- c(1:12)
    y <- c(1:12)
    axis(1, at = x, labels = y, cex.axis = 1.5)


    critval <- qnorm(1.95 / 2) / sqrt(a$n.used)

    #add approximated 5% critical levels
    abline(h = c(
    (critval), - (critval)), lty = c(2, 2))


}


#Takes the predictions for the variables in var
predictionsignificance <- function(vardata, basepred, scenariopred) {


    #Prepare Dataframe to be returned later
    WstatResult <- data.frame(matrix(nrow = length(colnames(vardata)), ncol = 2))
    colnames(WstatResult) <- c('Test Statistic', 'p-Value')
    rownames(WstatResult) <- colnames(vardata)

    for (i in 1:length(colnames(vardata))) local({
        i <- i
        varname <- colnames(vardata)[i]
        test <- wilcox.test(basepred$fcst[[varname]][, 1], scenariopred$fcst[[varname]][, 1], paired = TRUE)

        WstatResult[i,] <<- c(test[['statistic']][['V']], round(test[['p.value']], 2))
    })

    return(WstatResult)
}


#Build graphs for comparison of baseline forecast vs actual values
buildBaseGraphs <- function(vardata, model, pred, graphStart, graphEnd, predStart) {
    nrows <- nrow(vardata)
    plotlist <- list() #prepare list of plots

    for (i in 1:length(colnames(vardata))) local({

        i <- i
        varname <- colnames(vardata)[i]
        

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
        # print(varname)
        # print(dim(predDates))
        # print(dim(basefcst))
        # print(dim(lower))
        # print(nrow(upper))

        fcstdf <- data.frame(predDates, basefcst, lower, upper, check.rows = TRUE)
        colnames(fcstdf) = c('Dates', 'value', 'lower', 'upper')

        #Baseline Plot
        actualdf$id <- 'actual'
        actualdf$type <- 'Actual'
        basefcstdf <- fcstdf[, c('Dates', 'value' #, 'upper', 'lower'
                           )]
        basefcstdf$id <- 'base'
        basefcstdf$type <- 'Baseline'
        basefcstdf[nrow(basefcstdf) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$Dates == lastNonPredDate, 'value'], 'base', 'Baseline')

        #TODO: consider how to make forecast bands look prettier
        #TODO: also make look of graphs consistent

        upperdf <- fcstdf[, c('Dates', 'upper')]
        upperdf$id <- 'upper'
        upperdf$type <- 'Forecast Bands'
        colnames(upperdf) <- c('Dates', 'value', 'id', 'type')
        upperdf[nrow(upperdf) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$Dates == lastNonPredDate, 'value'], 'upper', 'Forecast Bands')

        lowerdf <- fcstdf[, c('Dates', 'lower')]
        lowerdf$id <- 'lower'
        lowerdf$type <- 'Forecast Bands'
        colnames(lowerdf) <- c('Dates', 'value', 'id', 'type')
        lowerdf[nrow(lowerdf) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$Dates == lastNonPredDate, 'value'], 'lower', 'Forecast Bands')

        baseplotdf <- rbind(actualdf #[actualdf$Dates < min(basefcstdf$Dates),]
                      , basefcstdf, upperdf, lowerdf)

        baseplot <- ggplot(data = data.frame(baseplotdf), aes(x = Dates, y = value, linetype = type, shape = id)) +
        geom_line() + scale_x_date(breaks = graphDates) +
        theme_bw() +
        theme(text = element_text(size = 15), legend.text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.title = element_blank()) +
        labs(tag = varname, x = NULL, y = NULL)
        
        #Store plot in list
        plotlist[[i]] <<- baseplot


    })

    fname <- 'basefcst.png'

    nGraphs <- length(plotlist)

    if (nGraphs %% 2 == 0) {
        plotrows <- nGraphs / 2
    } else {
        plotrows <- (nGraphs + 1) / 2 
    }


    #Write to File
    png(fname, height = 800, width = 1200)

    print(do.call(grid_arrange_shared_legend,
                       c(plotlist, list(nrow = plotrows, ncol = 2, position = "bottom"))))

    dev.off()

}


# Plots baseline (handed over as single pred) and scenarios (handed over as list of preds)
buildScenarioGraphs <- function(vardata, model, basePred, predList, graphStart, graphEnd, predStart) {
    nrows <- nrow(vardata)
    plotlist <- list() #prepare list of plots

    for (i in 1:length(colnames(vardata))) local({

        i <- i
        varname <- colnames(vardata)[i]

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
        scenPlotDf <- basefcst

        #Add scenario forecasts
        for (k in 1:length(scenFcstList)) {
            scenFcstList[[k]][nrow(scenFcstList[[k]]) + 1,] <-
            list(lastNonPredDate, actualdf[actualdf$dates == lastNonPredDate, 'value'], paste('Scenario', k))
            scenPlotDf <- rbind(scenPlotDf, scenFcstList[[k]])
        }

        
 
        
        scenplot <- ggplot(data = scenPlotDf, aes(x = dates, y = value, shape = type,
        linetype = type)) +
            geom_line() + geom_point(size = 2) +
            scale_x_date(breaks = graphDates) 

        #Make the graph look nice
        scenplot <- scenplot +
        theme_bw() +
        theme(text = element_text(size = 15), legend.text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.title = element_blank()) +
        labs(tag = varname, x = NULL, y = NULL)
      
        #Store plot in list
        plotlist[[i]] <<- scenplot




        

    })

    fname <- 'scenariofcst.png'

    nGraphs <- length(plotlist)

    if (nGraphs %% 2 == 0) {
        plotrows <- nGraphs/2
    } else {
        plotrows <- (nGraphs + 1) / 2
    }

    #Write to File
    png(fname, height = 800, width = 1200)

    print(do.call(grid_arrange_shared_legend,
                        c(plotlist, list(nrow = plotrows, ncol = 2, position = "bottom"))))

    dev.off()
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    #from https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
        plots <- list(...)
        position <- match.arg(position)
        g <-
        ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
        legend <- g[[which(sapply(g, function(x)
            x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x)
            x + theme(legend.position = "none"))
        gl <- c(gl, ncol = ncol, nrow = nrow)

        combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )

        grid.newpage()
        grid.draw(combined)

        # return gtable invisibly
        invisible(combined)

}

mod_stargazer <- function(output.file, ...) {
    #write stargazer output to file
    #from https://stackoverflow.com/questions/30195718/stargazer-save-to-file-dont-show-in-console
    output <- capture.output(stargazer(...))

    #to remove environment and caption (want to set those in LaTeX)
    output <- output[4:length(output)]

    #append = TRUE results in one single file; FALSE results in file for each table
    cat(paste(output, collapse = "\n"), "\n", file = output.file, append = TRUE)
}