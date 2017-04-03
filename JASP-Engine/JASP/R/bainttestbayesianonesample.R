#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

BainTTestBayesianOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
    
    if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
    if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
    if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
    if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
    if(is.null(options()$BFprogress)) options(BFprogress = interactive())
    if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)
    
    
    all.variables <- unlist(options$variables)
    
    if (is.null(dataset))
    {
        if (perform == "run") {
            
            if (options$missingValues == "excludeListwise") {
                
                dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
                
            } else {
                
                dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
            }
            
        } else {
            
            dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
        }
    }
    
    results <- list()
    
    meta <- list()
    
    meta[[1]] <- list(name="ttest", type="table")
    meta[[2]] <- list(name="descriptivesTable", type="table")
    meta[[3]] <- list(name="BFplots", type="collection", meta = "image")
    meta[[4]] <- list(name = "descriptivesPlots", type = "collection", meta = "image")
    
    results[[".meta"]] <- meta
    results[["title"]] <- "Bayesian Informative Hypothesis T-Test"
    
    ttest <- list()
    
    ttest[["title"]] <- "Bayesian Informative Hypothesis One Sample T-Test"
    
    ttest[["citation"]] <- list(
        "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
        "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
    
    bf.type <- options$bayesFactorType
    
    # Set the title of the Bayes Factor column
    
    if (bf.type == "BF10") {
        
        BFH1H0 <- TRUE
        
        if (options$hypothesis == "notEqualToTestValue") {
            bf.title <- "BF\u2081\u2080"
        }
        if (options$hypothesis == "greaterThanTestValue") {
            bf.title <- "BF\u208A\u2080"
        }
        if (options$hypothesis == "lessThanTestValue") {
            bf.title <- "BF\u208B\u2080"
        }
        if(options$hypothesis == "allTypes"){
            bf.title <- "BF\u2081\u2080" # same as not equal to
        }
        
    } else if (bf.type == "LogBF10") {
        
        BFH1H0 <- TRUE
        
        if (options$hypothesis == "notEqualToTestValue") {
            bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
        }
        if (options$hypothesis == "greaterThanTestValue") {
            bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
        }
        if (options$hypothesis == "lessThanTestValue") {
            bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
        }
        if(options$hypothesis == "allTypes"){
            bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)" # same as not equal to
        }
        
    } else if (bf.type == "BF01") {
        
        BFH1H0 <- FALSE
        
        if (options$hypothesis == "notEqualToTestValue") {
            bf.title <- "BF\u2080\u2081"
        }
        if (options$hypothesis == "greaterThanTestValue") {
            bf.title <- "BF\u2080\u208A"
        }
        if (options$hypothesis == "lessThanTestValue") {
            bf.title <- "BF\u2080\u208B"
        }
        if(options$hypothesis == "allTypes"){
            bf.title <- "BF\u2080\u2081" # same as not equal to
        }
    }
    
    
    # Make the fields for the t-test table
    
    if (options$hypothesis == "notEqualToTestValue" | 
        options$hypothesis == "greaterThanTestValue" | 
        options$hypothesis == 'lessThanTestValue') {
        
        
        fields <- list(
            list(name="Variable", type="string", title=""),
            list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
            list(name="pmp", type="number", format="sf:4;dp:3", title="Posterior probability"))
        
    }
    
    if(options$hypothesis == "allTypes"){
        
        type <- 4
        
        fields <- list(
            list(name="Variable", type="string", title=""),
            list(name = "type[greater]", type = "string", title = "Hypothesis"),
            list(name="BF[greater]", type="number", format="sf:4;dp:3", title=bf.title),
            list(name="pmp[greater]", type="number", format="sf:4;dp:3", title="Posterior probability"),
            list(name = "type[less]", type = "string", title = "Hypothesis"),
            list(name="BF[less]", type="number", format="sf:4;dp:3", title="bf.title"),
            list(name="pmp[less]", type="number", format="sf:4;dp:3", title="Posterior probability"),
            list(name = "type[equal]", type = "string", title = "Hypothesis"),
            list(name = "BF[equal]", type = "string", title = bf.title),
            list(name="pmp[equal]", type="number", format="sf:4;dp:3", title="Posterior probability"))
        
    }
    
    ttest[["schema"]] <- list(fields=fields)
    
    results[["ttest"]] <- ttest
    
    # Make the footnotes specifying what hypothesis is tested
    
    footnotes <- .newFootnotes()
    
    if (options$hypothesis == "notEqualToTestValue"){
        
        type <- 1
        
        note <- "For all tests, the alternative hypothesis specifies that the mean does not equal "
        message <- paste0(note, options$testValue, ".")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    }
    
    if (options$hypothesis == "greaterThanTestValue") {
        
        type = 2
        
        note <- "For all tests, the alternative hypothesis specifies that the mean
					is greater than "
        message <- paste0(note, options$testValue, ".")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    } else if (options$hypothesis == "lessThanTestValue") {
        
        type <- 3
        
        note <- "For all tests, the alternative hypothesis specifies that the mean
					is less than "
        message <- paste0(note, options$testValue, ".")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    } else if (options$hypothesis == "allTypes"){
        
        note <- "For all tests, all three hypothesis are tested against test value "
        message <- paste0(note, options$testValue, ".")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
        
    } 
    
    # if (options$descriptives || options$descriptivesPlots)
    # 	results[["descriptives"]] <- list(title="Descriptives")
    
    # Make state lists
    
    plotGroups <- list()
    
    ttest.rows <- list()
    plots.ttest <- list()
    plotTypes <- list()
    plotVariables <- list()
    descriptPlotVariables <- list()
    descriptivesPlots <- list()
    BFplots <- list()
    BFplotvariable <- list()
    errorFootnotes <- rep("no", length(options$variables))
    
    # Retrieve the state
    state <- .retrieveState()
    
    diff <- NULL
    
    # If there is a state, diff becomes the difference in options
    if (!is.null(state)) {
        
        diff <- .diff(options, state$options)
        
    }
    
    status <- rep("ok", length(options$variables))
    plottingError <- rep("error", length(options$variables))
    BF10post <- numeric(length(options$variables))
    
    i <- 1
    
    for (variable in options[["variables"]])
    {
        
        # If there is a state and nothing changed, make the data for ttest rows the one in the state
        if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                     && diff$bayesFactorType == FALSE && diff$testValue == FALSE && diff$missingValues == FALSE)))) {
            
            
            index <- which(state$options$variables == variable)
            
            # if (state$errorFootnotes[index] == "no") {
            # 
            ttest.rows[[length(ttest.rows)+1]] <- state$results$ttest$data[[index]]
            # 
            # } else {
            
            # index2 <- .addFootnote(footnotes, state$errorFootnotes[index])
            
            # errorFootnotes[i] <- state$errorFootnotes[index]
            
            # ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, BF=.clean(NaN), pmp="", .footnotes=list(BF=list(index2)))
            # }
            
            BF10post[i] <- state$BF10post[index]
            status[i] <- state$status[index]
            plottingError[i] <- state$plottingError[index]
            
        }
        # else, make an empty row
        else {
            
            ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, "BF"=".", pmp=".")
        }
        
        
        if (options$descriptivesPlots) {
            
            if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$testValue == FALSE && diff$
                                                                                                                                                             descriptivesPlotsCredibleInterval == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$descriptivesPlots) {
                
                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it
                
                index <- which(state$descriptPlotVariables == variable)
                
                descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]
                
                
            } else {
                
                descriptivesPlot <- list()
                
                descriptivesPlot[["title"]] <- variable
                descriptivesPlot[["width"]] <- options$plotWidth
                descriptivesPlot[["height"]] <- options$plotHeight
                descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
                descriptivesPlot[["status"]] <- "waiting"
                descriptivesPlot[["data"]] <- ""
                
                descriptivesPlots[[length(descriptivesPlots)+1]] <- descriptivesPlot
            }
            
            descriptPlotVariables[[length(descriptPlotVariables)+1]] <- variable
            
        }
        
        i <- i + 1
    }
    
    ttest[["data"]] <- ttest.rows
    ttest[["footnotes"]] <- as.list(footnotes)
    results[["ttest"]] <- ttest
    
    if (options$descriptivesPlots)
        results[["descriptivesPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)
    
    if (options$descriptives) {
        
        descriptivesComplete <- FALSE
        
        descriptives <- list()
        
        descriptives[["title"]] <- "Descriptives"
        descriptives[["cases"]] <- I(options$variables)
        
        fields <- list(
            list(name="v",    title="",   type="string"),
            list(name="N",    title="N",  type="integer"),
            list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
            list(name="sd",   title="SD", type="number",   format="sf:4;dp:3"),
            list(name="se",   title="SE", type="number",   format="sf:4;dp:3"))
        
        ## add credible interval values if asked for in plot
        if (options$descriptivesPlots) {
            interval <- 100 * options$descriptivesPlotsCredibleInterval
            title <- paste0(interval, "% Credible Interval")
            fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
                                                 format = "sf:4;dp:3", title = "Lower",
                                                 overTitle = title)
            fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
                                                 format = "sf:4;dp:3", title = "Upper",
                                                 overTitle = title)
        }
        
        descriptives[["schema"]] <- list(fields=fields)
        descriptives.results <- list()
        
        variables <- options[["variables"]]
        if (length(variables) == 0)
            variables = "."
        
        for (variable in variables) {
            
            if (perform == "run" && length(options[["variables"]]) > 0) {
                
                data <- na.omit(dataset[[ .v(variable) ]])
                
                if (class(data) != "factor") {
                    
                    posteriorSummary <- .posteriorSummaryGroupMean(variable=data, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
                    ciLower <- .clean(posteriorSummary$ciLower)
                    ciUpper <- .clean(posteriorSummary$ciUpper)
                    
                    n    <- .clean(length(data))
                    mean <- .clean(mean(data))
                    stdDeviation <- .clean(sd(data))
                    stdErrorMean <- .clean(sd(data)/sqrt(length(data)))
                    
                    result <- list(v=variable, N=n, mean=mean, sd=stdDeviation, se=stdErrorMean, lowerCI = ciLower, upperCI = ciUpper)
                } else {
                    
                    n <- .clean(length(data))
                    result <- list(v=variable, N=n, mean="", sd="", se="", lowerCI="", upperCI="")
                }
                
                descriptivesComplete <- TRUE
                
            } else {
                
                result <- list(v=variable, N=".", mean=".", sd= ".", se=".", lowerCI=".", upperCI=".")
                
            }
            
            descriptives.results[[length(descriptives.results)+1]] <- result
        }
        
        descriptives[["data"]] <- descriptives.results
        
        if (descriptivesComplete)
            descriptives[["status"]] <- "complete"
        
        results[["descriptivesTable"]] <- descriptives
    }
    
    
    if (perform == "run") {
        
        
        i <- 1
        
        errorFootnotes <- character()
        
        Bainresult <- list()
        Bainvariables <- list()
        
        for (variable in options[["variables"]])
        {
            
            if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                         && diff$bayesFactorType == FALSE && diff$testValue == FALSE && diff$missingValues == FALSE)))) {
                
                index <- which(state$options$variables == variable)
                
                # if (state$errorFootnotes[index] == "no") {
                #
                ttest.rows[[i]] <- state$results$ttest$data[[index]]
                #
                # } else {
                
                # index2 <- .addFootnote(footnotes, state$errorFootnotes[index])
                # 
                # errorFootnotes[i] <- state$errorFootnotes[index]
                # 
                # ttest.rows[[i]] <- list(Variable=variable, BF=.clean(NaN), pmp=".", .footnotes=list(BF=list(index2)))
                # }
                
                BF10post[i] <- state$BF10post[index]
                status[i] <- state$status[index]
                plottingError[i] <- state$plottingError[index]
                Bainresult[[i]] <- state$Bainresult[[index]]
                Bainvariables[[i]] <- state$Bainvariables[[index]]
                
            } else {
                
                variableData <- dataset[[ .v(variable) ]]
                variableData <- variableData[ ! is.na(variableData) ]
                
                variableData <- variableData - options$testValue
                
                r <- Bain::Bain_ttestData(variableData, type = type)
                Bainresult[[i]] <- r 
                Bainvariables[[i]] <- variable
                
                if(type == 1){
                    BF <- r$BF
                    PMP <- r$PMP
                } 
                if(type == 2){
                    BF_greater <- r$BF_greater
                    PMP_greater <- r$PMP
                }
                if(type == 3){
                    BF_less <- r$BF_less
                    PMP_less <- r$PMP
                }
                if(type == 4){
                    BF_greater <- r$BF_greater
                    BF_less <- r$BF_less
                    PMP_greater <- r$PMP_greater
                    PMP_less <- r$PMP_less
                    PMP_equal <- r$PMP_equal
                    
                }
                
                # Bayes factor transformation
                
                # if (bf.type == "BF01")
                # 	bf.raw <- 1 / bf.raw
                # 
                # BF10post[i] <- bf.raw
                # BF <- .clean(bf.raw)
                # 
                # if (options$bayesFactorType == "LogBF10") {
                # 
                # 	BF <- log(BF10post[i])
                # 	BF <- .clean(BF)
                # }
                
                
                
                
                # error handling here ############
                
                # errorMessage <- "Hoi"
                # errorFootnotes[i] <- errorMessage
                # index <- .addFootnote(footnotes, errorMessage)
                
                ##############################################
                
                if(options$hypothesis == "notEqualToTestValue"){
                    result_test <- list(Variable=variable, BF=BF, pmp = PMP)
                } 
                if(options$hypothesis == "greaterThanTestValue"){
                    result_test <-list(Variable=variable, BF=BF_greater, pmp = PMP_greater) 
                }
                if(options$hypothesis == "lessThanTestValue"){
                    result_test <-list(Variable=variable, BF=BF_less, pmp = PMP_less)  
                }
                if(options$hypothesis == "allTypes"){
                    result_test <-list(Variable=variable, 
                                       "type[greater]" = "> Test value",
                                       "BF[greater]"= BF_greater, 
                                       "pmp[greater]" = PMP_greater,
                                       "type[less]"= "< Test value",
                                       "BF[less]" = BF_less, 
                                       "pmp[less]" = PMP_less,
                                       "type[equal]" = "!= Test value",
                                       "BF[equal]" = "",
                                       "pmp[equal]" = PMP_equal) 
                }
                
                ttest.rows[[i]] <- result_test
            } #
            
            if(options$plotPriorAndPosterior){ # if bayes factor plots are wanted
                
                if (!is.null(state) && variable %in% state$options$variables && variable %in% state$Bainvariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE)))) {
                    
                    # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                    # then, if the requested plot already exists, use it
                    if (!is.null(state) && variable %in% state$BFplotvariable && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE)))) {
                        
                        index <- which(state$BFplotvariable == variable)
                        
                        BFplots[[i]] <- state$BFplots[[index]]
                        
                        BFplotvariable[[i]] <- state$BFplotvariable[[index]]
                        
                    } else {
                        
                        index <- which(state$Bainvariables == variable)
                        
                        BFplot <- list()
                        
                        BFplot[["title"]] <- variable
                        BFplot[["width"]] <- options$plotWidth
                        BFplot[["height"]] <- options$plotHeight
                        BFplot[["custom"]] <- list(width="plotWidth", height="plotHeight")
                        BFplot[["status"]] <- "waiting"
                        BFplot[["data"]] <- ""
                        
                        BFplotvariable[[i]] <- variable
                        
                        p <- try(silent= FALSE, expr= {
                            
                            image <- .beginSaveImage(options$plotWidth, options$plotHeight)
                            Bain::plot.BainT(Bainresult[[index]])
                            BFplot[["data"]] <- .endSaveImage(image)
                            
                        })
                        
                        BFplot[["status"]] <- "complete"
                        
                        BFplots[[i]] <- BFplot
                        
                    }
                    
                    
                } else { # create a new bayes factor plot
                    
                    BFplot <- list()
                    
                    BFplot[["title"]] <- variable
                    BFplot[["width"]] <- options$plotWidth
                    BFplot[["height"]] <- options$plotHeight
                    BFplot[["custom"]] <- list(width="plotWidth", height="plotHeight")
                    BFplot[["status"]] <- "waiting"
                    BFplot[["data"]] <- ""
                    
                    BFplotvariable[[i]] <- variable
                    p <- try(silent= FALSE, expr= { 
                        
                        image <- .beginSaveImage(options$plotWidth, options$plotHeight)
                        Bain::plot.BainT(Bainresult[[i]])
                        BFplot[["data"]] <- .endSaveImage(image)
                        
                    })
                    
                    BFplot[["status"]] <- "complete"
                    
                    BFplots[[i]] <- BFplot
                    
                }
            }
            
            # if (class(result_test) == "try-error") {
            # 
            # 	errorMessage <- .extractErrorMessage(result_test)
            # 
            # 	if (errorMessage == "x or y must not contain missing or infinite values.") {
            # 
            # 		errorMessage <- paste("Bayes factor is undefined - the sample contains infinity")
            # 
            # 		status[i] <- "error"
            # 		plottingError[i] <- "Plotting is not possible: Bayes factor is undefined - the sample contains infinity"
            # 
            # 	} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
            # 
            # 		errorMessage <- "Bayes factor is undefined - too few observations"
            # 
            # 		status[i] <- "error"
            # 		plottingError[i] <- "Plotting is not possible: Bayes factor is undefined - the sample has too few observations"
            # 	}
            # 
            # 	status[i] <- "error"
            # 	plottingError[i] <- paste("Plotting is not possible:", errorMessage)
            # 
            # 	errorFootnotes[i] <- errorMessage
            # 
            # 	index <- .addFootnote(footnotes, errorMessage)
            # 
            # 	result_test <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
            # 
            # 	ttest.rows[[i]] <- result_test
            # 
            # } else {
            
            # if (is.na(bf.raw)) {
            # 
            # 	status[i] <- "error"
            # 	plottingError[i] <- "Plotting is not possible: Bayes factor could not be calculated"
            # } else if (is.infinite(1 / bf.raw)) {
            # 
            # 	status[i] <- "error"
            # 	plottingError[i] <- "Plotting is not possible: The Bayes factor is too small"
            # }
            
            # ind <- which(variableData == variableData[1])
            # idData <- sum((ind+1)-(1:(length(ind))) == 1)
            
            # }
            
            i <- i + 1
        }
        
        if(options$plotPriorAndPosterior){
            results[["BFplots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "BF Plots", "BF Plot"), collection=BFplots)
        }
        
        ttest[["data"]] <- ttest.rows
        ttest[["status"]] <- "complete"
        results[["ttest"]] <- ttest
        
        if ( ! .shouldContinue(callback()))
            return()
        
        i <- 1
        descriptInd <- 1
        z <- 1
        
        for (variable in options[["variables"]])
        {
            
            variableData <- dataset[[ .v(variable) ]]
            variableData <- variableData[ ! is.na(variableData) ]
            variableDataDescriptivesPlot <- variableData
            variableData <- variableData - options$testValue
            
            if (options$descriptivesPlots) {
                
                
                if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$testValue == FALSE && diff$
                                                                                                                                                                 descriptivesPlotsCredibleInterval == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$descriptivesPlots) {
                    
                    # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                    # then, if the requested plot already exists, use it
                    
                    index <- which(state$descriptPlotVariables == variable)
                    
                    descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]
                    
                    
                } else {
                    
                    results[["descriptivesPlots"]][["collection"]][[descriptInd]][["status"]] <- "running"
                    
                    if ( ! .shouldContinue(callback(results)))
                        return()
                    
                    plot <- descriptivesPlots[[descriptInd]]
                    
                    p <- try(silent= FALSE, expr= {
                        
                        image <- .beginSaveImage(options$plotWidth, options$plotHeight)
                        .plotGroupMeanBayesOneSampleTtest(variable=variableDataDescriptivesPlot, variableName=variable, testValueOpt=options$testValue, descriptivesPlotsCredibleInterval=options$
                                                              descriptivesPlotsCredibleInterval)
                        plot[["data"]] <- .endSaveImage(image)
                    })
                    
                    # if (class(p) == "try-error") {
                    # 
                    # 	errorMessageTmp <- .extractErrorMessage(p)
                    # 	errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
                    # 	plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
                    # }
                    
                    plot[["status"]] <- "complete"
                    
                    descriptivesPlots[[descriptInd]] <- plot
                }
                
                results[["descriptivesPlots"]][["collection"]] <- descriptivesPlots
                
                descriptInd <- descriptInd + 1
                
                if ( ! .shouldContinue(callback(results)))
                    return()
                
                i <- i + 1
            }
            
        }
        
    }
    
    keep <- NULL
    
    #for (plot in plots.ttest)
    keep <- c(keep,lapply(plots.ttest, function(x) x$data))
    
    #for (plot in descriptivesPlots)
    keep <- c(keep, lapply(descriptivesPlots, function(x) x$data))
    
    #for(plot in BFplots)
    keep <- c(keep,lapply(BFplots, function(x) x$data))
    
    if (perform == "init") {
        
        return(list(results=results, status="inited", state=state, keep=keep))
        
    } else {
        
        return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes,
                                                                   plotVariables=plotVariables, descriptPlotVariables=descriptPlotVariables, descriptivesPlots=descriptivesPlots, BFplots = BFplots, BFplotvariable = BFplotvariable, status=status, plottingError=plottingError,
                                                                   BF10post=BF10post, errorFootnotes=errorFootnotes, Bainvariables = Bainvariables, Bainresult = Bainresult), keep=keep))
        
    }
}

.qt.shiftedT <- function(prob, parameters) {
    
    qt(prob, parameters[3]) * parameters[2] + parameters[1]
    
}

.posteriorSummaryGroupMean <- function(variable, descriptivesPlotsCredibleInterval=.95) {
    
    # Assumes that data are normally distributed
    # Jeffreys prior on mu and sigma: p(mu, sigma) proportional to 1/sigma
    # Compare Gelman et al. "Bayesian Data Analysis" for derivation of marginal posterior distribution of mu (inference for unknown mean and variance of a normal distribution)
    if (is.null(variable)) return(NULL)
    
    ciLower <- (1 - descriptivesPlotsCredibleInterval) / 2
    ciUpper <- ciLower + descriptivesPlotsCredibleInterval
    
    df <- length(variable) - 1
    location <- mean(variable)
    scale <- sd(variable) / sqrt(length(variable))
    
    outTmp <- .qt.shiftedT(c(ciLower, .5, ciUpper), parameters=c(location, scale, df))
    out <- list(ciLower=outTmp[1], median=outTmp[2], ciUpper=outTmp[3])
    
    return(out)
    
}

.base_breaks_y2 <- function(x, testValue){
    
    values <- c(testValue, x$ciLower, x$ciUpper)
    ci.pos <- c(min(values), max(values))
    b <- pretty(ci.pos)
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks=c(min(b), testValue, max(b))))
}

.plotGroupMeanBayesOneSampleTtest <- function(variable=1:10, variableName="test1", testValueOpt=0, descriptivesPlotsCredibleInterval=.95) {
    
    
    variable <- na.omit(variable)
    
    if (any(is.infinite(variable)))
        stop("Plotting not possible: Variable contains infinity")
    
    testValue <- data.frame("testValue" = testValueOpt) # default zero
    posteriorSummary <- .posteriorSummaryGroupMean(variable=variable, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
    summaryStat <- data.frame(groupingVariable=variableName, dependent=posteriorSummary$median, ciLower=posteriorSummary$ciLower, ciUpper=posteriorSummary$ciUpper)
    
    pd <- ggplot2::position_dodge(.2)
    
    p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
        ggplot2::geom_line(position=pd, size = .7) +
        ggplot2::geom_point(position=pd, size=4) +
        ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept=testValue), linetype="dashed") +
        ggplot2::ylab(NULL) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(	panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
                        panel.grid.major=ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
                        axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size=15),
                        panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                        plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                        legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                        panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                        legend.key = ggplot2::element_blank(),
                        legend.title = ggplot2::element_text(size=12),
                        legend.text = ggplot2::element_text(size = 12),
                        axis.ticks = ggplot2::element_line(size = 0.5),
                        axis.ticks.margin = grid::unit(1,"mm"),
                        axis.ticks.length = grid::unit(3, "mm"),
                        axis.ticks.x = ggplot2::element_blank(),
                        plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
        .base_breaks_y2(summaryStat, testValueOpt)
    
    print(p)
    
}
