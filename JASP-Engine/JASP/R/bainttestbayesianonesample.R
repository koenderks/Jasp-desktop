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
        
        bf.title <- "BF\u2081\u2080"
        
    } else if (bf.type == "BF01") {
        
        BFH1H0 <- FALSE
        
        bf.title <- "BF\u2080\u2081"
        
    }
    
    if(options$logscale == "logBF"){
        bf.title <- paste0("Log(",bf.title,")")
    }
    
    # Make the fields for the t-test table
    
    if (options$hypothesis == "notEqualToTestValue" | 
        options$hypothesis == "greaterThanTestValue" | 
        options$hypothesis == 'lessThanTestValue' |
        options$hypothesis == "_4type") {
        
        
        fields <- list(
            list(name="Variable", type="string", title=""),
            list(name = "hypothesis[type1]", type = "string", title = "Hypothesis"),
            list(name="BF[type1]", type="number", format="sf:4;dp:3", title=bf.title),
            list(name="pmp[type1]", type="number", format="dp:3", title="Posterior probability"),
            list(name = "hypothesis[type2]", type = "string", title = "Hypothesis"),
            list(name="BF[type2]", type="number", format="sf:4;dp:3", title=bf.title),
            list(name="pmp[type2]", type="number", format="dp:3", title="Posterior probability"))
        
    }
    
    if(options$hypothesis == "allTypes"){
        
        type <- 5
        
        fields <- list(
            list(name="Variable", type="string", title=""),
            list(name = "type[equal]", type = "string", title = "Hypothesis"),
            list(name="BF[equal]", type="number", format="sf:4;dp:3", title=bf.title),
            list(name="pmp[equal]", type="number", format="dp:3", title="Posterior probability"),
            list(name = "type[greater]", type = "string", title = "Hypothesis"),
            list(name="BF[greater]", type="number", format="sf:4;dp:3", title="bf.title"),
            list(name="pmp[greater]", type="number", format="dp:3", title="Posterior probability"),
            list(name = "type[less]", type = "string", title = "Hypothesis"),
            list(name = "BF[less]", type = "number", title = bf.title,format="sf:4;dp:3"),
            list(name="pmp[less]", type="number", format="dp:3", title="Posterior probability"))
        
    }
    
    ttest[["schema"]] <- list(fields=fields)
    
    results[["ttest"]] <- ttest
    
    # Make the footnotes specifying what hypothesis is tested
    
    footnotes <- .newFootnotes()
    
    if (options$hypothesis == "notEqualToTestValue"){
        
        type <- 1
        note <- "For all tests, the alternative hypothesis specifies that the mean is unequal to "
        message <- paste0(note, options$testValue, "."," The posterior probabilities are based on equal prior probabilities.")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    }
    
    if (options$hypothesis == "greaterThanTestValue") {
        
        type <- 2
        note <- "For all tests, the alternative hypothesis specifies that the mean of group 1 is bigger than "
        message <- paste0(note, options$testValue, "."," The posterior probabilities are based on equal prior probabilities.")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    } else if (options$hypothesis == "lessThanTestValue") {
        
        type <- 3
        note <- "For all tests, the alternative hypothesis specifies that the mean of group 1 is smaller than "
        message <- paste0(note, options$testValue, "."," The posterior probabilities are based on equal prior probabilities.")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    } else if (options$hypothesis == "allTypes"){
        
        note <- "For all tests, the null hypothesis with with test value "
        message <- paste0(note, options$testValue, " is tested against the other hypotheses."," The posterior probabilities are based on equal prior probabilities.")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    } else if(options$hypothesis == "_4type") {
        
        type <- 4
        message <- paste0("For all tests, the null hypothesis specifies that the mean of group 1 is bigger than ",options$testValue," and the alternative hypothesis specifies that the mean is bigger than ",options$testValue,". The posterior probabilities are based on equal prior probabilities.")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
        
    }
    
    # Make state lists
    
    plotGroups <- list()
    
    ttest.rows <- list()
    #plots.ttest <- list()
    plotTypes <- list()
    plotVariables <- list()
    descriptPlotVariables <- list()
    descriptivesPlots <- list()
    BFplots <- list()
    BFplotvariables <- list()
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
                # descriptivesPlot[["width"]] <- options$plotWidth
                # descriptivesPlot[["height"]] <- options$plotHeight
                # descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
                descriptivesPlot[["status"]] <- "waiting"
                descriptivesPlot[["data"]] <- ""
                
                descriptivesPlots[[length(descriptivesPlots)+1]] <- descriptivesPlot
            }
            
            descriptPlotVariables[[length(descriptPlotVariables)+1]] <- variable
            
        }
        
        if (options$plotPriorAndPosterior) {
            
            if (!is.null(state) && variable %in% state$BFplotvariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$testValue == FALSE && diff$
                                                                                                                                                             hypothesis == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$plotPriorAndPosterior) {
                
                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it
                
                index <- which(state$BFplotvariables == variable)
                
                BFplots[[length(BFplots)+1]] <- state$BFplots[[index]]
                
                
            } else {
                
                BFplot <- list()
                
                BFplot[["title"]] <- variable
                # BFplot[["width"]] <- options$plotWidth
                # BFplot[["height"]] <- options$plotHeight
                # BFplot[["custom"]] <- list(width="plotWidth", height="plotHeight")
                BFplot[["status"]] <- "waiting"
                BFplot[["data"]] <- ""
                
                BFplots[[length(BFplots)+1]] <- BFplot
            }
            
            BFplotvariables[[length(BFplotvariables)+1]] <- variable
            
        }
        
        i <- i + 1
    }
    
    if ( ! .shouldContinue(callback(results)))
        return()
    
    ttest[["data"]] <- ttest.rows
    ttest[["footnotes"]] <- as.list(footnotes)
    results[["ttest"]] <- ttest
    
    if(options$plotPriorAndPosterior){
        results[["BFplots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "Bayes Factors plots", "Bayes Factors plot"), collection=BFplots)
    }
    
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
        
        errorFootnotes <- list()
        
        Bainresult <- list()
        Bainvariables <- list()
        BFind <- 1
        
        footnotes2 <- .newFootnotes()
        
        for (variable in options[["variables"]])
        {
            
            if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                         && diff$bayesFactorType == FALSE && diff$logscale == FALSE && diff$testValue == FALSE && diff$missingValues == FALSE)))) {
                
                index <- which(state$options$variables == variable)
                
                 if (state$Bainresult[index] != "error") {
                
                ttest.rows[[i]] <- state$results$ttest$data[[index]]
                
                } else {
                
                index2 <- .addFootnote(footnotes2, state$errorFootnotes[index])

                errorFootnotes[i] <- state$errorFootnotes[index]

                if(options$hypothesis == "notEqualToTestValue"){
                    result_test <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                        "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clear(NaN), "pmp[type2]" = .clear(NaN),.footnotes = list(BF=list(index2)))
                } 
                if(options$hypothesis == "greaterThanTestValue"){
                    result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                       "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = .clear(NaN),.footnotes = list(BF=list(index2)))
                }
                if(options$hypothesis == "lessThanTestValue"){
                    result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clear(NaN), "pmp[type2]" = .clear(NaN),.footnotes = list(BF=list(index2)))
                }
                if(options$hypothesis == "_4type"){
                    result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clear(NaN), "pmp[type2]" = .clear(NaN),.footnotes = list(BF=list(index2))) 
                }
                if(options$hypothesis == "allTypes"){
                    result_test <-list(Variable=variable, 
                                       "type[equal]" = "H0: Equal",
                                       "BF[equal]"= .clear(NaN), 
                                       "pmp[equal]" = .clear(NaN),
                                       "type[greater]"= "H1: Bigger",
                                       "BF[greater]" = .clear(NaN), 
                                       "pmp[greater]" = .clear(NaN),
                                       "type[less]" = "H2: Smaller",
                                       "BF[less]" = .clear(NaN),
                                       "pmp[less]" = .clear(NaN),
                                       .footnotes = list(BF=list(index2))) 
                }
                
                ttest.rows[[i]] <- result_test
                
                }
                
                BF10post[i] <- state$BF10post[index]
                status[i] <- state$status[index]
                plottingError[i] <- state$plottingError[index]
                Bainresult[[i]] <- state$Bainresult[[index]]
                Bainvariables[[i]] <- state$Bainvariables[[index]]
                
            } else {
                
                # error handling  ############
                
                errors <- .hasErrors(dataset=dataset, perform=perform, type=c('observations', 'variance', "infinity"), 
                                     all.target=variable, observations.amount = "< 1", message = "short")
                
                errorMessage <- NULL
                
                if (!identical(errors, FALSE)) {
                    errorMessage <- errors$message
                }
                
                if (!is.null(errorMessage)) {
                    
                    ## add footnote of error #### 
                    index <- .addFootnote(footnotes2, errorMessage)
                    row.footnotes <- list(t = list(index))
                    
                    if(options$hypothesis == "notEqualToTestValue"){
                        result_test <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                            "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clear(NaN), "pmp[type2]" = .clear(NaN),.footnotes = row.footnotes)
                    } 
                    if(options$hypothesis == "greaterThanTestValue"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                           "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = .clear(NaN),.footnotes = row.footnotes)
                    }
                    if(options$hypothesis == "lessThanTestValue"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clear(NaN), "pmp[type2]" = .clear(NaN),.footnotes = row.footnotes)
                    }
                    if(options$hypothesis == "_4type"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"=.clear(NaN), "pmp[type1]" = .clear(NaN),
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clear(NaN), "pmp[type2]" = .clear(NaN),.footnotes = row.footnotes)
                    }
                    if(options$hypothesis == "allTypes"){
                        result_test <-list(Variable=variable, 
                                           "type[equal]" = "H0: Equal",
                                           "BF[equal]"= .clear(NaN), 
                                           "pmp[equal]" = .clear(NaN),
                                           "type[greater]"= "H1: Bigger",
                                           "BF[greater]" = .clear(NaN), 
                                           "pmp[greater]" = .clear(NaN),
                                           "type[less]" = "H2: Smaller",
                                           "BF[less]" = .clear(NaN),
                                           "pmp[less]" = .clear(NaN),
                                           .footnotes = row.footnotes) 
                    }
                    result[[i]] <- "error"
                    Bainvariables[[i]] <- variable 
                    errorFootnotes[[i]] <- errorMessage
                    
                } else {
                
                ##############################################
                
                variableData <- dataset[[ .v(variable) ]]
                variableData <- variableData[ ! is.na(variableData) ]
                
                # subtract test value from data points
                variableData <- variableData - options$testValue
                
                r <- Bain::Bain_ttestData(variableData, type = type)
                Bainresult[[i]] <- r 
                Bainvariables[[i]] <- variable
                
                if(type == 1){
                    BF_0u <- r$BF_0u
                    PMP_u <- r$PMP_u
                    PMP_0 <- r$PMP_0
                    if(options$bayesFactorType == "BF10"){
                        BF_0u <- 1/BF_0u
                    }
                    if(options$logscale == "logBF"){
                        BF_0u <- log(BF_0u)
                    }
                } else if(type == 2){
                    BF_01 <- r$BF_01
                    PMP_1 <- r$PMP_1
                    PMP_0 <- r$PMP_0
                    if(options$bayesFactorType == "BF10"){
                        BF_01 <- 1/BF_01
                    }
                    if(options$logscale == "logBF"){
                        BF_01 <- log(BF_01)
                    }
                } else if(type == 3){
                    BF_01 <- r$BF_01
                    PMP_0 <- r$PMP_0
                    PMP_1 <- r$PMP_1
                    if(options$bayesFactorType == "BF10"){
                        BF_01 <- 1/BF_01
                    }
                    if(options$logscale == "logBF"){
                        BF_01 <- log(BF_01)
                    }
                } else if (type == 4){
                    BF_01 <- r$BF_12
                    PMP_0 <- r$PMP_1
                    PMP_1 <- r$PMP_2
                    if(options$bayesFactorType == "BF10"){
                        BF_01 <- 1/BF_01
                    }
                    if(options$logscale == "logBF"){
                        BF_01 <- log(BF_01)
                    }
                } else if (type == 5) {
                    BF_01 <- r$BF_01
                    BF_02 <- r$BF_02
                    BF_12 <- r$BF_12
                    PMP_0 <- r$PMP_0
                    PMP_1 <- r$PMP_1
                    PMP_2 <- r$PMP_2
                    if(options$bayesFactorType == "BF10"){
                        BF_01 <- 1/BF_01
                        BF_02 <- 1/BF_02
                        BF_12 <- 1/BF_12
                    }
                    if(options$logscale == "logBF"){
                        BF_01 <- log(BF_01)
                        BF_02 <- log(BF_02)
                        BF_12 <- log(BF_12)
                    }
                }
                
                if(options$bayesFactorType == "BF01"){
                    
                    if(options$hypothesis == "notEqualToTestValue"){
                        result_test <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_0u), "pmp[type1]" = .clean(PMP_0),
                                            "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = "", "pmp[type2]" = PMP_u)
                    } else if(options$hypothesis == "greaterThanTestValue"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                           "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                    } else if(options$hypothesis == "lessThanTestValue"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                    } else if (options$hypothesis == "_4type"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                    } else if (options$hypothesis == "allTypes"){
                        result_test <-list(Variable=variable, 
                                           "type[equal]" = "H0: Equal",
                                           "BF[equal]"= "", 
                                           "pmp[equal]" = .clean(PMP_0),
                                           "type[greater]" = "H1: Bigger",
                                           "BF[greater]" = .clean(BF_01), 
                                           "pmp[greater]" = .clean(PMP_1),
                                           "type[less]" = "H2: Smaller",
                                           "BF[less]" = .clean(BF_02),
                                           "pmp[less]" = .clean(PMP_2)) 
                    }
                    
                } else if (options$bayesFactorType == "BF10"){
                    
                    if(options$hypothesis == "notEqualToTestValue"){
                        result_test <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                            "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clean(BF_0u), "pmp[type2]" = PMP_u)
                    } else if(options$hypothesis == "greaterThanTestValue"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                           "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                    } else if(options$hypothesis == "lessThanTestValue"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                    } else if (options$hypothesis == "_4type"){
                        result_test <-list(Variable=variable, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                    } else if (options$hypothesis == "allTypes"){
                        result_test <-list(Variable=variable, 
                                           "type[equal]" = "H0: Equal",
                                           "BF[equal]"= "", 
                                           "pmp[equal]" = .clean(PMP_0),
                                           "type[greater]"= "H1: Bigger",
                                           "BF[greater]" = .clean(BF_01), 
                                           "pmp[greater]" = .clean(PMP_1),
                                           "type[less]" = "H2: Smaller",
                                           "BF[less]" = .clean(BF_02),
                                           "pmp[less]" = .clean(PMP_2)) 
                    }
                    
                }
                
                }
                
                ttest.rows[[i]] <- result_test
            } #
            
            if (options$plotPriorAndPosterior) {
                
                
                if (!is.null(state) && variable %in% state$BFplotvariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$testValue == FALSE && diff$
                                                                                                                                                                 hypothesis == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$plotPriorAndPosterior) {
                    
                    # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                    # then, if the requested plot already exists, use it
                    
                    index <- which(state$BFplotvariables == variable)
                    
                    BFplots[[BFind]] <- state$BFplots[[index]]
                    
                    
                } else {
                    
                    results[["BFplots"]][["collection"]][[BFind]][["status"]] <- "running"
                    
                    if ( ! .shouldContinue(callback(results)))
                        return()
                    
                    plot <- BFplots[[BFind]]
                    
                    index <- which(Bainvariables == variable)
                    
                    if(Bainresult[[i]] == "error"){
                        
                        plot[["data"]] <- ""
                        plot[["error"]] <- list(error="badData", errorMessage=errorFootnotes[[i]])
                        
                    } else {
                    
                    p <- try(silent= FALSE, expr= {
                        
                        .plotFunc <- function() {
                            Bain::plot.BainT(Bainresult[[index]])
                        }
                        content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = .plotFunc)
                        plot[["convertible"]] <- TRUE
                        plot[["obj"]] <- content[["obj"]]
                        plot[["data"]] <- content[["png"]]

                    })
                    
                    }
                    
                    plot[["status"]] <- "complete"
                    
                    BFplots[[BFind]] <- plot
                }
                
                results[["BFplots"]][["collection"]] <- BFplots
                
                BFind <- BFind + 1
                
                if ( ! .shouldContinue(callback(results)))
                    return()

            }
            
            i <- i + 1
        }
        
        
        ttest[["data"]] <- ttest.rows
        ttest[["footnotes"]] <- as.list(footnotes)
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
                    
                    if(Bainresult[[i]] == "error"){
                        
                        plot[["data"]] <- ""
                        plot[["error"]] <- list(error="badData", errorMessage=errorFootnotes[[i]])
                        
                    } else {
                    
                    p <- try(silent= FALSE, expr= {
                        

                        figure <- .plotGroupMeanBayesOneSampleTtest(variable=variableDataDescriptivesPlot, variableName=variable, testValueOpt=options$testValue, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
                            
                        content2 <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = figure, obj = TRUE)
                        plot[["convertible"]] <- TRUE
                        plot[["obj"]] <- content2[["obj"]]
                        plot[["data"]] <- content2[["png"]]
                        
                    })
                    
                    }
                    
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
    
    for(plot in BFplots){
        keep <- c(keep, plot$data)
    }
    for(plot in descriptivesPlots){
        keep <- c(keep, plot$data)
    }
    
    if (perform == "init") {
        
        return(list(results=results, status="inited", state=state, keep=keep))
        
    } else {
        
        return(list(results=results, status="complete", state=list(options=options, results=results, plotTypes=plotTypes,
                                                                   plotVariables=plotVariables, descriptPlotVariables=descriptPlotVariables, descriptivesPlots=descriptivesPlots, BFplots = BFplots, BFplotvariables = BFplotvariables, status=status, plottingError=plottingError,
                                                                   BF10post=BF10post, errorFootnotes=errorFootnotes, Bainvariables = Bainvariables, Bainresult = Bainresult), keep=keep))
        
    }
}