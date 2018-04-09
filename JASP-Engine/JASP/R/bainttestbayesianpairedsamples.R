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

BainTTestBayesianPairedSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

    all.variables <- unique(unlist(options$pairs))
    all.variables <- all.variables[all.variables != ""]

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
    meta[[2]] <- list(name="descriptives", type="table")
    meta[[3]] <- list(name="BFplots", type="collection", meta = "image")
    meta[[4]] <- list(name = "descriptivesPlots", type = "collection", meta = "image")

    results[[".meta"]] <- meta
    results[["title"]] <- "Bayesian Informative T-Test"

    ttest <- list()

    ttest[["title"]] <- "Bayesian Inormative Hypothesis Paired T-Test"

    ttest[["citation"]] <- list(
        "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
        "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225-237.")

    bf.type <- options$bayesFactorType

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

    footnotes <- .newFootnotes()

    if (options$hypothesis == "groupsNotEqual") {
        type <- 1
        note <- "The alternative hypothesis H1 specifies that the mean of variable 1 is unequal to the mean of variable 2. The posterior probabilities are based on equal prior probabilities."
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "groupOneGreater") {
        type <- 2
        note <- "The alternative hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "groupTwoGreater") {
        type <- 3
        note <- "The alternative hypothesis H1 specifies that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "_4type") {
        type <- 4
        note <- "The null hypothesis H0 specifies that the mean of variable 1 is bigger than the mean of variable 2, while the alternative hypothesis H1 specifies that it is smaller. The posterior probabilities are based on equal prior probabilities."
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "allTypes"){
        type <- 5
        note <- "The null hypothesis H0 with equal means is tested against the other hypotheses. The alternative hypothesis H1 states that the mean of variable 1 is bigger than the mean of variable 2. The alternative hypothesis H2 states that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    }

    if (options$hypothesis == "groupsNotEqual" |
        options$hypothesis == "groupOneGreater" |
        options$hypothesis == 'groupTwoGreater' |
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

        fields <- list(
            list(name="Variable", type="string", title=""),
            list(name = "type[equal]", type = "string", title = "Hypothesis"),
            list(name="BF[equal]", type="number", format="sf:4;dp:3", title=bf.title),
            list(name="pmp[equal]", type="number", format="dp:3", title="Posterior probability"),
            list(name = "type[greater]", type = "string", title = "Hypothesis"),
            list(name="BF[greater]", type="number", format="sf:4;dp:3", title="bf.title"),
            list(name="pmp[greater]", type="number", format="dp:3", title="Posterior probability"),
            list(name = "type[less]", type = "string", title = "Hypothesis"),
            list(name = "BF[less]", type = "number", format="sf:4;dp:3", title = bf.title),
            list(name="pmp[less]", type="number", format="dp:3", title="Posterior probability"))

    }

    ttest[["schema"]] <- list(fields=fields)

    ttest.rows <- list()
    plotGroups <- list()
    plots.ttest <- list()
    plotTypes <- list()
    plotPairs <- list()
    descriptivesPlots <- list()
    descriptPlotPairs <- list()
    tablePairs <- list()
    errorFootnotes <- rep("no", length(options$pairs))

    state <- .retrieveState()

    diff <- NULL

    if (!is.null(state)) {

        diff <- .diff(options, state$options)

    }

    if (options$descriptives || options$descriptivesPlots)
        results[["descriptives"]] <- list(title="Descriptives")

    footnotes2 <- .newFootnotes()

    i <- 1

    if(length(options$pairs) == 0){
        currentPair <- ""
    }

    for (pair in options$pairs)
    {

        currentPair <- paste(pair, collapse=" - ")

        if (options$descriptivesPlots) {

            if (!is.null(state) && currentPair %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE && diff$plotWidth == FALSE &&
                                                                                                                                                            diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {

                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it

                index <- which(state$descriptPlotPairs == currentPair)

                descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]


            } else if (!is.null(state) && currentPair %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE && diff$plotWidth == FALSE &&
                                                                                                                                                                   diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {

                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it

                index <- which(state$descriptPlotPairs == currentPair)

                descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]


            } else {

                descriptivesPlot <- list()

                descriptivesPlot[["title"]] <- currentPair
                descriptivesPlot[["status"]] <- "waiting"
                descriptivesPlot[["data"]] <- ""

                descriptivesPlots[[length(descriptivesPlots)+1]] <- descriptivesPlot

            }

            descriptPlotPairs[[length(descriptPlotPairs)+1]] <- currentPair
        }

        if (options$plotPriorAndPosterior) {


            if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                    && diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)))) {

                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it

                stateIndex <- which(state$plotPairs == currentPair)

                plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]

            } else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                           && diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)))) {

                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # if the requested plot already exists use it

                stateIndex <- which(state$plotPairs == currentPair)

                plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]

            } else {

                plot <- list()

                plot[["title"]] <- currentPair
                plot[["status"]] <- "waiting"

                plots.ttest[[length(plots.ttest)+1]] <- plot
            }

            plots.ttest[[i]] <- plots.ttest[[length(plots.ttest)]]

            plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")

        }

        i <- i + 1

    }

    if (options$descriptivesPlots)
        results[["descriptivesPlots"]] <- list(title=ifelse(length(options$pairs) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)
    if (options$plotPriorAndPosterior)
        results[["BFplots"]] <- list(title=ifelse(length(options$pairs) > 1, "Bayes factor Plots", "Bayes factor Plot"), collection=plots.ttest)

    BF10post <- numeric(length(options$pairs))

    for (i in .indices(options$pairs))
    {
        pair <- options$pairs[[i]]

        currentPair <- paste(pair, collapse=" - ")

        tablePairs[[length(tablePairs)+1]] <- paste(pair, collapse=" - ")

        # If only one of pairs is selected
        if ((pair[[1]] == "" || pair[[2]] == "") || pair[[1]] == pair[[2]]) {

            p1 <- ifelse(pair[[1]] != "", pair[[1]], "...")
            p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")

            if(options$hypothesis == "groupsNotEqual"){
                result_test <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                    "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = ".", "pmp[type2]" = ".")
            }
            if(options$hypothesis == "groupTwoGreater"){
                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
            }
            if(options$hypothesis == "groupOneGreater"){
                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = ".", "pmp[type2]" = ".")
            }
            if(options$hypothesis == "_4type"){
                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
            }
            if(options$hypothesis == "allTypes"){
                result_test <-list(Variable=currentPair,
                                   "type[equal]" = "H0: Equal",
                                   "BF[equal]"= ".",
                                   "pmp[equal]" = ".",
                                   "type[greater]"= "H1: Bigger",
                                   "BF[greater]" = ".",
                                   "pmp[greater]" = ".",
                                   "type[less]" = "H2: Smaller",
                                   "BF[less]" = ".",
                                   "pmp[less]" = ".")
            }

        } else {

            # init perform
            if (perform == "init") {

                # If pair is in state
                if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                             && diff$bayesFactorType == FALSE && diff$logscale == FALSE && diff$missingValues == FALSE)))) {

                    stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]

                    # If there was no error
                    if (state$errorFootnotes[stateIndex] == "no") {

                        result_test <- state$results$ttest$data[[stateIndex]]

                        # if there was an error
                    } else {

                        index2 <- .addFootnote(footnotes2, state$errorFootnotes[stateIndex])

                        errorFootnotes[i] <- state$errorFootnotes[stateIndex]

                        if(options$hypothesis == "groupsNotEqual"){
                            result_test <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
                                                "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        }
                        if(options$hypothesis == "groupTwoGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
                                               "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        }
                        if(options$hypothesis == "groupOneGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
                                               "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        }
                        if(options$hypothesis == "_4type"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
                                               "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        }
                        if(options$hypothesis == "allTypes"){
                            result_test <-list(Variable=currentPair,
                                               "type[equal]" = "H0: Equal",
                                               "BF[equal]"= .clean(NaN),
                                               "pmp[equal]" = .clean(NaN),
                                               "type[greater]"= "H1: Bigger",
                                               "BF[greater]" = .clean(NaN),
                                               "pmp[greater]" = .clean(NaN),
                                               "type[less]" = "H2: Smaller",
                                               "BF[less]" = .clean(NaN),
                                               "pmp[less]" = .clean(NaN))
                        }
                    }

                    # if pair is not in state
                } else {

                    if(options$hypothesis == "groupsNotEqual"){
                        result_test <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                            "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = ".", "pmp[type2]" = ".")
                    }
                    if(options$hypothesis == "groupTwoGreater"){
                        result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
                    }
                    if(options$hypothesis == "groupOneGreater"){
                        result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                           "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = ".", "pmp[type2]" = ".")
                    }
                    if(options$hypothesis == "_4type"){
                        result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                                           "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
                    }
                    if(options$hypothesis == "allTypes"){
                        result_test <-list(Variable=currentPair,
                                           "type[equal]" = "H0: Equal",
                                           "BF[equal]"= ".",
                                           "pmp[equal]" = ".",
                                           "type[greater]"= "H1: Bigger",
                                           "BF[greater]" = ".",
                                           "pmp[greater]" = ".",
                                           "type[less]" = "H2: Smaller",
                                           "BF[less]" = ".",
                                           "pmp[less]" = ".")
                    }
                }

                # if perform is run
            } else {

                unplotable <- FALSE
                unplotableMessage <- NULL

                # if pair is in state
                if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                             && diff$bayesFactorType == FALSE && diff$logscale == FALSE && diff$missingValues == FALSE)))) {

                    stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]

                    # if there was no error
                    if (state$errorFootnotes[stateIndex] == "no") {

                        result_test <- state$results$ttest$data[[stateIndex]]

                        # if there was an error
                    } else {

                        index2 <- .addFootnote(footnotes2, state$errorFootnotes[stateIndex])

                        errorFootnotes[i] <- state$errorFootnotes[stateIndex]

                        if(options$hypothesis == "groupsNotEqual"){
                            result_test <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                                "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = ".", "pmp[type2]" = ".", .footnotes=list(BF=list(index2)))
                        }
                        if(options$hypothesis == "groupTwoGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                               "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = ".", .footnotes=list(BF=list(index2)))
                        }
                        if(options$hypothesis == "groupOneGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                               "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = ".", "pmp[type2]" = ".", .footnotes=list(BF=list(index2)))
                        }
                        if(options$hypothesis == "_4type"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                                               "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = ".", .footnotes=list(BF=list(index2)))
                        }
                        if(options$hypothesis == "allTypes"){
                            result_test <-list(Variable=currentPair,
                                               "type[equal]" = "H0: Equal",
                                               "BF[equal]"= ".",
                                               "pmp[equal]" = ".",
                                               "type[greater]"= "H1: Bigger",
                                               "BF[greater]" = ".",
                                               "pmp[greater]" = ".",
                                               "type[less]" = "H2: Smaller",
                                               "BF[less]" = ".",
                                               "pmp[less]" = ".", .footnotes=list(BF=list(index2)))
                        }
                    }

                    BF10post[i] <- state$BF10post[stateIndex]

                    # if pair is not in state
                } else {

                    # if there is a pair
                    if(length(pair) > 0){

                        subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
                        subDataSet <- na.omit(subDataSet)

                        c1 <- subDataSet[[ .v(pair[[1]]) ]]
                        c2 <- subDataSet[[ .v(pair[[2]]) ]]

                        r <- Bain::Bain_ttestData(c1, c2, paired = TRUE,type = type)

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


                            if(options$hypothesis == "groupsNotEqual"){
                                result_test <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_0u), "pmp[type1]" = .clean(PMP_0),
                                                    "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_u))
                            }
                            if(options$hypothesis == "groupTwoGreater"){
                                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                            }
                            if(options$hypothesis == "groupOneGreater"){
                                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                                   "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                            }
                            if(options$hypothesis == "_4type"){
                                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                            }
                            if(options$hypothesis == "allTypes"){
                                result_test <-list(Variable=currentPair,
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

                        } else if (options$bayesFactorType == "BF10"){

                            if(options$hypothesis == "groupsNotEqual"){
                                result_test <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                                    "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clean(BF_0u), "pmp[type2]" = .clean(PMP_u))
                            }
                            if(options$hypothesis == "groupTwoGreater"){
                                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                            }
                            if(options$hypothesis == "groupOneGreater"){
                                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                                   "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                            }
                            if(options$hypothesis == "_4type"){
                                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                            }
                            if(options$hypothesis == "allTypes"){
                                result_test <-list(Variable=currentPair,
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
                }
            }
        }

        ttest.rows[[length(ttest.rows)+1]] <- result_test

    }

    # if there is no pair
    if (length(ttest.rows) == 0){
        if(options$hypothesis == "groupsNotEqual"){
            ttest.rows <- list(list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                    "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = ".", "pmp[type2]" = "."))
        }
        if(options$hypothesis == "groupTwoGreater"){
            ttest.rows <-list(list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = "."))
        }
        if(options$hypothesis == "groupOneGreater"){
            ttest.rows <-list(list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = ".", "pmp[type2]" = "."))
        }
        if(options$hypothesis == "_4type"){
            ttest.rows <-list(list(Variable=currentPair, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = ".", "pmp[type2]" = "."))
        }
        if(options$hypothesis == "allTypes"){
            ttest.rows <-list(list(Variable=currentPair,
                                   "type[equal]" = "H0: Equal",
                                   "BF[equal]"= ".",
                                   "pmp[equal]" = ".",
                                   "type[greater]"= "H1: Bigger",
                                   "BF[greater]" = ".",
                                   "pmp[greater]" = ".",
                                   "type[less]" = "H2: Smaller",
                                   "BF[less]" = ".",
                                   "pmp[less]" = "."))
        }
    }

    ttest[["data"]] <- ttest.rows
    ttest[["footnotes"]] <- as.list(footnotes)


    if (perform == "run")
        ttest[["status"]] <- "complete"

    results[["ttest"]] <- ttest

    descriptives <- NULL

    if (options$descriptives) {

        descriptives <- list()

        descriptives[["title"]] <- "Descriptives"

        fields <- list(
            list(name="v", type="string", title=""),
            list(name="N",                  type="integer"),
            list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
            list(name="sd",   title="SD",   type="number", format="sf:4;dp:3"),
            list(name="se",   title="SE",   type="number", format="sf:4;dp:3"))

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

        variables <- unlist(options$pairs)
        variables <- unique(variables)
        variables <- variables[variables != ""]

        for (variable in variables) {

            if (perform == "run") {

                result <- try (silent = TRUE, expr = {

                    variableData <- dataset[[.v(variable)]]
                    variableDataOm <- na.omit(variableData)

                    posteriorSummary <- .posteriorSummaryGroupMean(variable=variableDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
                    ciLower <- .clean(posteriorSummary$ciLower)
                    ciUpper <- .clean(posteriorSummary$ciUpper)

                    n <- .clean(as.numeric(length(variableDataOm)))
                    m <- .clean(as.numeric(mean(variableDataOm)))
                    std <- .clean(as.numeric(sd(variableDataOm)))
                    if(is.numeric(std)){
                        se <- .clean(as.numeric(std/sqrt(n)))}
                    else
                        se <- .clean(NaN)

                    list(v=variable, N=n, mean=m, sd=std, se=se, lowerCI=ciLower, upperCI=ciUpper)
                })

                if (class(result) == "try-error") {

                    result <- list(v=variable, N="", mean="", sd="", se="", lowerCI="", upperCI="")
                }

            } else {

                result <- list(v=variable, N=".", mean=".", sd=".", se=".", lowerCI=".", upperCI=".")
            }

            descriptives.results[[length(descriptives.results)+1]] <- result
        }
            
        for (i in .indices(options$pairs)) {

        pair <- options$pairs[[i]]
        
        if (!(pair[[1]] == "" || pair[[2]] == "" || pair[[1]] == pair[[2]])) {
        
        subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])))
        subDataSet <- na.omit(subDataSet)

        c1 <- subDataSet[[ .v(pair[[1]]) ]]
        c2 <- subDataSet[[ .v(pair[[2]]) ]]
        
        currentPair <- paste(pair, collapse=" - ")
        diff <- c1-c2
        meandiff <- mean(diff)
        sd <- sd(diff)        
        se <- sqrt(var(c1) + var(c2) - (2*cor(c1,c2)*sd(c1)*sd(c2)))/sqrt(length(diff))
        N <- length(diff)
        
        ciLower <- meandiff - 1.96*se
        ciUpper <- meandiff + 1.96*se
        
        result <- list(v=currentPair, N=.clean(N), mean=.clean(meandiff), sd=.clean(sd), se=.clean(se), lowerCI=.clean(ciLower), upperCI=.clean(ciUpper))
        
        descriptives.results[[length(descriptives.results)+1]] <- result

        }
        
    }

        descriptives[["data"]] <- descriptives.results
        descriptives[["status"]] <- "complete"


    }

    results[["descriptives"]] <- descriptives


    # PLOTS

    if (perform == "run" && length(options$pairs) > 0 && (options$plotPriorAndPosterior || options$descriptivesPlots)) {

        if ( ! .shouldContinue(callback(results)))
            return()

        j <- 1
        descriptInd <- 1

        for (i in .indices(options$pairs)) {

            pair <- options$pairs[[i]]

            status <- list(unplotable = FALSE)

            p1 <- ifelse(pair[[1]] != "", pair[[1]], "...")
            p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")

            if (perform == "run" && status$unplotable == FALSE && p2 != "..." && p1 != "...") {

                subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
                subDataSet <- na.omit(subDataSet)

                c1 <- subDataSet[[ .v(pair[[1]]) ]]
                c2 <- subDataSet[[ .v(pair[[2]]) ]]

                ind <- which(c1 == c1[1])
                idData <- sum((ind+1)-(1:(length(ind))) == 1)

                ind2 <- which(c2 == c2[1])
                idData2 <- sum((ind2+1)-(1:(length(ind2))) == 1)
            }
            else
            {
                c1 <- NULL
                c2 <- NULL
            }

            if (options$descriptivesPlots) {

                if (!is.null(state) && tablePairs[[i]] %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE &&
                                                                                                                                                                    diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
                    # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                    # then, if the requested plot already exists, use it

                    index <- which(state$descriptPlotPairs == tablePairs[[i]])

                    descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]


                } else {

                    results[["descriptivesPlots"]][["collection"]][[descriptInd]][["status"]] <- "running"

                    if ( ! .shouldContinue(callback(results)))
                        return()

                    plot <- descriptivesPlots[[descriptInd]]

                    p <- try(silent= FALSE, expr= {

                        difference <- c1 - c2
                        figure <- .plotGroupMeanBayesOneSampleTtest(variable=difference, variableName=paste0(pair[[1]]," - ", pair[[2]]), testValueOpt=0, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)

                        content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = figure, obj = TRUE)
                        plot[["convertible"]] <- TRUE
                        plot[["obj"]] <- content[["obj"]]
                        plot[["data"]] <- content[["png"]]

                    })

                    if (class(p) == "try-error") {

                        errorMessageTmp <- .extractErrorMessage(p)
                        errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
                        plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
                    }

                    plot[["status"]] <- "complete"
                    descriptivesPlots[[descriptInd]] <- plot

                }

                results[["descriptivesPlots"]][["collection"]] <- descriptivesPlots

                if ( ! .shouldContinue(callback(results)))
                    return()

                descriptInd <- descriptInd + 1

            }

            if (options$plotPriorAndPosterior) {


                if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                            && diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)))) {

                    # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                    # if the requested plot already exists use it

                    stateIndex <- which(state$plotPairs == tablePairs[[i]])

                    plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]

                } else {

                    #results[["BFplots"]][["collection"]][[i]][["status"]] <- "running"

                    if ( ! .shouldContinue(callback(results)))
                        return()

                    plot <- plots.ttest[[j]]


                    if (status$unplotable == FALSE) {


                        p <- try(silent= FALSE, expr= {

                            .plotFunc2 <- function() {
                                Bain::plot.BainT(Bain::Bain_ttestData(x = c1, y = c2,type = type, paired = TRUE))
                            }
                            content2 <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = .plotFunc2, obj = TRUE)
                            plot[["convertible"]] <- TRUE
                            plot[["obj"]] <- content2[["obj"]]
                            plot[["data"]] <- content2[["png"]]

                        })


                        if (class(p) == "try-error") {

                            errorMessage <- .extractErrorMessage(p)

                            if (errorMessage == "not enough data") {

                                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
                            } else if (errorMessage == "'from' cannot be NA, NaN or infinite") {

                                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
                            }

                            plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
                        }

                    } else if (status$unplotable && "unplotableMessage" %in% names(status)) {

                        message <- paste("Plotting is not possible:", status$unplotableMessage)
                        plot[["error"]] <- list(error="badData", errorMessage=message)
                    }

                    plot[["status"]] <- "complete"

                    plots.ttest[[j]] <- plot
                }

                results[["BFplots"]][["collection"]] <- plots.ttest

                if ( ! .shouldContinue(callback(results)))
                    return()

                j <- j + 1
            }

        }
    }

    results[["ttest"]] <- ttest

    keep <- NULL

    for (plot in plots.ttest)
        keep <- c(keep, plot$data)

    for (plot in descriptivesPlots)
        keep <- c(keep, plot$data)

    if (perform == "init") {

        return(list(results=results, status="inited", state=state, keep=keep))

    } else {

        return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes, plotPairs=plotPairs,
                                                                   descriptPlotPairs=descriptPlotPairs, descriptivesPlots=descriptivesPlots, BF10post=BF10post, tablePairs=tablePairs,
                                                                   errorFootnotes=errorFootnotes), keep=keep))
    }

}
