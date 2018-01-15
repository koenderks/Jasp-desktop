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

BainAnovaBayesian <- function (dataset = NULL, options, state = NULL, perform = "run", callback = function(...) list(status = "ok"), ...) {

## TODO: ADD state
## TODO: GET CONSTRAINTS WORKING
## TODO: Fix H0 & H1 in init state Bayes factor matrix
	
## SPECIFY VARIABLES
	bain.variables <- c(unlist(options$dependent), unlist(options$fixedFactors))
	bain.variables <- bain.variables[bain.variables != ""]
	all.variables <- c(unlist(options$dependent), unlist(options$fixedFactors))
	all.variables <- all.variables[all.variables != ""]
	numeric.variables <- c(unlist(options$dependent))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- unlist(options$fixedFactors)
	factor.variables <- factor.variables[factor.variables != ""]

## STATE
	stateKey <- list(
	  bainResult = c("dependent", "fixedFactors", "bayesFactorType"),
	  BFmatrix = c("dependent", "fixedFactors", "bayesFactorType"),
	  descriptives = c("dependent", "fixedFactors", "descriptives"),
	  BFplot = c("dependent", "fixedFactors", "BFplot")
	)
	
	bainResult <- state$bainResult
	BFmatrix <- state$BFmatrix
	descriptives <- state$descriptives
	BFplot <- state$BFplot

## READ IN DATA
	if (is.null(dataset)) {
		
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
		}
		
	} else {
		dataset <- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
	}

## LABEL ADJUSTMENT
	if (options$bayesFactorType == "BF10") {      
        bf.title <- "BF\u2081\u2080"        
    } else if (options$bayesFactorType == "BF01") {               
        bf.title <- "BF\u2080\u2081"        
    }
    
    if(options$logScale == "logBF"){
        bf.title <- paste0("Log(",bf.title,")")
    }
	
## META
	meta <- list ()
	meta[[1]] <- list (name = "title", type = "title")
	meta[[2]] <- list (name = "bainResult", type = "table")
	meta[[3]] <- list(name = "BFmatrix", type = "table")
	meta[[4]] <- list(name = "descriptives", type = "table")
	meta[[5]] <- list(name="BFplot", type="image")

## RESULTS
	results <- list ()
	results [[".meta"]] <- meta
	results [["title"]] <- "Bain ANOVA"

## BAIN ANALYSIS	
	result <- .bainANOVATable(dataset, options, bain.variables, bf.title, perform)
	bainTable <- result[[1]]
	run <- result[[2]] # status check
	bainAnalysis <- result[[3]] # Plotable object
	results[["bainResult"]] <- bainTable

## BF matrix
	if(options$BFmatrix){
		results[["BFmatrix"]] <- .BainBFmatrix(bainAnalysis, options, perform)
	}

## DESCRIPTIVES	
	if (options$descriptives) {    
        results[["descriptives"]] <- .bainANOVADescriptivesTable(dataset, options, bain.variables, factor.variables, all.variables, perform)
    }
	
## PLOT
	if(options$BFplot){
		results[["BFplot"]] <- .bainANCOVAPlot(options, bainAnalysis, run)
	}
	
## KEEP THE PLOTS
	keep <- NULL
	  if (! is.null(names(BFplot)) && "data" %in% names(BFplot)) {
		keep <- BFplot$data
	  }

	if (perform == "run") {
## GIVE THE STATE	
		state <- list(
		  options = options,
		  bainResult = results[["bainResult"]],
		  BFmatrix = results[["BFmatrix"]],
		  descriptives = results[["descriptives"]],
		  BFplot = results[["BFplot"]]
		)
		attr(state, "key") <- stateKey

## END ANALYSIS	
		return (list (results = results, status = "complete", state = state, keep = keep))
	} else {
## END ANALYSIS	
		return (list (results = results, status = "inited", state = state, keep = keep))
	}
}

.bainANOVATable <- function(dataset, options, bain.variables, bf.title, perform){
	
	fields <- list(
		list(name="hypotheses", type="string", title=""),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
		list(name="PMP1", type="number", format="sf:4;dp:3", title="PMP a"),
		list(name="PMP2", type="number", format="sf:4;dp:3", title="PMP b")
	)
	
	bainResult <- list()
	bainResult[["title"]] <- "ANOVA result"
	bainResult[["schema"]] <- list(fields=fields)
	
## MODEL VARIABLES
	dependent <- .v(options$dependent)
	group <- .v(options$fixedFactors)
			
	bainData <- list()
	
	if(perform == "run"){
	
		if(options$model == ""){
			
			ERr1<-matrix(c(1,0,0,0,-1,1,0,0,0,-1,1,0,0,0,-1,1,0,0,0,-1,0,0,0,0),nrow=4,ncol=6)
			IRr1<-matrix(0,0,0)	
			ERr2<-matrix(0,0,0)
			IRr2<-matrix(c(1,0,0,0,-1,1,0,0,0,-1,1,0,0,0,-1,1,0,0,0,-1,0,0,0,0),nrow=4,ncol=6)
		
		} else {
			restrictions <- .restriction.matrices(options$model)
			
			# for (i in 1:length(restrictions)){
			# 	assign(paste0("ER", i), restrictions[[i]][[1]]
			# 	assign(paste0("IR", i), restrictions[[i]][[2]]
			# }
			
			ER1 <- restrictions[[1]][[1]]
			IR1 <- restrictions[[1]][[2]]
			ER2 <- restrictions[[2]][[1]]
			IR2 <- restrictions[[2]][[2]]
		}
		
		if(length(bain.variables) > 1){
		
			p <- try(silent= FALSE, expr= {
				res <- Bain::Bain_anova(X = dataset, dep_var = dependent, group = group, ERr1, IRr1, ERr2, IRr2)
				run <- TRUE
			})
			
			if (class(p) == "try-error") {
				bainData[[1]] <- list(hypotheses = "H1", BF = ".", PMP1 = ".", PMP2 = ".")
				bainData[[2]] <- list(hypotheses = "Hu", BF = ".", PMP1 = ".", PMP2 = ".")
				run <- "error"
				res <- NULL
			} else {
				
				BF <- res$BF
				
				if(options$bayesFactorType == "BF01"){		
					BF <- 1/BF		
				} 
				
				if(options$logScale == "logBF"){
					BF <- log(BF)
				}
				
				for(i in 1:length(BF)){
					bainData[[length(bainData) + 1]] <- list(hypotheses = paste0("H",i), BF = .clean(BF[i]), PMP1 = .clean(res$PMPa[i]), PMP2 = .clean(res$PMPb[i]))
				}
				bainData[[length(bainData) + 1]] <- list(hypotheses = "Hu", BF = "", PMP1 = "", PMP2 = .clean(1-sum(res$PMPb)))
			}
			
		} else {
			
			bainData[[1]] <- list(hypotheses = "H1", BF = ".", PMP1 = ".", PMP2 = ".")
			bainData[[2]] <- list(hypotheses = "Hu", BF = ".", PMP1 = ".", PMP2 = ".")
			run <- FALSE
			
			res <- NULL
			
		}
	
	} else if (perform == "init") {
		
		bainData[[1]] <- list(hypotheses = "H1", BF = ".", PMP1 = ".", PMP2 = ".")
		bainData[[2]] <- list(hypotheses = "Hu", BF = ".", PMP1 = ".", PMP2 = ".")
		run <- FALSE
		
		res <- NULL
		
	}
	
	bainResult[["data"]] <- bainData
	
	if(run == "error"){
		bainResult[["error"]] <- list(errorType = "badData", errorMessage = "Error in the analysis")
	}
	
	# FOOTNOTES
	footnotes <- .newFootnotes()
	message <- "PMP a indicates posterior probability for each hypothesis excluding unconstrained hypothesis. PMP b indicates posterior probability for each hypothesis including unconstrained hypothesis."
	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	bainResult[["footnotes"]] <- as.list(footnotes)
	
	return(list(bainResult, run, res))
}

.bainANOVADescriptivesTable <- function(dataset, options, bain.variables, factor.variables, all.variables, perform){
	
	descriptivesComplete <- FALSE
	
	descriptives <- list()
	
	descriptives[["title"]] <- "Descriptives"
	
	fields <- list(
		list(name="v",    title="",   type="string"),
		list(name="N",    title="N",  type="integer"),
		list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
		list(name="se",   title="SE", type="number",   format="sf:4;dp:3"))
		
		interval <- options$CredibleInterval
		title <- paste0(interval, "% Credible Interval")
		fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
											 format = "sf:4;dp:3", title = "Lower",
											 overTitle = title)
		fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
											 format = "sf:4;dp:3", title = "Upper",
											 overTitle = title)
	
	descriptives[["schema"]] <- list(fields=fields)
			
	result <- list()
	
	if(perform == "run"){
		if(length(all.variables) >= 1){
			
			for(variable in all.variables){
				
				column <- dataset[, .v(variable) ]
				
				if(variable != factor.variables){
					
					posteriorSummary <- .posteriorSummaryGroupMean(variable=column, descriptivesPlotsCredibleInterval=options$CredibleInterval/100)
                    ciLower <- .clean(posteriorSummary$ciLower)
                    ciUpper <- .clean(posteriorSummary$ciUpper)
					
					result[[length(result) + 1]] <- list(v = variable, N = .clean(length(column)), mean = .clean(mean(column)), sd = .clean(sd(column)), 
													se = .clean(sd(column)/sqrt(length(column))), lowerCI = ciLower, upperCI = ciUpper)			
				} else {
					result[[length(result) + 1]] <- list(v = variable, N = .clean(length(column)), mean = "", sd= "", se = "", lowerCI = "", upperCI = "")	
				}
			}
		} else {
			result[[length(result) + 1]] <- list(v = ".", N = ".", mean = ".", sd= ".", se = ".")	
		}
	} else {
		result[[length(result) + 1]] <- list(v = ".", N = ".", mean = ".", sd= ".", se = ".")	
	}
	
	descriptivesComplete <- TRUE
	descriptives[["data"]] <- result
	
	if (descriptivesComplete){
		descriptives[["status"]] <- "complete"
	}
	
	return(descriptives)
	
}
