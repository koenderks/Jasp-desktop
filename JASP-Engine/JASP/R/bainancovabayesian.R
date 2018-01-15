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

BainAncovaBayesian	 <- function (dataset = NULL, options, state = NULL, perform = "run", callback = function(...) list(status = "ok"), ...) {
	
## TODO: ADD state
## TODO: GET CONSTRAINTS WORKING
## TODO: Fix H0 & H1 in init state Bayes factor matrix
	
## SPECIFY VARIABLES
	bain.variables <- c(unlist(options$dependent),unlist(options$covariates)[1],unlist(options$fixedFactors))
	bain.variables <- bain.variables[bain.variables != ""]
	all.variables <- c(unlist(options$dependent),unlist(options$covariates),unlist(options$fixedFactors))
	all.variables <- all.variables[all.variables != ""]
	numeric.variables <- c(unlist(options$dependent),unlist(options$covariates))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- unlist(options$fixedFactors)
	factor.variables <- factor.variables[factor.variables != ""]

## STATE
	stateKey <- list(
	  bainResult = c("dependent", "fixedFactors", "covariates", "bayesFactorType"),
	  BFmatrix = c("dependent", "fixedFactors", "covariates", "bayesFactorType"),
	  descriptives = c("dependent", "fixedFactors", "covariates", "descriptives"),
	  BFplot = c("dependent", "fixedFactors", "covariates", "BFplot")
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
	results [["title"]] <- "Bain ANCOVA"

## BAIN ANALYSIS	
	result <- .bainANCOVATable(dataset, options, bain.variables, bf.title, perform)
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
        results[["descriptives"]] <- .bainANCOVADescriptivesTable(dataset, options, bain.variables, factor.variables, all.variables, perform)
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
		  descriptives = result[["descriptives"]],
		  BFplot = result[["BFplot"]]
		)
		attr(state, "key") <- stateKey

## END ANALYSIS	
		return (list (results = results, status = "complete", state = state, keep = keep))
	} else {
## END ANALYSIS	
		return (list (results = results, status = "inited", state = state, keep = keep))
	}
}

.bainANCOVATable <- function(dataset, options, bain.variables, bf.title, perform){
	
	fields <- list(
		list(name="hypotheses", type="string", title=""),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
		list(name="PMP1", type="number", format="sf:4;dp:3", title="PMP a"),
		list(name="PMP2", type="number", format="sf:4;dp:3", title="PMP b")
	)
	
	bainResult <- list()
	bainResult[["title"]] <- "ANCOVA result"
	bainResult[["schema"]] <- list(fields=fields)
	
## MODEL VARIABLES
	dependent <- .v(options$dependent)
	group <- .v(options$fixedFactors)
	covariates <- .v(options$covariates)
			
	bainData <- list()
	
	if(perform == "run"){
	
		if(options$model == ""){
			
			ERr1<-matrix(c(1,0,0,0,-1,1,0,0,0,-1,1,0,0,0,-1,1,0,0,0,-1,0,0,0,0),nrow=4,ncol=6)
			IRr1<-matrix(0,0,0)
			ERr2<-matrix(0,0,0)
			IRr2<-matrix(c(-1,0,0,-1,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,-1,0,0,-1,0,0,0,-1,0,0,-1,0,0,0,0,0,0),nrow=6,ncol=6)
		
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
		
		if(length(bain.variables) > 2){
		
			p <- try(silent= FALSE, expr= {
				res <- Bain::Bain_ancova(X = dataset, dep_var = dependent, covariates = covariates, group = group, ERr1, IRr1, ERr2, IRr2)
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

.BainBFmatrix <- function(res, options, perform){
	
	BFmatrix <- diag(1, length(res$BF))
	
	for (h1 in 1:length(res$BF)) {
	    for (h2 in 1:length(res$BF)) {
	        BFmatrix[h1, h2] <- res$fit[h1]/res$fit[h2]/(res$complexity[h1]/res$complexity[h2])
	    }
	}
	
	if(options$logScale == "logBF"){
		BFmatrix <- log(BFmatrix)
	}
	
	if(perform == "init"){
		BFmatrix <- matrix(rep(NA, 4), nrow = 2, ncol = 2)
	}
	
	BainMatrix <- list()
	BainMatrix[["title"]] <- "Bayes factor matrix"
	
	fields <- list()
	fields[[1]] <- list(name = "hypothesis", title = "", type = "string")
	for(i in 1:nrow(BFmatrix)){
		fields[[length(fields)+1]] <- list(name = paste0("H", i), title = paste0("H", i), type = "number", format="sf:4;dp:3")
	}
	
	BainMatrix[["schema"]] <- list(fields = fields)
	
	result <- list()
	if(perform == "run"){
		
		for(i in 1:nrow(BFmatrix)){
			tmp <- list(hypothesis = paste0("H", i))
			for(j in 1:ncol(BFmatrix)){
				tmp[[paste0("H", j)]] <- .clean(BFmatrix[i,j])
			}
		result[[length(result) + 1]] <- tmp
		}
		
	} else {
		
		for(i in 1:nrow(BFmatrix)){
			tmp <- list(hypothesis = paste0("H", i))
			for(j in 1:ncol(BFmatrix)){
				tmp[[paste0("H", j)]] <- "."
			}
		result[[length(result) + 1]] <- tmp
		}
		
	}
	
	BainMatrix[["data"]] <- result
	
	# FOOTNOTES
	if(options$logScale == "logBF"){
		footnotes <- .newFootnotes()
		message <- "Logarithmic Bayes factors are displayed."
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		BainMatrix[["footnotes"]] <- as.list(footnotes)
	}
	
	return(BainMatrix)
	
}

.bainANCOVADescriptivesTable <- function(dataset, options, bain.variables, factor.variables, all.variables, perform){
	
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

.bainANCOVAPlot <- function(options, bainAnalysis, run){
	
	BFplot <- list()
	BFplot[["title"]] <- "Bayes factor comparison"
	BFplot[["width"]]  <- options$plotWidth
	BFplot[["height"]] <- options$plotHeight
	BFplot[["custom"]] <- list(width="plotWidth", height="plotHeight")

	if(!is.null(bainAnalysis)){
		
		if(run == "error"){
			
			BFplot[["data"]] <- ""
			BFplot[["error"]] <- list(error="badData", errorMessage="Something is up.........")
			
		} else {
		
		p <- try(silent= FALSE, expr= {
			
			.plotFunc <- function() {
				Bain::plot.BainA(bainAnalysis)
			}
			content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = .plotFunc)
			BFplot[["convertible"]] <- TRUE
			BFplot[["obj"]] <- content[["obj"]]
			BFplot[["data"]] <- content[["png"]]

		})
		
		}	

	}
	
	return(BFplot)
	
}
################################### FUNCTION TO READ IN CONSTRAINTS #####################################
.restriction.matrices <- function(hypotheses) {
    suppressWarnings(library(stringr))
    #hypotheses = "(p1,p2)=(p2,p3)=(p4,p5,p6,p5)"
    # hypotheses = "p9-p3+1=p10"
    #h = 1
    #i = 1
    hypotheses <- gsub("not", "!", hypotheses)
    # give all "variables" that are actually a numeric a "#" as an identifier
    hypotheses <- gsub("([=<>,;+-]*)((?<![\\w])[\\d\\.]+)([=<>,;+-]*)", "\\1\\2#\\3", hypotheses, perl=TRUE)
    if (!grepl("[ a-zA-Z]", hypotheses)) stop("Incorrect hypothesis/hypotheses formulated: no variables")
    hypotheses <- gsub("[ a-zA-Z]", "", hypotheses)
    if (!grepl("\\d", hypotheses)) stop("Incorrect hypothesis/hypotheses formulated: no variable identifier")
    if (!grepl("[0-9=<>,;!()#]", hypotheses)) stop("Hypotheses contain inadmissible character(s).")
    vars = unlist(strsplit(gsub("[!()]", "", hypotheses), split="[=<>,;+-]+"))
    vars = unlist(strsplit(gsub("[!()]", "", hypotheses), split="[=<>,;+-]+"))
    vars = subset(vars, vars != "") 
    J <- max(as.numeric(vars[!grepl("\\#", vars)]))
    hypotheses <- unlist(strsplit(hypotheses, split=";"))
    
    n_hypotheses <- length(hypotheses)
    n_unconstrained <- 0
    results <- list()
    
    resultsIQ <- list()
    resultsEQ <- list()
    
    for (h in 1:length(hypotheses)) {
        hypothesis <- hypotheses[h] 
        complement <- F
        if (grepl("!", hypothesis)) {
            if (grepl("=", hypothesis)){
                results[[h]] <- "Unconstrained"
                n_unconstrained = n_unconstrained+1
                next
            }
            complement <- T
        }
        if (grepl("[()]", hypothesis)) {
            hypothesis <- gsub("[()]", "", hypothesis)
        }
        num <- suppressWarnings(!is.na(as.numeric(unlist(strsplit(hypothesis, split="")))))
        num2 <- "#" == c(unlist(strsplit(hypothesis, split="") ))
        dot <- "." == c(unlist(strsplit(hypothesis, split="") ))
        minus <- "-" == c(unlist(strsplit(hypothesis, split="") ))
        plus <- "+" == c(unlist(strsplit(hypothesis, split="") ))
        comma <- "," == c(unlist(strsplit(hypothesis, split="") ))
        
        eqs.pos <- unlist(gregexpr("=", hypothesis))
        eqs.pos <- eqs.pos[eqs.pos>0]
        ineqs.pos <- unlist(list(gregexpr("<", hypothesis), gregexpr(">", hypothesis)))
        ineqs.pos <- ineqs.pos[ineqs.pos>0]
        
        if (length(eqs.pos)+length(ineqs.pos)==0) {
            results[[h]] <- "Unconstrained"
            n_unconstrained = n_unconstrained+1
            next
        }
        
        # Count number of rows based on number of operators and commas
        text = unlist(strsplit(hypothesis, split="="))
        RE_total = length(text)-1
        prev = 1
        if(grepl(",",text[1])){
            test= c(unlist(strsplit(text[1], split="") ))
            first <- "," == test
            first <- sum(first)+1
            prev = first
        }
        
        for (s in 2:length(text)){
            count =1
            if(grepl(",",text[s])){
                test= c(unlist(strsplit(text[s], split="") ))
                count <- "," == test
                count <- sum(count)+1
            }
            subtotal <- prev* count-1
            prev= count
            RE_total = RE_total+ subtotal
        }
        
        text = unlist(strsplit(hypothesis, split="[<>]"))
        prev = 1
        RI_total = length(text)-1
        
        if(grepl(",",text[1])){
            test= c(unlist(strsplit(text[1], split="") ))
            first <- "," == test
            first <- sum(first)+1
            prev = first
        }
        
        for (s in 2:length(text)){
            count =1
            if(grepl(",",text[s])){
                test= c(unlist(strsplit(text[s], split="") ))
                count <- "," == test
                count <- sum(count)+1
            }
            subtotal <- prev* count-1
            prev= count
            RI_total = RI_total+ subtotal
        }
        EQ <- IQ <- F
        RE <- RI <- NULL
        #Create RE and RI
        if (length(eqs.pos)>0 ){
            EQ =T
            RE <- matrix(0, nrow=RE_total, ncol=J+1)
        }
        # Create a row in RI for every inequal sign 
        if (length(ineqs.pos)>0){
            IQ =T
            RI <- matrix(0, nrow=RI_total, ncol=J+1)
        }
        if(EQ){
            j=1
            for (i in 1:length(eqs.pos)) {
                lower <- upper <- eq <- eqs.pos[i] 
                while( !(lower==1) && (num[lower-1]|dot[lower-1]|plus[lower-1]|minus[lower-1]|comma[lower-1]|num2[lower-1] ) ) lower <- lower-1
                while(!(upper==length(num)) && (num[upper+1]|dot[upper+1]|plus[upper+1]|minus[upper+1]|num2[upper+1]|comma[upper+1]) ) upper <- upper+1
                left_full = unlist(substr(hypothesis, lower, eq-1))
                right_full = unlist(substr(hypothesis, eq+1, upper))
                left_split = unlist(strsplit(left_full, split=","))
                right_split = unlist(strsplit(right_full, split=","))
                for (left in  left_split){
                    for (right in right_split){
                        right_coefs = unlist(strsplit(right, split="[=<>+-]"))
                        right_minus <- "-" == c(unlist(strsplit(right, split="") ))
                        for (c in right_coefs){
                            begin = str_locate(right, c)[1]
                            negate = 1
                            input = 1
                            if (begin != 1 && right_minus[begin-1]) {
                                negate <- -1
                            }
                            if(grepl("\\#", c)){
                                RE[j, dim(RE)[2]] <- -1*negate*as.numeric(gsub("#","",c))
                            }
                            else{
                                RE[j, as.numeric(c)] <- negate*input
                            }
                        } 
                        left_coefs = unlist(strsplit(left, split="[=<>+-]"))
                        left_minus <- "-" == c(unlist(strsplit(left, split="") ))
                        for (c in left_coefs){
                            begin = str_locate(left, c)[1]
                            negate = 1
                            input = -1
                            if (begin != 1 && left_minus[begin-1]) {
                                negate <- -1
                            }
                            if(grepl("\\#", c)){
                                RE[j, dim(RE)[2]] <- negate*as.numeric(gsub("#","",c))
                            }
                            else{
                                RE[j, as.numeric(c)] <- negate*input
                            }
                        }
                        j = j+1
                    }
                }
            }
        }else{
            RE <- matrix(0,0,0)
        }
        
        if(IQ){
            j=1
            for (i in 1:length(ineqs.pos)) {
                lower <- upper <- eq <- ineqs.pos[i] 
                sign = substr(hypothesis, lower, upper)
                while( !(lower==1) && (num[lower-1]|dot[lower-1]|plus[lower-1]|minus[lower-1]|comma[lower-1]|num2[lower-1] ) ) lower <- lower-1
                while(!(upper==length(num)) && (num[upper+1]|dot[upper+1]|plus[upper+1]|minus[upper+1]|num2[upper+1]|comma[upper+1]) ) upper <- upper+1
                if (sign == "<"){
                    left_full = unlist(substr(hypothesis, lower, eq-1))
                    right_full = unlist(substr(hypothesis, eq+1, upper))
                }
                else{
                    right_full = unlist(substr(hypothesis, lower, eq-1))
                    left_full = unlist(substr(hypothesis, eq+1, upper))
                }
                left_split = unlist(strsplit(left_full, split=","))
                right_split = unlist(strsplit(right_full, split=","))
                print(left_split)
                print(right_split)
                for (left in  left_split){
                    for (right in right_split){
                        right_coefs = unlist(strsplit(right, split="[=<>+-]"))
                        right_minus <- "-" == c(unlist(strsplit(right, split="") ))
                        for (c in right_coefs){
                            begin = str_locate(right, c)[1]
                            negate = 1
                            input = 1
                            if (begin != 1 && right_minus[begin-1]) {
                                negate <- -1
                            }
                            if(grepl("\\#", c)){
                                RI[j, dim(RI)[2]] <- -1*negate*as.numeric(gsub("#","",c))
                            }
                            else{
                                RI[j, as.numeric(c)] <- negate*input
                            }
                        } 
                        left_coefs = unlist(strsplit(left, split="[=<>+-]"))
                        left_minus <- "-" == c(unlist(strsplit(left, split="") ))
                        for (c in left_coefs){
                            begin = str_locate(left, c)[1]
                            negate = 1
                            input = -1
                            if (begin != 1 && left_minus[begin-1]) {
                                negate <- -1
                            }
                            if(grepl("\\#", c)){
                                RI[j, dim(RI)[2]] <- negate*as.numeric(gsub("#","",c))
                            }
                            else{
                                RI[j, as.numeric(c)] <- negate*input
                            }
                        }
                        j = j+1
                    }
                }
            }
        }else{
            RI = matrix(0,0,0)
        }
        results[[h]] <- list(RE, RI, complement)
        names(results[[h]]) <- c("Equalities", "Inequalities", "Complement")
        resultsIQ[[h]] <- RI
        resultsEQ[[h]] <- RE
    }
    if (n_hypotheses == n_unconstrained){
        stop("no equality/inequality constrained hypothesis formulated")
    }
    names(results) <- paste("H", 1:length(hypotheses), sep="")
    
    #return(list(resultsEQ,resultsIQ))
    return(list(resultsEQ,resultsIQ))
}
