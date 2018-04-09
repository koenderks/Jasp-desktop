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
	  BFplot = c("dependent", "fixedFactors", "BFplot"),
	  plotDescriptives = c("dependent", "fixedFactors", "plotDescriptives")
	)
	
	bainResult <- state$bainResult
	BFmatrix <- state$BFmatrix
	descriptives <- state$descriptives
	BFplot <- state$BFplot
	plotDescriptives <- state$plotDescriptives

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
        bf.title <- "BF.c"        
    } else if (options$bayesFactorType == "BF01") {               
        bf.title <- "BFc."        
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
	meta[[6]] <- list(name = "plotDescriptives", type = "image")

## RESULTS
	results <- list ()
	results [[".meta"]] <- meta
	results [["title"]] <- "Bayesian Informative ANOVA"

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
	
## BF PLOT
	if(options$BFplot){
		results[["BFplot"]] <- .bainANCOVAPlot(options, bainAnalysis, run)
	}
	
## DESCRIPTIVES Plot
	if(options$plotDescriptives){
		results[["plotDescriptives"]] <- .bainDescriptivesPlot(dataset, options, perform)
	}
	
## KEEP THE PLOTS
	keep <- NULL
	  if (! is.null(names(BFplot)) && "data" %in% names(BFplot)) {
		keep <- c(keep, BFplot$data)
	  }
	  if (! is.null(names(plotDescriptives)) && "data" %in% names(plotDescriptives)) {
		keep <- c(keep, plotDescriptives$data)
	  }

	if (perform == "run") {
## GIVE THE STATE	
		state <- list(
		  options = options,
		  bainResult = results[["bainResult"]],
		  BFmatrix = results[["BFmatrix"]],
		  descriptives = results[["descriptives"]],
		  BFplot = results[["BFplot"]],
		  plotDescriptives = result[["plotDescriptives"]]
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
		
		if(length(bain.variables) > 1){
	
		if(options$model == ""){
			
			# We have to make a default matrix depending on the levels of the grouping variable...meh
			# The default hypothesis is that all groups are equal (e.g., 3 groups, "p1=p2=p3")
			groupVars <- options$fixedFactors
			groupVars <- unlist(groupVars)
			
			groupCol <- dataset[ , .v(groupVars)]
			varLevels <- levels(groupCol)
			
			len <- length(varLevels)
			
			null.mat <- matrix(0, nrow = (len-1), ncol = (len+1))
			indexes <- row(null.mat) - col(null.mat)
			null.mat[indexes == 0] <- 1
			null.mat[indexes == -1] <- -1
			
			ERr <- null.mat
		    IRr<-NULL
			
			p <- try(silent= FALSE, expr= {
				res <- Bain::Bain_anova(X = dataset, dep_var = dependent, group = group, ERr, IRr)
				run <- TRUE
			})
		
		} else {
				
			rest.string <- options$model

			rest.string <- gsub("\n", ";", rest.string)
			restrictions <- .restriction.matrices(rest.string)

			lisst <- list()
			if(grepl(";", rest.string)){
			    for (i in 1:length(restrictions[[1]])){
			        lisst[[length(lisst)+1]] <- assign(paste0("ER", i), restrictions[[1]][[i]])
			        lisst[[length(lisst)+1]] <-assign(paste0("IR", i), restrictions[[2]][[i]])
			    }
			} else {
			        lisst[[length(lisst)+1]] <- assign("ER", restrictions[[1]][[1]])
			        lisst[[length(lisst)+1]] <-assign("IR", restrictions[[2]][[1]])
			    }
	
			inpt <- list()
			inpt[[1]] <- dataset
			inpt[[2]] <- dependent
			inpt[[3]] <- group
			inpt <- c(inpt, lisst)
			
			p <- try(silent= FALSE, expr= {
				res <- do.call(Bain::Bain_anova, inpt)
				#res <- Bain::Bain_anova(X = dataset, dep_var = dependent, group = group, ERr1, IRr1, ERr2, IRr2)
				run <- TRUE
			})
		}
			
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
		bainResult[["error"]] <- list(errorType = "badData", errorMessage = "Please correctly identify your constraints.")
	}
	
	# FOOTNOTES
	footnotes <- .newFootnotes()
	message <- "BF.c denotes the Bayes factor of the hypothesis in the row versus its complement. PMP a indicates posterior probability for each hypothesis excluding unconstrained hypothesis. PMP b indicates posterior probability for each hypothesis including unconstrained hypothesis."
	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	bainResult[["footnotes"]] <- as.list(footnotes)
	
	return(list(bainResult, run, res))
}

.bainANOVADescriptivesTable <- function(dataset, options, bain.variables, factor.variables, all.variables, perform){
	
	descriptivesComplete <- FALSE
	
	descriptives <- list()
	
	descriptives[["title"]] <- "Descriptives"
	
	fields <- list(
		list(name="v",    title="Level",   type="string"),
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
		if(length(all.variables) > 1){
			
			groupVars <- options$fixedFactors
			groupVars <- unlist(groupVars)
			
			groupCol <- dataset[ , .v(groupVars)]
			varLevels <- levels(groupCol)
			
			for(variable in varLevels){
				
					column <- dataset[ , .v(options$dependent)]
					column <- column[which(groupCol == variable)]
					
					posteriorSummary <- .posteriorSummaryGroupMean(variable=column, descriptivesPlotsCredibleInterval=options$CredibleInterval/100)
                    ciLower <- .clean(posteriorSummary$ciLower)
                    ciUpper <- .clean(posteriorSummary$ciUpper)
					
					result[[length(result) + 1]] <- list(v = variable, N = .clean(length(column)), mean = .clean(mean(column)), sd = .clean(sd(column)), 
													se = .clean(sd(column)/sqrt(length(column))), lowerCI = ciLower, upperCI = ciUpper)			
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

.bainDescriptivesPlot <- function(dataset, options, perform) {

	descriptivesPlot <- list()
	descriptivesPlot[["title"]] <- "Descriptives plot"
	descriptivesPlot[["width"]]  <- options$plotWidth
	descriptivesPlot[["height"]] <- options$plotHeight
	descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
	
	groupVars <- options$fixedFactors
	groupVars <- unlist(groupVars)

	if (perform == "run" && !is.null(groupVars) && options$dependent != "" && options$fixedFactors != "") {

		groupVarsV <- .v(groupVars)
		dependentV <- .v(options$dependent)

		summaryStat <- .summarySE(as.data.frame(dataset), measurevar = dependentV, groupvars = groupVarsV,
						conf.interval = options$CredibleInterval, na.rm = TRUE, .drop = FALSE, errorBarType = TRUE)

		colnames(summaryStat)[which(colnames(summaryStat) == dependentV)] <- "dependent"
		colnames(summaryStat)[which(colnames(summaryStat) == .v(options$fixedFactors))] <- "plotHorizontalAxis"

		base_breaks_x <- function(x){
			b <- unique(as.numeric(x))
			d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
			list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))
		}

		base_breaks_y <- function(x, plotErrorBars = TRUE){
				ci.pos <- c(x[,"dependent"], x[,"ciLower"], x[,"ciUpper"])
				b <- pretty(ci.pos)
				d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
				list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
					 ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
		}

			summaryStatSubset <- summaryStat
			#########################3
			groupVars <- options$fixedFactors
			groupVars <- unlist(groupVars)
			
			groupCol <- dataset[ , .v(groupVars)]
			varLevels <- levels(groupCol)
			
			ciLower <- NULL
			ciUpper <- NULL
			
			for(variable in varLevels){
				
					column <- dataset[ , .v(options$dependent)]
					column <- column[which(groupCol == variable)]
					
					posteriorSummary <- .posteriorSummaryGroupMean(variable=column, descriptivesPlotsCredibleInterval=options$CredibleInterval/100)
					ciLower <- c(ciLower,.clean(posteriorSummary$ciLower))            
					ciUpper <- c(ciUpper,.clean(posteriorSummary$ciUpper))
							
			}
			
			summaryStatSubset$ciLower <- ciLower
			summaryStatSubset$ciUpper <- ciUpper
			
			summaryStat <- summaryStatSubset

			p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=plotHorizontalAxis,
										y=dependent,
										group=1))


			pd <- ggplot2::position_dodge(.2)
			p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower,
														ymax=ciUpper),
														colour="black", width=.2, position=pd)


		p <- p + ggplot2::geom_line(position=pd, size = .7) +
			ggplot2::geom_point(position=pd, size=4) +
			ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::scale_color_manual(values = rep("black",200),guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::ylab(options$dependent) +
			ggplot2::xlab(groupVars) +
			#ggplot2::labs(shape=options$plotSeparateLines, fill=options$plotSeparateLines) +
			ggplot2::theme_bw() +
			ggplot2::theme(#legend.justification=c(0,1), legend.position=c(0,1),
				panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
				panel.grid.major=ggplot2::element_blank(),
				axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
				axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
				panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
				plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
				legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
				panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
				legend.key = ggplot2::element_blank(), #legend.key.width = grid::unit(10,"mm"),
				legend.title = ggplot2::element_text(size=12),
				legend.text = ggplot2::element_text(size = 12),
				axis.ticks = ggplot2::element_line(size = 0.5),
				axis.ticks.margin = grid::unit(1,"mm"),
				axis.ticks.length = grid::unit(3, "mm"),
				plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
			base_breaks_y(summaryStat, TRUE) +
			base_breaks_x(summaryStatSubset[,"plotHorizontalAxis"])


			descriptivesPlot[["title"]] <- "Descriptives Plot"


			content <- .writeImage(width = options$plotWidth,
								   height = options$plotHeight,
								   plot = p, obj = TRUE)

			
			descriptivesPlot[["data"]] <- content[["png"]]
			descriptivesPlot[["obj"]] <- content[["obj"]]
			descriptivesPlot[["convertible"]] <- TRUE

			descriptivesPlot[["status"]] <- "complete"
			
		} else {
			
			descriptivesPlot[["data"]] <- ""
			
		}
		
		return(descriptivesPlot)

}
