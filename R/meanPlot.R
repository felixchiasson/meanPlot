######################################################################################
#' @title meanPlot 
#'
#' @description Plots standard error or confidence interval for various descriptive 
#'      statistics under various designs, sampling schemes, population size and purposes.
#'
#' @param data Dataframe in wide format
#' @param bsFactor The name of the columns containing the between-subject factor(s)
#' @param wsFactor The name of the within-subject factor(s)
#' @param factorOrder Order of factors as shown in the graph (x axis, groups, horizontal 
#'       panels, vertical panels)
#' @param measure The dependent variable(s)
#' @param statistic The summary statistic function to use
#' @param errorbar The function that computes the error bar. Should be "CI" or "SE" or 
#'      any function name. Defaults to "SE"
#' @param gamma The converage factor; necessary when errorbar == "CI". Default is 0.95.
#' @param adjustments List of adjustments as described below:
#' @param adjustments$popsize Size of the population under study. Defaults to Inf
#' @param adjustments$purpose The purpose of the comparisons. Defaults to "single". 
#'      Can be "single" or "difference".
#' @param adjustments$decorrelation Decorrelation method for repeated measure designs. 
#'      Chooses among the methods ("CM", "LM", "CA" or "none"). Defaults to "none".
#' Default is adjustments = list(purpose = "single", popSize = Inf, decorrelation = "none")
#' @param showPlot Defaults to TRUE. Set to FALSE if you do not want the output to be a plot.
#' @param plot.type The type of object to plot on the graph. Can be either "bar" or "line".
#'      Defaults to "bar".
#' @param graph.params a list of ggplot2 parameters to input inside geoms (see ?geom_bar2)
#' @param error.params a list of ggplot2 parameters for geom_errobar (see ?geom_errorbar)
#' @param Debug export internal information into global environment. Default is FALSE
#'
#' @return a plot or a table of the summary statistics
#'
#' @examples
#' meanPlot(ToothGrowth, bsFactor = c("dose", "supp"), 
#'   measure = "len", statistic = "mean",
#'   xlab = xlab("Dose"), ylab = ylab("Tooth Growth"),
#'   theme = theme_bw())
#'
#' @export meanPlot
######################################################################################


meanPlot <- function(data, 
    bsFactor     = NULL,                # vector of the between-subject factor columns
    wsFactor     = NULL,                # vector of the names of the within-subject factors
    factorOrder,                        # order of the factors for plots
    measure,                            # dependent variable name(s)
    statistic    = "mean",              # descriptive statistics
    errorbar     = "SE",                # content of the error bars
    gamma        = 0.95,                # coverage if confidence intervals
    adjustments  = list(
        purpose        = "single",      # is "single" or "difference"
        popSize        = Inf,           # is Inf or a specific positive integer
        decorrelation  = "none",        # is "CM", "LM", "CA" or "none"
        samplingScheme = "SRS"          # is "SRS" or "CRS"
    ),
    showPlot     = TRUE,                # show a plot or else statistics
    plot.type    = "bar",               # type of plot
    error.params = list(width = .8),    # sent to ggplot
    graph.params = list(),              # sent to ggplot
    ...,                                # all else is sent as is to ggplot
    Debug        = FALSE                # dump named variables into global env for debugging
) {

    ##############################################################################
    # STEP 0: Load required libraries
    ##############################################################################
    require(ggplot2)
    require(lsr)        # only function wideToLong is used
    require(plyr)       # only function ddply is used
    
    
    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################
    # 1.1: missing adjustements
    if(is.null(adjustments$purpose))        {adjustments$purpose        <- "single"}
    if(is.null(adjustments$popSize))        {adjustments$popSize        <- Inf}
    if(is.null(adjustments$decorrelation))  {adjustments$decorrelation  <- "none"}
    if(is.null(adjustments$samplingScheme)) {adjustments$samplingScheme <- "SRS"}

    # 1.2: unknown adjustments listed
### THIS LINE HAS CHANGED
    if (!all(names(adjustments) %in% c("purpose","popSize","decorrelation","samplingScheme")))
            stop("ERROR: one of the adjustment is unknown. Exiting...")

    # 1.3: invalid choice in a list of possible choices
### THIS LINE HAS CHANGED
    if (is.character(adjustments$popSize)||!(all(adjustments$popSize >0)))  
            stop("ERROR: popSize should be a positive number or Inf (or a list of these). Exiting...")
    if (!(adjustments$purpose %in% c("single","difference"))) 
            stop("ERROR: Invalid purpose. Did you mean 'difference'? Exiting...")
    if (!(adjustments$decorrelation %in% c("none","CM","LM","CA"))) 
            stop("ERROR: Invalid decorrelation method. Did you mean 'CM'? Exiting...")
    if (!(adjustments$samplingScheme %in% c("SRS","CRS"))) 
            stop("ERROR: Invalid samplingScheme. Did you mean 'SRS'? Exiting...")

### THESE LINES ARE NEW
    # 1.4a: innapropriate choice for between-subject specifications
    bsLevels <- dim(unique(data[bsFactor]))[1]
    if (!(length(adjustments$popSize) %in% c(1,bsLevels))) 
            stop("ERROR: popSize is a list whose length does not match the number of groups. Exiting...")
    
    # 1.4b: invalid within-subject factors
    if (any(unlist(gregexpr("\\w\\((\\d+)\\)", wsFactor))== -1))
            stop("ERROR: One of the repeated-measure factor not properly formed 'name(nlevel)'. Exiting...")
    wsMissing <- "DummyWithinSubjectFactor"
    wsLevels <- c(1)
    if (is.null(wsFactor)) {
        wsFactor <- wsMissing
    } else {
        for (i in 1:length(wsFactor)) {
            wsLevels[i] <- as.integer(unlist(strsplit(wsFactor[i], '[()]'))[2])
            wsFactor[i] <-            unlist(strsplit(wsFactor[i], '[()]'))[1]
        }
    }

    wslevel <- prod(wsLevels)
    if (!(length(measure) == wslevel)) 
            stop("ERROR: The number of levels of the within-subject level(s) does not match the number of measure. Exiting...")
    if ((wslevel == 1)&&(!(adjustments$decorrelation == "none"))) 
            stop("ERROR: Decorrelation is not to be used when there is no within-subject factors. Exiting...")
    if(missing(factorOrder))  {factorOrder <- c(wsFactor, bsFactor)}

    # 1.5: invalid column names where column names must be listed
    if (!(all(measure %in% names(data)))) 
            stop("ERROR: One of the measure column is not found in data. Exiting...")
    if (!(all(bsFactor %in% names(data)))) 
            stop("ERROR: One of the bsFactor column is not found in data. Exiting...")

    # 1.6: invalid inputs
    if (length(factorOrder[factorOrder != wsMissing] ) > 4)
            stop("ERROR: Too many factors named on factorOrder. Maximum 4. Exiting...")
    if (length(factorOrder[factorOrder != wsMissing]) < length(wsFactor[wsFactor != wsMissing]) + length(bsFactor)) 
            stop("ERROR: Too few factors named on factorOrder. Exiting...")
    if ((gamma <0)||(gamma>1))
            stop("ERROR: gamma is not within 0 and 1. Exiting...")
    if (!(plot.type %in% c("bar","line"))) 
            stop("ERROR: plot.type must be 'bar' or 'line'. Exiting...")
    if (!is.logical(showPlot))
            stop("ERROR: showPlot must be TRUE or FALSE. Exiting...")

    # 1.7: align levels and corresponding variables
    weird        <-"+!+" # to make sure that these characters are not in the column names
    combinaisons <- expand.grid(lapply(wsLevels,seq))
    newnames     <- paste("DV", apply(combinaisons,1,paste,collapse=weird) ,sep=weird)
    design       <- cbind(combinaisons, measure, newnames)
    colnames(design)[1:length(wsFactor)] <- wsFactor
    colnames(design)[length(wsFactor)+1] <- "variable"
    colnames(design)[length(wsFactor)+2] <- "newvars"
    if (length(wsLevels)>1) {
      cat("Here is how the within-subject variables are understood:\n")
      print( design[,c(wsFactor, "variable") ]) 
    }

    # 1.8: invalid statistical functions 
    widthfct <- paste(errorbar, statistic, sep = ".")
    if ( !(is.exists.function(statistic)) )
            stop("ERROR: The function ", statistic, " is not a known descriptive statistic function. Exiting...")
    if ( !(is.exists.function(widthfct)) )
            stop("ERROR: The function ", widthfct, " is not a known function for error bars. Exiting...")

    # We're clear to go!
    runDebug(Debug, "End of Step 1: Input validation", 
        c("measure2","design2","bsFactor2","wsFactor2","wsLevels2","wslevel2","factorOrder2","adjustments2"), 
        list(measure, design, bsFactor, wsFactor, wsLevels, wslevel, factorOrder, adjustments) )


    ##############################################################################
    # STEP 2: Decorrelate repeated-measure data if needed
    ##############################################################################

    data.wide <- data
    # We do this step for each group and only on columns with repeated measures.
    if (adjustments$decorrelation == "CM" || adjustments$decorrelation == "LM") {
        data.wide <- plyr::ddply(data.wide, .fun = two_step_transform, .variables= bsFactor, measure)    
    }
    # is LM (pooled standard error) needed?
    if (adjustments$decorrelation == "LM") {
        data.wide <- plyr::ddply(data.wide, .fun = pool_sd_transform, .variables= bsFactor, measure) 
    }

    runDebug(Debug, "End of Step 2: Data post decorrelation", 
        c("data.wide2"), list(data.wide) )


    ##############################################################################
    # STEP 3: Put data into long format for conveniency
    ##############################################################################

    # replace variable names with names based on design...
    colnames(data.wide)[grep(paste(measure,collapse="|"),names(data.wide))] = newnames

    # set data to long format using lsr (Navarro, 2015)
    # if no unique identifier is found, a column ".id" may be added; don't bother
    data.long <- suppressWarnings(lsr::wideToLong(data.wide, within = wsFactor, sep = weird))

    # if there was no within-subject factor, a dummy had been added
    if (wsFactor[1]  == wsMissing) {
        # removing all traces of the dummy
        data.long[[wsMissing]] = NULL # remove the column 
        wsFactor = NULL # remove the dummy factor
        factorOrder = factorOrder[ factorOrder != wsMissing]
    }
    
    runDebug(Debug, "End of Step 3: Reformat data frame into long format", 
        c("data.long2"), list(data.long) )


    ##############################################################################
    # STEP 4: Get summary statistics (center, and lowerwidth + upperwidth)
    ##############################################################################

    aggregatefct <- function(subsetOfData) { 
        params1 <- list( subsetOfData$DV  )
        if (is.gamma.required(widthfct)) {
            paramsV <- list( subsetOfData$DV, gamma = gamma )
        } else {
            paramsV <- list( subsetOfData$DV  )
        }
        center <- do.call(statistic, params1)
        limits <- do.call(widthfct, paramsV)
        if (is.interval.function(widthfct)) {
            lowerwidth <- ( min(limits) - center)
            upperwidth <- ( max(limits) - center ) 
        } else {
            lowerwidth <- -limits
            upperwidth <- +limits
        }
        return( c(center=center, lowerwidth=lowerwidth, upperwidth=upperwidth) )
    }

    summaryStatistics <- plyr::ddply( data.long, .fun = aggregatefct, .variables = factorOrder ) 
    summaryStatistics[factorOrder] <- lapply(summaryStatistics[factorOrder], as.factor)

    runDebug(Debug, "End of Step 4: Statistics obtained", 
        c("summaryStatistics2"), list( summaryStatistics) )


    ##############################################################################
    # STEP 5: Get all the adjustments
    ##############################################################################

    # 5.1: Adjust for population size if not infinite 
### THE FOLLOWING LINE IS MODIFIED
    nadj <- if (min(adjustments$popSize) != Inf) {
        # Ns the number of subjects per group
        Ns  <- plyr::ddply(data, .fun = dim, .variables = bsFactor )$V1
### THE FOLLOWING TWO LINES ARE NEW
        ### THE Ns MUST BE EXPANDED FOR EACH REPEATED MEASURES
        Ns  <- rep(Ns, wslevel)
        sqrt(1 - Ns / adjustments$popSize )        
    } else {1}

    # 5.2: Adjust for purpose if "difference"
    padj <- if (adjustments$purpose == "difference") { sqrt(2) } else {1}
    
    # 5.3: Adjust for cluster-randomized sampling
    sadj <- if (adjustments$samplingScheme == "CRS") {
        #### TODO
    } else {1}

    # 5.4: Adjust for correlation if decorrelation == "CA"
    radj <- if (adjustments$decorrelation == "CA") {
### THE FOLLOWING LINES ARE NEW
        rs <- plyr::ddply(data.wide, .fun = meanCorrelation, .variables = bsFactor, cols = measure)
        ### THE rs MUST BE EXPANDED FOR EACH REPEATED MEASURES
        rs  <- rep(rs, wslevel)
        sqrt(1- rs)
    } else {1}

    # All done: apply the corrections to all the widths
    summaryStatistics$lowerwidth = nadj*padj*sadj*radj*summaryStatistics$lowerwidth
    summaryStatistics$upperwidth = nadj*padj*sadj*radj*summaryStatistics$upperwidth

    runDebug(Debug, "End of Step 5: Getting adjustments", 
        c("nadj2","padj2","sadj2","radj2","summaryStatistics3"), list(nadj,padj,sadj,radj,summaryStatistics) )


    ##############################################################################
    # ALL DONE! Output the plot(s) or the summary data
    ##############################################################################

    if (showPlot == TRUE) {
        plot <- make_plot(data = summaryStatistics, type = plot.type,
            x = factorOrder[1],
            y = "center",
            groups = switch(!is.na(factorOrder[2]), factorOrder[2], NULL),
            ymin = "center + lowerwidth",
            ymax = "center + upperwidth",
            error.params = error.params,
            graph.params = graph.params,
            ...)
        if(!is.na(factorOrder[4])) {
            plot + facet_grid(summaryStatistics[[factorOrder[3]]] ~ summaryStatistics[[factorOrder[4]]])
        } else if (!is.na(factorOrder[3])) {
            plot + facet_grid(summaryStatistics[[factorOrder[3]]] ~ .)
        } else {
            plot
        }

    } else {
        # do some renaming of the columns for clearer results
        verbosecol = c(
            statistic,
            if (errorbar == "SE") c("- 1 * SE", "+ 1 * SE") 
            else if (errorbar == "CI") c(paste("-", gamma* 100, "% CI width"), paste("+", gamma* 100, "% CI width") ) 
            else c(paste("-", widthfct), paste("+", widthfct) )
        )
        colnames(summaryStatistics)[(length(factorOrder)+1):(length(factorOrder)+3)] = verbosecol
        return(summaryStatistics)
    }

    ##############################################################################
    # FINISHED! End of function meanPlot
    ##############################################################################
}




#################################################################################
# logical functions:    is.interval.function; is.gamma.required; is.exists.function
# statistics functions: colSDs; meanCorrelation
# tranform functions:   two_step_transform; pool_sd_transform;
# debugging function:   runDebug; 
#################################################################################

is.interval.function <- function(fctname) {
    # is the function provided by the user an interval (e.g., CI) 
    # or a single width (e.g., SE)?
    res <- do.call(fctname, list( c(1,2,3)) )
    if (length(res) == 2) TRUE else FALSE
}

is.gamma.required <- function(fctname) {
    # is the function provided by the user requires a coverage factor
    # gamma (e.g., CI) or not (e.g., SE)?
    res <- tryCatch(
        {do.call(fctname, list( c(1,2,3), gamma = 0.95) ); TRUE},
        error = function(cond) {return(FALSE)}
    )
    res
}

is.exists.function <- function(fctname) {
    # does the function provided by the user exists?
    res <- tryCatch(
        {do.call(fctname, list( c(1,2,3) ) ); TRUE},
        error = function(cond) {return(FALSE)} 
    )
    res
}

meanCorrelation <- function(X, cols) {
    rs   <- cor(X[cols])
    rbar <- mean(rs[upper.tri(rs)])
    rbar
}
    
colSDs = function (x) {
    # the equivalent of colMeans for standard deviations
    if (is.vector(x))          sd(x)
    else if (is.matrix(x))     apply(x, 2, sd)
    else if (is.data.frame(x)) apply(x, 2, sd)
    else "what the fuck??"
}

two_step_transform <- function(dta, measure) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    X <- dta[ measure ]
    C <- ncol(X)
    Y <- X - rowMeans(X) + mean(rowMeans(X))
    Z <- sqrt(C / (C - 1)) * (t(Y) - colMeans(Y)) + colMeans(Y)
    Z <- as.data.frame(t(Z))
    dta [ measure ] = Z
    return(dta)
}

pool_sd_transform <- function(dta, measure) {
    # from Cousineau, in prep.
    Z   <- dta[ measure ]
    sds <- colSDs(Z)
    sdp <- sqrt(mean(sds^2))
    W   <- sdp / sds * (t(Z) - colMeans(Z)) + colMeans(Z)
    W <- as.data.frame(t(W))
    dta [ measure ] = W
    return(dta)
}

runDebug <- function(state, title, vars, vals) { 
    # runDebug provides traces of the vars and
    # reassign them in the globalenv so that we can try commands
    if (state) {
        cat(paste("==>",title,"<==\n"))
        for (i in 1:length(vars)) {
            cat(paste("-",vars[i],"-\n"))
            print(vals[[i]])
            assign(vars[i], vals[[i]], envir = globalenv())
        }
    }
}


#################################################################################
# plotting functions:    make_plot, build_graph
#################################################################################

make_plot <- function(data, type, x, y, ymin, ymax, 
    groups = NULL, 
    error.params = list(),
    graph.params = list(), 
    ... ) {
    Reduce(`+`, list(
        ggplot(data, aes_string(x, y, ymin = ymin, ymax = ymax, fill = groups)) +
        build_graph(graph.params, error.params, type = type),
        ...)
    )
}

build_graph <- function(graph.params = list(), error.params = list(), type) {

    plot.type <- if (type == "bar") {
        do.call(geom_bar, modifyList(
            list(position = position_dodge(),
                stat = "identity"),
                graph.params)
        )
    } else {
        # needs to add geom_line as well as geom_point.
        do.call(geom_point, modifyList(
            list(position = position_dodge(width = .9),
                stat = "identity",
                aes(group=1)),
                graph.params)
        )
    }

    errorbar <- do.call(geom_errorbar, modifyList(
        list(position = position_dodge(.9)),
        error.params)
    )

    list(plot.type, errorbar)

}

