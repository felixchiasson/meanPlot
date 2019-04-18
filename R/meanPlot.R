#' @title Computes standard error and confidence intervals under various designs and sampling schemes
#'
#' @description Computes standard error and confidence interval under various designs and sampling schemes
#'
#' @param data Data frame
#' @param bsfactor The name of your between-subject factor(s)
#' @param wsfactor The name of your within-subject factor(s)
#' @param factorOrder Order of factors as shown in the graph (x axis, groups, panels)
#' @param measure Your dependent variable
#' @param errorbar The content of your error bar. Can either be "CI" or "SE".
#' Defaults to "SE"
#' @param gamma Only necessary if errorbar == "CI". The confidence level.
#' @param adjustments List of adjustments as described below
#' @param popsize Size of the population under study if known. Defaults to Inf
#' @param purpose The purpose of the comparisons. Defaults to "single". Options: single, difference
#' @param decorrelation For repeated measure designs only.
#' Chooses the decorrelation method ("CM" or "LM"). Defaults to "none".
#' @param sep The separator used to separate dependent variables from within-subject factors in a wide data frame.
#' Only required for wide data frames where wsfactors are separated by a character. Can be skipped if you would like
#' to have groups of within factors in your graph.
#' @param plot Defaults to TRUE. Set to FALSE if you do not want the output to be a plot.
#' @param plot.type The type of object to plot on the graph. Can be either "bar" or "line". Defaults to "bar".
#' @param graph.params a list of ggplot2 parameters to input inside geoms (see ?geom_bar in ggplot2)
#' @param error.params a list of ggplot2 parameters to input inside geom_errobar (see ?geom_errorbar in ggplot2)
#'
#' @return NULL
#'
#' @examples
#' meanPlot(ToothGrowth, bsfactor = c("dose", "supp"), measure = "len",
#' statistic = "mean",
#' xlab = xlab("Dose"), ylab = ylab("Tooth Growth"),
#' theme = theme_bw())
#'
#' @export meanPlot


meanPlot <- function(data, ...,
                     bsfactor=NULL, wsfactor=NULL,
                     factorOrder, measure,
                     statistic = "mean",
                     errorbar = "SE", gamma,
                     adjustments = list(purpose = "single", popsize = Inf,
                                        decorrelation = "none"),
                     sep,
                     plot = TRUE, plot.type = "bar",
                     error.params = list(width = .8), graph.params = list()) {

  # TODO(Felix): Adjustments for clusters, halved/pooled SEM.
  # TODO(Felix): Add exception handling

  # Set defaults if they are missing ####

  if(is.null(adjustments$purpose)) {
    adjustments$purpose <- "single"
  }

  if(is.null(adjustments$popsize)) {
    adjustments$popsize <- Inf
  }

  if(is.null(adjustments$decorrelation)) {
    adjustments$decorrelation <- "none"
  }

  if(!missing(factorOrder)) {
    groupvars <- factorOrder
  } else {
    groupvars <- c(wsfactor, bsfactor)
  }

  ########################################

  # if ((!is.null(wsfactor) && is.null(wslevels)) || (is.null(wsfactor) && !is.null(wslevels))) {
  #   stop("ERROR: Did you forget to specify wslevels or your wsfactor?")
  # }

  wslevels <- length(grep(x = colnames(data), pattern = paste0("^", measure)))


  # Decorrelate data ----

  # We do this first if necessary
  # Temporary function to get column # of our measure
  select_col <- function(df, levels) (ncol(df) - (levels - 1))

  two_step_normalize <- function(data, levels) {
    df.x <- data[, select_col(data, levels):ncol(data)]
    df.y <- df.x - rowMeans(df.x) + mean(rowMeans(df.x))
    df.z <-
      (sqrt(ncol(df.x) / (ncol(df.x) - 1))) *
      (t(df.y) - colMeans(df.y)) + colMeans(df.y)
    df.z <- as.data.frame(t(df.z))
    return(df.z)
  }

  # We want to do this step for each group and only on columns with measures.
  # We use delim() to only select the data we need knowing the number of levels
  # in our repeated measures.
  if (adjustments$decorrelation != "none") {
    if (adjustments$decorrelation == "CM" || adjustments$decorrelation == "LM") {

      if (!is.null(bsfactor)) {
        df.wide <- split(data, data[[bsfactor]])
        print("IT IS NOT NULL")

        for (i in 1:length(df.wide)) {

          df.part <- two_step_normalize(df.wide[[i]], wslevels)
          delim   <- select_col(df.wide[[i]], wslevels)
          df.wide[[i]][, delim:ncol(df.wide[[i]])] <- df.part

        }
        # Once decorrelation is over, we merge both data frames together again.
        df.wide <- unsplit(df.wide, as.factor(data[[bsfactor]]))

      } else {
        df.wide <- data

        df.part <- two_step_normalize(df.wide, wslevels)
        delim   <- select_col(data, wslevels)
        df.wide[, delim:ncol(data)] <- df.part

      }

      df <- lsr::wideToLong(df.wide, within = wsfactor, sep = sep)

    } else {
      stop("Invalid decorrelation adjustment. Try 'none', 'CM', or 'LM'.")
    }
  } else {
    if (!is.null(wsfactor) && !missing(sep)) {
      df <- lsr::wideToLong(data, within = wsfactor, sep = sep)
    } else {
      df <- data
    }
  }



  # Start summary ----
  # Create a data frame with the necessary summary statistics

  if(missing(gamma)) {
    df.summary <- make_summary(df = df,
                               groupvars = groupvars,
                               stats = statistic,
                               width = errorbar,
                               measure = measure)
  } else {
    df.summary <- make_summary(df = df,
                               groupvars = groupvars,
                               stats = statistic,
                               width = errorbar,
                               measure = measure,
                               gamma)
  }




  if (adjustments$decorrelation == "LM") {
    df.summary$SE <- (1 / wslevels) * (df.summary$SE^2)
  }

  # Adjustments start here ----------------

  # Adjust for population size. If population size is finite the SE should be
  # adjusted to take into account the population size.

  if (is.character(adjustments$popsize)) {
    stop("ERROR: popsize should be a number")
  } else if (adjustments$popsize != Inf) {
    print("popSize Adjust")
    df.summary[grepl("error", names(df.summary))] <-
      pop.adjust(df.summary, errorbar, adjustments$popsize)
  }

  # Adjust for purpose.
  if (adjustments$purpose != "single") {
    if (adjustments$purpose == "difference") {
      print("Purpose Adjust")
      df.summary[grepl("error", names(df.summary))] <-
        purpose(df.summary, errorbar)
    } else {
      stop("Invalid purpose adjustment. Did you mean to write 'difference'?")
    }
  }

  print(df.summary)
  # End of Adjustments


  # Graphs -----------
  # For this section we make use of the ggplot package. For each group, we plot
  # TODO: We need a case for grouping for between factors


  if (plot == TRUE) {

    require(ggplot2)

    if (is.null(df.summary$error2)) {

      plot <- make_plot(data = df.summary, type = plot.type,
                        x = groupvars[1],
                        y = "statistic",
                        groups = switch(!is.na(groupvars[2]), groupvars[2], NULL),
                        ymin = "statistic - error",
                        ymax = "statistic + error",
                        error.params = error.params,
                        graph.params = graph.params,
                        ...)
      if(!is.na(groupvars[3])) {
        plot + facet_wrap(df.summary[[groupvars[3]]] ~ .)
      } else {
        plot
      }

    } else {
      plot <- make_plot(data = df.summary, type = plot.type,
                        x = groupvars[1],
                        y = "statistic",
                        groups = switch(!is.na(groupvars[2]), groupvars[2], NULL),
                        ymin = "error1",
                        ymax = "error2",
                        error.params = error.params,
                        graph.params = graph.params,
                        ...)
      if(!is.na(groupvars[3])) {
        plot + facet_wrap(df.summary[[groupvars[3]]] ~ .)
      } else {
        plot
      }

    }

  } else {
    return(df.summary)
  }


}
