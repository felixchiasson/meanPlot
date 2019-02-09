#' @title Computes standard error and confidence interval of means under various designs and sampling schemes
#'
#' @description Computes standard error and confidence interval of means under various designs and sampling schemes
#'
#' @param data Data frame
#' @param betweenVar The name of your between-subject factor(s)
#' @param withinVar The name of your within-subject factor(s)
#' @param withinLevels The number of levels in your within-subject factor (for repeated measures only)
#' @param measure Your dependent variable
#' @param errorBarContent The content of your error bar. Can either be "CI" or "SE". Defaults to "SE"
#' @param popSize Size of the population under study if known. Defaults to Inf
#' @param purpose The purpose of the comparisons. Defaults to "single"
#' @param decorrelation For repeated measure designs only. Chooses the decorrelation method ("CM" or "LM"). Defaults to "none".
#' @param sep The separator used to separate dependent variables from within-subject factors in a wide data frame. Defaults to "_"
#'
#' @return NULL
#'
#' @examples meanPlot(ToothGrowth, betweenVar = "supp", withinVar = "dose", measure = "len", summaryStatistic = 'mean')
#'
#' @export meanPlot


meanPlot <- function(data=NULL, bsfactor=NULL, wsfactor=NULL, wslevels=NULL, measure, summaryStatistic = "mean", errorBarContent = "SE",
                     popSize = Inf, purpose = "single", decorrelation = "none", sep = "_") {

  # TODO(Felix): Replace summarySE for a more modular option
  # TODO(Felix): Better variable and data frame names
  # TODO(Felix): Make it possible to choose variable name for graph
  # TODO(Felix): Adjustments for clusters, halved/pooled SEM.
  # TODO(Felix): SummaryStats option? Is this necessary or should we just give summary stats anyway as output
  # TODO(Felix): For graphs - add show epsilon (for decorr.) and epsilon position in graph
  #              Show ICC, Show ICC position
  # TODO(Felix): Plot styling
  # TODO(Felix): Add exception handling


  library(ICC)
  library(ggplot2)
  library(data.table)
  library(lsr)
  library(plyr)
  library(tidyverse)

  groupvars <- c(wsfactor, bsfactor)


  # Decorrelate data ----
  # TODO: this only works on WIDE data... Convert data to long if it is wide first?
  # We do this first if necessary
  # TODO(Felix): Verify if this is valid with within-subject and between subject designs where multiple factors exist

  # Temporary function to get column # of our measure
  delim <- function(df,levels) {(ncol(df)-(levels-1))}

  # Cousineau-Morey

  twoStepNormalize <- function(data, levels) {
    df.x <- data[ ,delim(data, levels):ncol(data)]
    df.y <- df.x - rowMeans(df.x) + mean(rowMeans(df.x))
    df.z <- (sqrt(ncol(df.x)/(ncol(df.x)-1)))*(t(df.y) - colMeans(df.y)) + colMeans(df.y)
    df.z <- t(df.z) %>% as.data.frame()
    return(df.z)
  }

  # We want to do this step for each group and only on columns with measures.
  # We use delim() to only select the data we need knowing the number of levels in our repeated measures.

  if (decorrelation %in% c("CM", "LM")) {

    if (!is.null(bsfactor)) {
      df.wide <- data %>% split(data[[bsfactor]])
      print("IT IS NOT NULL")

      for (i in 1:length(df.wide)) {

        df.part <- twoStepNormalize(df.wide[[i]], wslevels)
        df.wide[[i]][, delim(df.wide[[i]],wslevels):ncol(df.wide[[i]])] <- df.part

      }
      # Once decorrelation is over, we merge both data frames together again.
      df.wide <- unsplit(df.wide, as.factor(data[[bsfactor]]))

    } else {
      df.wide <- data

      df.part <- twoStepNormalize(df.wide, wslevels)
      df.wide[ ,delim(data, wslevels):ncol(data)] <- df.part

    }

    df <- wideToLong(df.wide, within = wsfactor, sep = sep)

  } else {
    df <- data
  }



  # Start summary ----
  # Create a data frame with the necessary summary statistics

  df.summary <- make.summary(df, groupvars, summaryStatistic, errorBarContent, measure)


  if (decorrelation == "LM") {
    df.summary$se <- (1/wslevels)*(df.summary$se^2)
  }

  # TODO(Felix): Re-visit this when comes the time to do graphs. We might want to have separate summaries for each group.

  # Adjustments start here ----------------

  # All manipulations to original data frame must be done before this. This includes IcC and decorrelation.

  # Adjust for population size. If population size is finite the SE should be adjusted to take into
  # account the population size.

  if(popSize != Inf) {

    print("popSize Adjust")
    df.summary$se <- df.summary$se * sqrt(1 - df.summary$N/popSize)
    print(df.summary)

  }

  # Adjust for purpose.

  if(purpose == "diff") {

    print("Purpose Adjust")
    df.summary$se <- df.summary$se * sqrt(2)
    print(df.summary)

  }

  # End of Adjustments


  # Graphs -----------
  # For this section we make use of the ggplot package. For each group, we plot
  # TODO: We need a case for grouping for between factors

  # Using SEM

  if(errorBarContent[1] == "SE") {
    ggplot(df.summary, aes(x=4, y=mean, group=1)) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
      geom_line() +
      geom_point()
  } else {
    ggplot(df.summary, aes(x=try, y=mean, group=1)) +
      geom_errorbar(aes(ymin=ci1, ymax=ci2), width=.1) +
      geom_line() +
      geom_point()
  }


}


