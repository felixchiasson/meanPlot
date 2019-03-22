#' @importFrom magrittr %>%
#' @title Computes standard error and confidence intervals under various designs and sampling schemes
#'
#' @description Computes standard error and confidence interval under various designs and sampling schemes
#'
#' @param data Data frame
#' @param bsfactor The name of your between-subject factor(s)
#' @param wsfactor The name of your within-subject factor(s)
#' @param x Only necessary if plot == TRUE. The name of the column containing
#' your x.
#' @param group.by Only necessary if plot == TRUE. The name of the column
#' containing your grouping variable.
#' @param wslevels The number of levels in your within-subject factor for all
#' within subject factors. For multiple levels, list them in the same order as
#' your wsfactor.
#' @param measure Your dependent variable
#' @param errorbar The content of your error bar. Can either be "CI" or "SE".
#' Defaults to "SE"
#' @param gamma Only necessary if errorbar == "CI". The confidence level.
#' @param popsize Size of the population under study if known. Defaults to Inf
#' @param purpose The purpose of the comparisons. Defaults to "single"
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
#' meanPlot(ToothGrowth, bsfactor = "dose", wsfactor = "supp", measure = "len",
#' x = "dose", group.by = "supp", statistic = "mean",
#' xlab = xlab("Dose"), ylab = ylab("Tooth Growth"),
#' theme = theme_bw())
#'
#' @export meanPlot


meanPlot <- function(data, ...,
                     bsfactor=NULL, wsfactor=NULL,
                     x, group.by, wslevels = NULL,
                     measure, statistic = "mean",
                     errorbar = "SE", gamma,
                     popsize = Inf, purpose = "single",
                     decorrelation = "none",
                     sep,
                     plot = TRUE, plot.type = "bar",
                     error.params = list(width = .8), graph.params = list()) {

  # TODO(Felix): Adjustments for clusters, halved/pooled SEM.
  # TODO(Felix): Add exception handling

  groupvars <- c(wsfactor, bsfactor)

  # if ((!is.null(wsfactor) && is.null(wslevels)) || (is.null(wsfactor) && !is.null(wslevels))) {
  #   stop("ERROR: Did you forget to specify wslevels or your wsfactor?")
  # }

  wslevels <- sum(wslevels)


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
    df.z <- t(df.z) %>% as.data.frame()
    return(df.z)
  }

  # We want to do this step for each group and only on columns with measures.
  # We use delim() to only select the data we need knowing the number of levels
  # in our repeated measures.

  if (decorrelation %in% c("CM", "LM")) {

    if (!is.null(bsfactor)) {
      df.wide <- data %>% split(data[[bsfactor]])
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




  if (decorrelation == "LM") {
    df.summary$SE <- (1 / wslevels) * (df.summary$SE^2)
  }

  # Adjustments start here ----------------

  # Adjust for population size. If population size is finite the SE should be
  # adjusted to take into account the population size.


  if (popsize != Inf) {
    print("popSize Adjust")
    df.summary[grepl(errorbar[1], names(df.summary))] <-
      pop.adjust(df.summary, errorbar[1], popsize)
  }

  # Adjust for purpose.

  if (purpose == "diff" || decorrelation == "none") {
    print("Purpose Adjust")
    df.summary[grepl(errorbar[1], names(df.summary))] <-
      purpose(df.summary, errorbar[1])
  }

  print(df.summary)

  # End of Adjustments


  # Graphs -----------
  # For this section we make use of the ggplot package. For each group, we plot
  # TODO: We need a case for grouping for between factors

  # Using SEM

  if (plot == TRUE) {

    require(ggplot2)

    if (missing(x)) {
      stop("ERROR: Argument x missing. Cannot continue.")
    }

    if (errorbar == "SE") {

      make_plot(data = df.summary, type = plot.type,
                x = x,
                y = "statistic",
                groups = switch(!missing(group.by), group.by, NULL),
                ymin = "statistic - SE",
                ymax = "statistic + SE",
                error.params = error.params,
                graph.params = graph.params,
                ...)

    } else if (errorbar == "CI") {
      make_plot(data = df.summary, type = plot.type,
                x = x,
                y = "statistic",
                groups = switch(!missing(group.by), group.by, NULL),
                ymin = "CI1",
                ymax = "CI2",
                error.params = error.params,
                graph.params = graph.params,
                ...)
    } else {
      stop("ERROR: errorbar must be 'CI' or 'SE'. Stopping.")
    }
  }


}
