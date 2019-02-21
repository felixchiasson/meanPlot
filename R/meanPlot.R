#' @importFrom magrittr %>%
#' @title Computes standard error and confidence interval of means under various designs and sampling schemes
#'
#' @description Computes standard error and confidence interval of means under various designs and sampling schemes
#'
#' @param data Data frame
#' @param bsfactor The name of your between-subject factor(s)
#' @param wsfactor The name of your within-subject factor(s)
#' @param wslevels The number of levels in your within-subject factor (for repeated measures only)
#' @param measure Your dependent variable
#' @param errorbar The content of your error bar. Can either be "CI" or "SE". Defaults to "SE"
#' @param popsize Size of the population under study if known. Defaults to Inf
#' @param purpose The purpose of the comparisons. Defaults to "single"
#' @param decorrelation For repeated measure designs only. Chooses the decorrelation method ("CM" or "LM"). Defaults to "none".
#' @param sep The separator used to separate dependent variables from within-subject factors in a wide data frame. Defaults to "_"
#'
#' @return NULL
#'
#' @examples meanPlot(ToothGrowth, bsfactor = "dose", wsfactor = "supp", measure = "len", statistic = "mean")
#'
#' @export meanPlot


meanPlot <- function(data, bsfactor=NULL, wsfactor=NULL,
                     wslevels=NULL, measure, statistic = "mean",
                     errorbar = "SE", popsize = Inf, purpose = "single",
                     decorrelation = "none", sep = "_") {

  # TODO(Felix): Replace summarySE for a more modular option
  # TODO(Felix): Better variable and data frame names
  # TODO(Felix): Make it possible to choose variable name for graph
  # TODO(Felix): Adjustments for clusters, halved/pooled SEM.
  # TODO(Felix): Plot styling
  # TODO(Felix): Add exception handling

  groupvars <- c(wsfactor, bsfactor)


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
    df <- data
  }



  # Start summary ----
  # Create a data frame with the necessary summary statistics

  df.summary <- make_summary(df, groupvars, statistic, errorbar, measure)


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

  if (purpose == "diff") {
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

  if (errorbar[1] == "SE") {
    ggplot2::ggplot(df.summary, ggplot2::aes_string(x = bsfactor,
                                                    y = "statistic",
                                                    fill = wsfactor)
                    ) +
      ggplot2::geom_bar(position = ggplot2::position_dodge(),
                        stat = "identity") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = statistic - SE,
                                          ymax = statistic + SE),
                             width = .2,
                             position = ggplot2::position_dodge(.9))
  } else {
    ggplot2::ggplot(df.summary, ggplot2::aes_string(x = bsfactor,
                                                    y = "statistic",
                                                    colour = wsfactor)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = CI1, ymax = CI2),
                             width = .1
                             ) +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  }


}
