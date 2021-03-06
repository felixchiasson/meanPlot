% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meanPlot.R
\name{meanPlot}
\alias{meanPlot}
\title{Computes standard error and confidence intervals under various designs and sampling schemes}
\usage{
meanPlot(data, ..., bsfactor = NULL, wsfactor = NULL, factorOrder,
  measure, statistic = "mean", errorbar = "SE", gamma,
  adjustments = list(purpose = "single", popsize = Inf, decorrelation =
  "none"), sep, plot = TRUE, plot.type = "bar",
  error.params = list(width = 0.8), graph.params = list())
}
\arguments{
\item{data}{Data frame}

\item{bsfactor}{The name of your between-subject factor(s)}

\item{wsfactor}{The name of your within-subject factor(s)}

\item{factorOrder}{Order of factors as shown in the graph (x axis, groups, panels)}

\item{measure}{Your dependent variable}

\item{errorbar}{The content of your error bar. Can either be "CI" or "SE".
Defaults to "SE"}

\item{gamma}{Only necessary if errorbar == "CI". The confidence level.}

\item{adjustments}{List of adjustments as described below}

\item{sep}{The separator used to separate dependent variables from within-subject factors in a wide data frame.
Only required for wide data frames where wsfactors are separated by a character. Can be skipped if you would like
to have groups of within factors in your graph.}

\item{plot}{Defaults to TRUE. Set to FALSE if you do not want the output to be a plot.}

\item{plot.type}{The type of object to plot on the graph. Can be either "bar" or "line". Defaults to "bar".}

\item{error.params}{a list of ggplot2 parameters to input inside geom_errobar (see ?geom_errorbar in ggplot2)}

\item{graph.params}{a list of ggplot2 parameters to input inside geoms (see ?geom_bar in ggplot2)}

\item{popsize}{Size of the population under study if known. Defaults to Inf}

\item{purpose}{The purpose of the comparisons. Defaults to "single". Options: single, difference}

\item{decorrelation}{For repeated measure designs only.
Chooses the decorrelation method ("CM" or "LM"). Defaults to "none".}
}
\description{
Computes standard error and confidence interval under various designs and sampling schemes
}
\examples{
meanPlot(ToothGrowth, bsfactor = c("dose", "supp"), measure = "len",
statistic = "mean",
xlab = xlab("Dose"), ylab = ylab("Tooth Growth"),
theme = theme_bw())

}
