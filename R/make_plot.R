#' @import ggplot2
make_plot <- function(data, x, y, groups = NULL, error.params = list(),
                      graph.params = list(), ...) {

  build_graph_options(
    ggplot(data, aes_string(x, y, colour = groups, ymin = paste(y, "-SE"), ymax = paste(y,"+SE"))) +
    build_bar_graph(error.params, graph.params),
    list(...))

}

build_bar_graph <- function(..., graph.params = list(), error.params = list()) {


  bar <- do.call(geom_bar, modifyList(
    list(position = position_dodge(),
         stat = "identity"),
    graph.params)
  )

  errorbar <- do.call(geom_errorbar, modifyList(
    list(position = position_dodge(.9)),
    error.params)
  )

  list(bar, errorbar)

}

build_graph_options <- function(...) {
  Reduce(`+`, list(...), accumulate = TRUE)
}
