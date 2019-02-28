#' @import ggplot2
make_plot <- function(data, type, x, y, ymin, ymax, groups = NULL, error.params = list(),
                      graph.params = list(), ...) {

  build_graph_options(
    ggplot(data, aes_string(x, y, ymin = ymin, ymax = ymax, fill = groups)) +
    build_bar_graph(graph.params, error.params, type = type),
    ...)

}

build_bar_graph <- function(graph.params = list(), error.params = list(), type) {



  if (type == "bar") {
    plot.type <- do.call(geom_bar, modifyList(
      list(position = position_dodge(),
           stat = "identity"),
      graph.params)
    )
  } else {
    plot.type <- do.call(geom_point, modifyList(
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

build_graph_options <- function(...) {
  Reduce(`+`, list(...))
}
