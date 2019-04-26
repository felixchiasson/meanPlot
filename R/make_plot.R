#' @import ggplot2

make_plot <- function(data, type, x, y, ymin, ymax, 
    groups = NULL, 
    error.params = list(),
    graph.params = list(), 
    ...
) {
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

