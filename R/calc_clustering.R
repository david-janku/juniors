#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param graph
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_clustering <- function(graph) {

    clustering <-
        transitivity(
            graph,
            type = c("local"),
            vids = which(V(graph)$name == "klapilovak"),
            weights = E(graph)$weight,
            isolates = c("NaN", "zero")
        )
    

}
