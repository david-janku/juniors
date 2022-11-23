#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param coauthorship
#' @param 
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_graph <- function(coauthorship) {


coauthorship_graph <-  coauthorship %>% 
            mutate(graph = purrr::map(coauthorship$edgelist, 
                                 function(first){igraph::graph_from_data_frame(first, directed = FALSE) }))
    

    }
