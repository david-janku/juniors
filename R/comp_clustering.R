#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param comp_clustering
#' @param 
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
comp_clustering <- function(eigen_centrality) {


clustering <-  eigen_centrality %>% 
                        mutate(clustr = purrr::pmap(.l = list(graph, sup_name),
                                   .f = function(first, second){
                                       calc_clustering(graph = first, one_author = second)
                                   } ))





    }
