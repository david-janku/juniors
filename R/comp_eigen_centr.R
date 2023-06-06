#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param coauthorship_graph
#' @param 
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
comp_eigen_centr <- function(coauthorship_graph) {


eigen_centrality <-  coauthorship_graph %>% 
                        mutate(eig_centr = purrr::pmap(.l = list(graph, sup_name),
                                   .f = function(first, second){
                                       calc_eigen_centr(graph = first, one_author = second)
                                   } ))

# eigen_centrality <- eigen_centrality %>% 
# mutate_all(~ na_if(., "numeric(0)"))

# replace(independent_topics$eig_centr, NA, 0)


    }
