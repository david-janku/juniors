#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param clustr
#' @param 
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
comp_ind_pubs <- function(cluster) {


independent_pubs <-  cluster %>% 
                        mutate(ind_pubs = purrr::pmap(.l = list(pub_table, sup_name),
                                   .f = function(first, second){
                                       calc_ind_pubs(one_author = first, sup_name = second)
                                   } ))





    }
