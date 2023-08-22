#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param independent_pubs
#' @param 
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
comp_ind_topics <- function(independent_pubs, topic_model, db_path) {


independent_topics <-  independent_pubs %>% 
                        mutate(ind_topics = purrr::pmap(.l = list(pub_table, sup_vedidk),
                                   .f = function(first, second){
                                       calc_ind_topics(topic_model, db_path,
                                                       one_author = first, sup_vedidk = second)
                                   } ))


    }
