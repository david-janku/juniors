#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param eigen_centr
#' @param clustering
#' @param ind_topics
#' @param ind_pubs
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_final_data <- function(eigen_centr, clustering, ind_topics, ind_pubs) {
# 
#     vedidk_researcher <- one_author %>% 
#         map("vedidk") %>% 
#         unlist() %>% 
#         unique() 
    
    
  # final_data <- tibble(vedidk = vedidk_researcher, 
  #                      eigen_centr = unlist(eigen_centr), 
  #                      # clustering = unlist(clustering),
  #                      # ind_pubs = unlist(ind_pubs),
  #                      # ind_topics = unlist(ind_topics)
  #                      ) 
      

    a <- left_join(bind_rows(eigen_centr), bind_rows(clustering), by = "vedidk")
    b <- left_join(bind_rows(a), bind_rows(ind_pubs), by = "vedidk")        
    c <- left_join(bind_rows(b), bind_rows(ind_topics), by = "vedidk")    
    
    c$RII <- ((1-c$eig)+c$clustr+c$ind_pubs+c$ind_topics*2)/4
    
    c
    
}
