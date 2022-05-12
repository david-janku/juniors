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
calc_final_data <- function(eigen_centr, clustering = NULL, ind_topics = NULL, ind_pubs = NULL) {
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
      

    bind_rows(eigen_centr)
    
    # left_join(eigen_centr, eigen_centr, by = "vedidk")

}
