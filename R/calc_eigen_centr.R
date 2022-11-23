#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_eigen_centr <- function(graph, one_author, db_path) {

    ecentr <- igraph::eigen_centrality(
        graph,
        directed = FALSE,
        scale = TRUE,
        weights = igraph::E(graph)$weight,
        # options = arpack_defaults
    )
    
    centr <- tibble::enframe(ecentr$vector, name = "author", value = "eigen_ctr")
    
    # vedidk_researcher <- one_author %>% 
    #     pull(vedidk) %>% 
    #     unique()
    
    # a <- one_author %>% 
    #     pull(sup_name) %>% 
    #     unique()
    # 
    
    a <- one_author
    
   #  sup_vector <- ids_complete %>% 
   #          filter(vedidk_core_researcher == vedidk_researcher) %>% 
   #          pull(vedoucí.vedidk)
   #      
   #  con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
   #  on.exit(DBI::dbDisconnect(con))
   #  
   # a <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
   #      filter(vedidk == sup_vector) %>% 
   #      select(id_helper) %>% 
   #      count(id_helper) %>% 
   #      filter(n == max(n)) %>% 
   #      slice_sample(n=1) %>% 
   #      pull(id_helper)
    
    
    
    sup <- centr %>% 
               filter(author == a) %>% 
        pull(eigen_ctr)
    
    # table <- tibble(vedidk = vedidk_researcher, eig = sup)
    

}
