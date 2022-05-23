#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param graph
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_clustering <- function(graph, ids_complete, one_author, db_path) {

   
        
     vedidk_researcher <- one_author %>% 
         pull(vedidk) %>% 
         unique()
     
     b <- one_author %>% 
         pull(vedouci) %>% 
         unique()
     
    # sup_vector <- ids_complete %>% 
    #     filter(vedidk_core_researcher == vedidk_researcher) %>% 
    #     pull(vedoucí.vedidk)
    # 
    # con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    # on.exit(DBI::dbDisconnect(con))
    # 
    # b <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
    #     filter(vedidk == sup_vector) %>% 
    #     select(id_helper) %>% 
    #     count(id_helper) %>% 
    #     filter(n == max(n)) %>% 
    #     slice_sample(n=1) %>% 
    #     pull(id_helper)
    
    clustering <-
        transitivity(
            graph,
            type = c("local"),
            vids = which(V(graph)$name == b),
            weights = E(graph)$weight,
            isolates = c("NaN", "zero")
        )
    
    table <- tibble(vedidk = vedidk_researcher, clustr = clustering)
    
    

}
