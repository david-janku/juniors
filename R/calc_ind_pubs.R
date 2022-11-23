#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param one_author
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_ind_pubs <- function(one_author, sup_name) {

    #hledani jmena vedouciho
    # vedidk_researcher <- one_author %>% 
    #     pull(vedidk) %>% 
    #     unique()
    # 
    # c <- one_author %>% 
    #     pull(vedouci) %>% 
    #     unique()
    
    c <- sup_name
    
    # sup_vector <- ids_complete %>% 
    #     filter(vedidk_core_researcher == vedidk_researcher) %>% 
    #     pull(vedoucí.vedidk)
    # 
    # con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    # on.exit(DBI::dbDisconnect(con))
    # 
    # c <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
    #     filter(vedidk == sup_vector) %>% 
    #     select(id_helper) %>% 
    #     count(id_helper) %>% 
    #     filter(n == max(n)) %>% 
    #     slice_sample(n=1) %>% 
    #     pull(id_helper)
    
    all_pubs_author <- one_author %>% 
        select(id_unique) %>% 
        distinct()
    
    sup_pubs <- one_author %>% 
        filter(id_helper == c) %>% 
        select(id_unique)
    
    ind_pubs <- anti_join(all_pubs_author, sup_pubs)
       
    rpubs = nrow(ind_pubs)/nrow(all_pubs_author)
    
    # table <- tibble(vedidk = vedidk_researcher, ind_pubs = rpubs)
    
    # e <- anti_join(one_author[[1]], sup_pubs, by = "id_unique")
    
}
