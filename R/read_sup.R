#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param db_path
#' @param vedidk
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
read_sup <- function(db_path, ids_full_vector) {

  con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbListTables(con)
  

  one_vedidk_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
      filter(vedidk == ids_full_vector) %>% 
      dplyr::select(vedidk, id_helper, id_unique) %>% 
      distinct() %>% 
      as_tibble()
 
 # mozna by bylo fajn v tomto kroku vyselektovat vsechny nepublikacni vystupy, protoze ty jsou asi defaultne zapocitane:
 ## 
     one_vedidk_filtered_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  # colnames()
     dplyr::select(id_unique, year) %>%
     filter(id_unique %in% one_vedidk_pubids$id_unique) %>%
     # filter(pub_type %in% c("J","B","C","D")) %>%
     left_join(one_vedidk_pubids) %>% 
     arrange(year) %>% 
     slice_head(n = 5) %>%
     as_tibble()
     
     
 one_vedidk_coauthors <- DBI::dbReadTable(con, "authors_by_pubs") %>% # colnames()
      dplyr::select(id_unique, id_helper, vedidk) %>% 
      filter(id_unique %in% one_vedidk_filtered_pubs$id_unique) %>% 
      rename(sup_name = id_helper) %>%
      rename(sup_vedidk = vedidk) %>%
      distinct() %>%
      left_join(one_vedidk_filtered_pubs, by ="id_unique") %>% 
      filter(id_helper != sup_name) %>% 
      dplyr::count(sup_name, sup_vedidk) %>%
      filter(n == max(n)) %>%
      slice(1) %>%
      dplyr::select(!n) %>% 
      as_tibble() 
 
 
      
     # add_row()
 
 one_vedidk_coauthors
 
 # pull(sup_name)
  
}
