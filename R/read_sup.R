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
  
  

  one_vedidk_pubids <- dplyr::tbl(con, "authors_by_pubs") %>% #colnames()
      filter(vedidk == ids_full_vector) %>% 
      dplyr::select(vedidk, id_helper, id_unique) %>% 
      distinct() %>% 
      as_tibble()
 
 # mozna by bylo fajn v tomto kroku vyselektovat vsechny nepublikacni vystupy, protoze ty jsou asi defaultne zapocitane:
 ## 
     control_id <- one_vedidk_pubids$id_unique
     one_vedidk_filtered_pubs <- dplyr::tbl(con, "riv_disc") %>%  # colnames()
     dplyr::select(id_unique, year) %>%
     filter(id_unique %in% control_id) %>%
     dplyr::collect() %>% 
     # filter(pub_type %in% c("J","B","C","D")) %>%
     left_join(one_vedidk_pubids) %>% 
     arrange(year) %>% 
     slice_head(n = 5) %>%
     as_tibble()
     
    
     control_id2 <-  one_vedidk_filtered_pubs$id_unique
 one_vedidk_coauthors <- dplyr::tbl(con, "authors_by_pubs") %>% # colnames()
      dplyr::select(id_unique, id_helper, vedidk) %>% 
      filter(id_unique %in% control_id2) %>% 
      dplyr::rename(sup_name = id_helper) %>%
      dplyr::rename(sup_vedidk = vedidk) %>%
      dplyr::collect() %>% 
      distinct() %>%
      left_join(one_vedidk_filtered_pubs, by ="id_unique") %>% 
      filter(id_helper != sup_name) %>% 
      dplyr::count(sup_name, sup_vedidk) %>%
      filter(n == max(n)) %>%
      slice(1) %>%
      dplyr::select(!n) %>% 
      as_tibble() 
 
 
 if(nrow(one_vedidk_coauthors)==0){tibble(sup_name = NA, sup_vedidk = NA)}else(one_vedidk_coauthors)
 
     # add_row()
 
 
 # pull(sup_name)
  
}
