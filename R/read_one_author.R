#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dv_path
#' @param dedidk
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
read_one_author <- function(db_path, ids_complete_vector) {

  con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbListTables(con)
  

 
 one_vedidk_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
      filter(vedidk == ids_complete_vector) %>% 
      select(vedidk, id_unique) %>% 
      distinct() %>% 
      as_tibble()
  
  one_vedidk_coauthors <- DBI::dbReadTable(con, "authors_by_pubs") %>% # colnames()
      select(id_unique, id_helper) %>% 
      filter(id_unique %in% one_vedidk_pubids$id_unique) %>% 
      distinct() %>%
      mutate(vedidk = ids_complete_vector) %>% 
      as_tibble()
  
  ####
  
  # sup_vector <- ids_complete$vedoucí.vedidk %>% 
  #     as_tibble() %>% 
  #     as_vector() 
  # 
  # a <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
  #     filter(vedidk %in% sup_vector[1]) %>% 
  #     select(id_helper) %>% 
  #     count(id_helper) %>% 
  #     filter(n == max(n)) %>% 
  #     slice_sample(n=1) %>% 
  #     pull(id_helper)
      
  
      
  #%>%
      # filter(row_number() == 1)
      
  
     
  
  # comb_vedidk_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>%
  #     filter(vedidk == .env$vedidk) %>%
  #     select(vedidk, id_unique) %>%
  #     distinct() %>%
  #     as_tibble()
  
  
  # comb_pubids_kod <- DBI::dbReadTable(con, "riv_unique_ids") %>% 
  #     filter(id_unique %in% comb_vedidk_pubids$id_unique) %>% 
  #     distinct() %>% 
  #     as_tibble()
  # 
  # text_pubs <- DBI::dbReadTable(con, "riv_text") %>%  # colnames()
  #     select(id_unique, title_eng, abstract_eng, keywords) %>% 
  #     filter(id_unique %in% comb_vedidk_pubids$id_unique) %>% 
  #     filter(!duplicated(id_unique)) %>% 
  #     as_tibble() 
      
  #interestingly, we lost 33061-27607=5454 (16,5 %) publications in this step, not sure why. Also, many of the remianing publications seem to have missing abstracts or keywords

  # sum(is.na(text_pubs$keywords))
  # sum(nchar(text_pubs$abstract_eng, type = "chars", allowNA = FALSE, keepNA = NA)<25)   #this way we can calculate number of missing values there (complicated because more symbols are used: Annotation not available, N/A, XXX, xxx )          
  # na_if(text_pubs$abstract_eng, nchar(text_pubs$abstract_eng, type = "chars", allowNA = FALSE, keepNA = NA)<25) #tohle z nejakeho duvodu nefunguje a ja nevim proc         
  # sum(is.na(text_pubs$abstract_eng))
 
  # text_pubs$abstract_eng %>% 
  #     na_if(25 > nchar(text_pubs$abstract_eng, type = "chars", allowNA = FALSE, keepNA = NA))    
  
  # mutate(na_if(text_pubs$abstract_eng, 25 > nchar(text_pubs$abstract_eng, type = "chars", allowNA = FALSE, keepNA = NA))) %>% 
      
   
 # text_pubs %>% 
 #      left_join(text_pubs, by = "id_unique")  #what is this code doing here?
 
# below is not relevant for the topic modelling, but only for the edgelist
 
# coauth <- DBI::dbReadTable(con, "authors_by_pubs") %>%  #colnames()
#      select(id_helper, id_unique, name_last, name_first, vedidk) %>% 
#      filter(id_unique %in% comb_vedidk_pubids$id_unique) %>%
#     group_by(id_unique) %>% 
#      filter(!duplicated(id_helper)) %>%
#     ungroup() %>% 
#      as_tibble()
  
}
