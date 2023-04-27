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
read_one_author <- function(db_path, ids_complete_vector, ids_complete, matching, ids_full_vector, intervention_year, timing) {

  con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbListTables(con)
  

  one_vedidk_pubids <- dplyr::tbl(con, "authors_by_pubs") %>% #colnames()
      filter(vedidk == ids_full_vector) %>% 
      dplyr::select(vedidk, id_unique) %>% 
      distinct() %>% 
      as_tibble()
  
 # matching <- matching %>%    # tohle asi bude dělat problémy když se tam budou opakovat vedidky, a oni se opakují -> nevím jak to vyřešit - ledaže bych měl nějaké pořadí toho vektoru a pracoval s tím pořadím
 #     filter(vedidk == ids_full_vector)
 # 
 #  saving just for case that something goes bad and I will need to return to this / this is the last version that worked
 # one_vedidk_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
 #      filter(vedidk == ids_complete_vector) %>% 
 #      select(vedidk, id_unique) %>% 
 #      distinct() %>% 
 #      as_tibble()
 
 # mozna by bylo fajn v tomto kroku vyselektovat vsechny nepublikacni vystupy, protoze ty jsou asi defaultne zapocitane:
 
     control_vedidk <- one_vedidk_pubids$id_unique
     one_vedidk_filtered_pubs <- dplyr::tbl(con, "riv_disc") %>%  # colnames()
     dplyr::select(id_unique, year) %>%
     filter(id_unique %in% control_vedidk) %>% 
     dplyr::collect() %>% 
     # filter(pub_type %in% c("J","B","C","D")) %>%
     left_join(one_vedidk_pubids) %>% 
     inner_join(matching %>% dplyr::select(vedidk, treatment_year, independence_timing)) %>%  #jako další věc do select přidat sup_vedidk - to bude sloupec ve kterém bude vedidk supervisora daného člověka 
     filter(treatment_year == intervention_year, independence_timing == timing) %>% 
     # purrr::when(.$independence_timing == "before_intervention" ~ filter(., year <= treatment_year), ~ filter(., year >= treatment_year)) %>% 
     # purrr::when(matching$independence_timing == "before_intervention" ~ filter(., year <= matching$treatment_year), ~ filter(., year >= matching$treatment_year)) %>% 
     as_tibble()
     
     one_vedidk_filtered_pubs <- if(unique(one_vedidk_filtered_pubs$independence_timing) == "before_intervention"){filter(one_vedidk_filtered_pubs, year <= intervention_year)}else{filter(one_vedidk_filtered_pubs, year >= intervention_year)}
     
     
     
#tabulka nahoře obsahuje obor každé z publikací daného autora, a proto by šlo z ní vypočítat majoritní disciplínu daného autora. K tomu by bylo potřeba:
#     1) přeložit disciplíny z roku dřívějšího než 2018 do klasifikace ford a 2) udělat nějaký algoritmus který by to agregoval a vytáhl z toho disciplinaritu daného autora - například vzít modus disciplín
#     3)  tohle by se pak dalo vytisknout do proměnné "discipline" (podobně jako je proměnná sup_name) a tu pak přidat do tabulky níže tí že odstraníme #před mutate)
 
    # sup_vector <- ids_complete %>%
    #     filter(vedidk_core_researcher == ids_complete_vector) %>%
    #     pull(vedoucí.vedidk)
    # 
    # sup_name <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
    #     filter(vedidk == unique(one_vedidk_filtered_pubs$sup_vedidk) %>% 
    #     select(id_helper) %>%
    #     count(id_helper) %>%
    #     filter(n == max(n)) %>%
    #     slice_sample(n=1) %>%
    #     pull(id_helper)
 
     control_vedidk2 <- one_vedidk_filtered_pubs$id_unique
  one_vedidk_coauthors <- dplyr::tbl(con, "authors_by_pubs") %>% # colnames()
      dplyr::select(id_unique, id_helper) %>% 
      filter(id_unique %in% control_vedidk2) %>% 
      distinct() %>% dplyr::collect() %>% 
      left_join(one_vedidk_filtered_pubs, by ="id_unique") %>% 
      mutate(vedidk = one_vedidk_filtered_pubs$vedidk[1]) %>% 
      # mutate(vedouci = sup_name) %>% 
      #mutate(discipline = discipline)
      as_tibble()
  
  if(nrow(one_vedidk_coauthors)==0){tibble(id_unique = NA, id_helper = NA, year = NA, vedidk = NA, treatment_year = NA, independence_timing = NA)}else(one_vedidk_coauthors)
  
  #####
  
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
