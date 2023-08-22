#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param db_path
#' @param ids
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
get_random_pubs <- function(db_path) {

   
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    
    comb_vedidk_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
        select(id_unique) %>% 
        distinct() %>% 
        sample_n(1200, replace = FALSE) %>% 
        as_tibble()
    
    # comb_vedidk_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>%
    #     filter(vedidk == .env$vedidk) %>%
    #     select(vedidk, id_unique) %>%
    #     distinct() %>%
    #     as_tibble()
    
    
    # comb_pubids_kod <- DBI::dbReadTable(con, "riv_unique_ids") %>% #colnames()
    #     filter(id_unique %in% comb_vedidk_pubids$id_unique) %>% 
    #     distinct() %>% 
    #     as_tibble()
    
    filtered_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
        select(id_unique, pub_type) %>% 
        filter(id_unique %in% comb_vedidk_pubids$id_unique) %>% 
        filter(pub_type %in% c("J","B","C","D")) %>% 
        as_tibble() 
  
    # this filtered out all non-publication outputs, n=4676
    
    text_pubs <- DBI::dbReadTable(con, "riv_text") %>% #colnames()
        select(id_unique, title_eng, abstract_eng, keywords) %>% 
        filter(id_unique %in% filtered_pubs$id_unique) %>% 
        filter(!duplicated(id_unique)) %>% 
        mutate(abstract_eng = ifelse(nchar(abstract_eng, type = "chars", allowNA = FALSE, keepNA = NA)<27, NA, abstract_eng)) %>% 
        filter(!is.na(abstract_eng)) %>% 
        as_tibble() 
    
    ## we decided to only include papers with full abstracts = 27134 pubs 
    
    #interestingly, we lost 778 (2.7 %) publications in this step, not sure why. Also, many of the remaining publications seem to have missing abstracts or keywords. Analysis of the 778 missing cases below:
    
    # disc_pubs_missing <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
    #     filter(id_unique %in% comb_vedidk_pubids$id_unique) %>% 
    #     filter(pub_type %in% c("J","B","C","D")) %>% 
    #     filter(!id_unique %in% text_pubs$id_unique) %>% 
    #     as_tibble() 
    # 
    # text_pubs_missing <- DBI::dbReadTable(con, "riv_text") %>% #colnames()
    #     select(id_unique, title_eng, abstract_eng, keywords) %>% 
    #     filter(id_unique %in% disc_pubs_missing$id_unique) %>% 
    #     filter(!duplicated(id_unique)) %>% 
    #     as_tibble() 
    # 
    # kod_pubs_missing <- DBI::dbReadTable(con, "riv_unique_ids") %>% #colnames()
    #          filter(id_unique %in% disc_pubs_missing$id_unique) %>% 
    #          distinct() %>% 
    #          as_tibble()
    # 
    # year_pubs_missing <- DBI::dbReadTable(con, "riv_details") %>% #colnames()
    #     filter(kod %in% kod_pubs_missing$kod) %>% 
    #     distinct() %>% 
    #     as_tibble()
    # 
    # wos_pubs_missing <- DBI::dbReadTable(con, "riv_wos_scopus") %>% #colnames()
    #     filter(id_unique %in% disc_pubs_missing$id_unique) %>% 
    #     distinct() %>% 
    #     as_tibble()
    # 
    # authors_pubs_missing <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
    #     filter(id_unique %in% disc_pubs_missing$id_unique) %>% 
    #     distinct() %>% 
    #     as_tibble()
    
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
