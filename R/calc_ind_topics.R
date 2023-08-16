#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topic_model
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_ind_topics <- function(topic_model, db_path, one_author, sup_vedidk) {

    
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    
    # topic_model <- read.csv2(here::here("data", "derived", "tmm.csv"))
    
    # topic_model_2 <- load(paste(here::here("data", c("derivedtopic_dist.Rdata"))))
    
    #for debugging: this is easy method how to call nested table:
    # one_author <- as_tibble(do.call(cbind, independent_pubs$pub_table[4]))
    
    
    # names(topic_model)=substring(names(topic_model),2)
    
    
    #spočítat poměrné zastoupení každého tématu v celém vzorku
     topic_model$topic_means <- rowMeans(topic_model)
    
    #spočítat poměrné zastoupení každého tématu ve vzorku publikací daného výzkumníka
    all_pubs_author <- one_author %>% 
        select(id_unique) %>% 
        distinct()
    
    all_pubs_author$id_unique <- as.character(all_pubs_author$id_unique)
    
    author_pubs_topics <- as_tibble(cbind(id_unique = names(topic_model), t(topic_model))) 
       
    author_pubs_tp <- semi_join(author_pubs_topics, all_pubs_author, by = "id_unique") 

    author_pubs_tp <-  as_tibble(author_pubs_tp) %>% 
        tibble::column_to_rownames("id_unique")
    
    author_pubs_tp[] <- lapply(author_pubs_tp, as.numeric)    
     
    author_pubs_tp <- author_pubs_tp  %>% 
        t() %>% 
        as_tibble()
    
    author_pubs_tp$author_means <- rowMeans(author_pubs_tp)
    
    #spočítat poměrné zastoupení každého tématu ve vzorku publikací vedouciho daneho vyzkumnika
   
        # vedidk_researcher <- one_author %>% 
        #     pull(vedidk) %>% 
        #     unique()
        # 
        # sup_vector <- ids_complete %>% 
        #     filter(vedidk_core_researcher == vedidk_researcher) %>% 
        #     pull(vedoucí.vedidk)
            
    sup_vedidk <- sup_vedidk
    
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    sup_pubids <- dplyr::tbl(con, "authors_by_pubs") %>% #colnames()
        filter(vedidk == sup_vedidk) %>% 
        select(id_unique) %>% 
        distinct() %>% 
        dplyr::collect() %>% 
        as_tibble()
    
    sup_pubids$id_unique <- as.character(sup_pubids$id_unique)
    
    sup_pubs_tp <- semi_join(author_pubs_topics, sup_pubids, by = "id_unique") 
    
    sup_pubs_tp <-  as_tibble(sup_pubs_tp) %>% 
        tibble::column_to_rownames("id_unique")
    
    sup_pubs_tp[] <- lapply(sup_pubs_tp, as.numeric)    
    
    sup_pubs_tp <- sup_pubs_tp  %>% 
        t() %>% 
        as_tibble()
    
    sup_pubs_tp$sup_means <- rowMeans(sup_pubs_tp)
    
    
    #spočítat v kolika tématech je činný daný výzkumník a ne jeho vedouci
    
    topic_model$author_topic <- ifelse(author_pubs_tp$author_means>topic_model$topic_means, 1, 0)
    
    topic_model$sup_topic <- ifelse(sup_pubs_tp$sup_means>topic_model$topic_means, 1, 0)
    
    topic_model$sup_topic <- tidyr::replace_na(topic_model$sup_topic, 0)
    topic_model$author_topic <- tidyr::replace_na(topic_model$author_topic, 0)
    
    topic_independence_categories = sum(ifelse(topic_model$author_topic>topic_model$sup_topic, 1, 0))/(sum(topic_model$author_topic))
    
    # table <- tibble(vedidk = vedidk_researcher, ind_topics = topic_independence_categories)
    
    
    # #alternativni zpusob vypoctu
    # author_pubs_tp$author_topic <- ifelse("each value in a given row">0.1, 1, 0)
    # sup_pubs_tp$sup_topic <- ifelse("each value in a given row">0.1, 1, 0)
    # 
    # topic_independence_categories = sum(ifelse(author_pubs_tp$author_topic>sup_pubs_tp$sup_topic, 1, 0))/(sum(author_pubs_tp$author_topic))
    # 
    # stary nepouzity kod:
   #  all_pubs <- as_tibble(t(topic_model)) %>% #in this step, the identificators of pubs gets deleted and substituted with numbers
   #      rownames_to_column("pub_id") %>% 
   #      mutate(pub_id = as.integer(pub_id)) %>% 
   #      left_join(two_authors %>% 
   #                    select(pub_id = ID_core_pubs, list)) #this should be matched with "Kód.výstupu" not ID_core_ 
   #  
   #  
   #  all_pubs <- mutate(all_pubs, str_detect(all_pubs$list, "Binter"))
   #  
   #  all_pubs <- mutate(all_pubs, str_detect(all_pubs$list, "Klapilová"))
   #  
   # # colnames(all_pubs) <- paste0("topic_", colnames(topics))
   #  
    #tahle funkce říká: pokud v daném tématu nemá sledovaný výzkumník ani jednu publikaci s loadingem  více než 0,1, pak mi vytiskni "0", pokud má alespoň jednu, ale zároveň i jeho supervisors má alsepoň jednu, tak mi vytiskni "2", a pokud má alespoň jednu ale supervisor nemá žádnou, tak vytiskni "1". Takhle funkce bohužel není škálovatelná, takže se bude muset přepsat.
    # topics$topic_one <- if(sum(ifelse(author_pubs_tp[1,]>0.1, 1, 0))<1){print("0")
    # } else if(sum(ifelse(sup_pubs_tp[1,]>0.1, 1, 0))>1){print("2")
    # } else {
    #     print("1")
    # }
    # 
    # topics$topic_two <- if(sum(ifelse(data[2,1:20]>0.1, 1, 0))<1){print("0")
    # } else if(sum(ifelse(data[2,21:77]>0.1, 1, 0))>1){print("2")
    # } else {
    #     print("1")
    # }
    # 
    # topics$topic_three <- if(sum(ifelse(data[3,1:20]>0.1, 1, 0))<1){print("0")
    # } else if(sum(ifelse(data[3,21:77]>0.1, 1, 0))>1){print("2")
    # } else {
    #     print("1")
    # }
    # 
    # topics$topic_four<- if(sum(ifelse(data[4,1:20]>0.1, 1, 0))<1){print("0")
    # } else if(sum(ifelse(data[4,21:77]>0.1, 1, 0))>1){print("2")
    # } else {
    #     print("1")
    # }
    # 
    # topics$topic_five <- if(sum(ifelse(data[5,1:20]>0.1, 1, 0))<1){print("0")
    # } else if(sum(ifelse(data[5,21:77]>0.1, 1, 0))>1){print("2")
    # } else {
    #     print("1")
    # }
    # 
    # topics <- rm(topics$topics)
    # 
    # topic_independence_categories = sum(ifelse(topics==1, 1, 0))/(sum(ifelse(topics==2, 1, 0)))+(sum(ifelse(topics==1, 1, 0)))

}
