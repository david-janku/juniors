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
just_test <- function(db_path) {

   # aa <- one_author[[4]] %>% group_by(id_unique) %>% 
   #      right_join(one_author[[4]], by = "id_unique") %>% 
   #      filter(id_helper.x != id_helper.y) %>% 
   #      mutate(from = pmin(id_helper.x, id_helper.y),
   #             to = pmax(id_helper.x, id_helper.y)) %>% 
   #      select(pub_id = id_unique, from, to) %>% 
   #      distinct() %>% 
   #      # ungroup() %>% 
   #      
   #      # mutate(from = tolower(stringi::stri_trans_general(from, "latin-ascii")),
   #      #        to = tolower(stringi::stri_trans_general(to, "latin-ascii"))) %>% 
   #      # mutate(from = str_trim(str_replace(from, "(.*), (.?){1}.*", "\\1\\2")),
   #      #        to = str_trim(str_replace(to, "(.*), (.?){1}.*", "\\1\\2"))) %>% 
   #      
   #      relocate(pub_id, .after = last_col()) %>% 
   #      group_by(from, to) %>%        #tady mám uz podruhe group_by a to prvni neni dokoncene - nevim jestli to vadi
   #      count(name = "weight") %>% 
   #      ungroup()
   # 
   # ab <- aa %>% 
   #     filter(to == "rmarada")
    # 
    # tar_read(one_author_5ae9b0c0)
    # tar_read(edgelist_9de09176)
    
    
    ##plotting 
    
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    DBI::dbListTables(con)
    
    ##plotting number of publications of all core researchers
    
    ids <- read.csv2(here::here("data", "raw", "supervisors.csv"))
    
     ids_complete <- as_tibble(ids) %>% 
             select(vedidk_core_researcher, vedoucí.vedidk, grant_start_year) %>% 
             filter(!is.na(vedoucí.vedidk)) %>% 
             filter(!is.na(vedidk_core_researcher)) 
     
     all_researchers_vector <- ids_complete$vedidk_core_researcher %>% 
         as_tibble() %>% 
         distinct() %>% #this was added - I should check that it does change any further results 
         as_vector() 
     
     all_researchers_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
         select(vedidk, id_unique) %>% 
         filter(vedidk %in% all_researchers_vector) %>% 
         distinct() %>% 
         as_tibble()
     
     ## z nejakeho duvodu ten postup nize nefunguje, vzdy skoncim se stejnym poctem publikaci jako na zacatku (7770) 
     # all_researchers_filtered_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
     #     select(id_unique, pub_type) %>% 
     #     filter(id_unique %in% all_researchers_pubids$id_unique) %>% 
     #     filter(pub_type %in% c("J","B","C","D")) %>% 
     #     as_tibble() 
     # 
     # all_researchers_pubids_final <- DBI::dbReadTable(con, "authors_by_pubs") %>%
     #     select(id_unique, vedidk) %>%
     #     filter(id_unique %in%  all_researchers_filtered_pubs$id_unique) %>%
     #     distinct() %>%
     #     filter(vedidk %in% all_researchers_vector) %>% 
     #     distinct() %>% 
     #     as_tibble()
         
     all_researchers_pubs <- all_researchers_pubids %>% 
         group_by(vedidk) %>% 
         count(name = "weight") %>% 
         ungroup() #this will generate only 239 vedidks, not 248 entered in the all_researchers_vector, which is 4 % --> explained by duplicates in vedidks (people who received 2 grants)
     
     
     #important things that I need to work on further
     
     ids_complete$vedidk_core_researcher <- as.character(ids_complete$vedidk_core_researcher)
     
     missing_researchers <- anti_join(ids_complete, all_researchers_pubs, by = c("vedidk_core_researcher" = "vedidk"))
     
     ids_complete_duplicated <- ids_complete %>% 
         filter(duplicated(vedidk_core_researcher))  #I should filter out the double vedidks at the beginning (i.e. from the ids_complete dataset) and only include the earlier one (i.e. the date of the first grant, not the second)
     
     
     
     all_researchers_pubs <- all_researchers_pubs %>% 
         filter(vedidk != 4427920)    #one vedidk had over 800 publications, which seemed unrealistic, so I deleted it
     
     sum(all_researchers_pubs$weight<11) #4 autori maji mene nez 5 publikaci a 29 autoru ma mene nez 10 --> nevim jestli je to pro nas problem? jestli bychom meli dat nejaky cut-off?
     
     median(all_researchers_pubs$weight) #průměr je 28.9 publikace na autora, median je 22,5 publikace na autora
     
     graph_all_researchers <- ggplot(data=all_researchers_pubs, aes(x = reorder(vedidk, -weight), y=weight)) +
         geom_col() + ggtitle("Number of publications per research (whole career)") +
         xlab("Researchers (vedidk)") + ylab("Number of publications")
     
     graph_all_researchers
     
     
     ##plotting number of publications per researcher prior to receiving grant 
     
     before_grant_researchers_pubids <- DBI::dbReadTable(con, "riv_disc") %>% 
         select(id_unique, year) %>% 
         filter(id_unique %in% all_researchers_pubids$id_unique) %>% 
         distinct() %>% 
         as_tibble()
     
     bg <- left_join(before_grant_researchers_pubids, all_researchers_pubids, by = "id_unique") %>% 
         distinct()  #když přidám  filter(duplicated(id_unique)) tam mi to ukaze tabulku, kde ale zadne duplikovane id nejsou, takze nevim jestli je problem v te funkci nebo v cem
        
         
     
    ids_complete$vedidk_core_researcher <- as.character(ids_complete$vedidk_core_researcher)
         
     bg <- left_join(bg, ids_complete, by = c("vedidk" = "vedidk_core_researcher")) #v tomhle kroku z nejakeho duvodu pribzde asi 170 radku
     
                   
     bg <- bg %>% 
         # select(-c("vedoucí.vedidk")) %>% 
         distinct() %>% 
         filter(!duplicated(id_unique))
     
     bg$grant_start_year <- substring(bg$grant_start_year, 7) 
     
     pubs_before_grant_complete <- bg %>% 
         filter(grant_start_year>=year) #this has reduced the number of rows (which should be equal to unique ids) from 7700(/7585 not duplicated) to 6295, suggesting that only 1290 (17 %) publikaci vzniklo po zahajeni danych projektu. 83% tedy vzniklo pred zahajenim projektu
     
     #more sensible would be to only count publications that were published in the secord (or even third) year of the grant and beyond (e.g. if the grant strat year was 2015, we should only count publications published in year >= 2016)
     pubs_before_grant_extended <- bg %>% 
         filter(grant_start_year>=year) #funkce filter nedovoluje davat numebricke funknce, musim to zvresit nejak jinak
     
     #tohle je vzorek ktery budeme porovnavat -> ted se zda maly (pouze 17%), ale jakmile Radim uploaduje databazi, pribudou tam publikaci za dalsi 3 posledni roky a rekl bych ze to vyrazne vzroste klidne az ke 40 % -> zatim to tedy pocitat nebudu a do grafu davat taky ne, protozeje to zavadejici
     pubs_after_grant <- bg %>% 
         filter(grant_start_year<year)
     
     
     pubs_before_grant <- pubs_before_grant_complete %>% 
         group_by(vedidk) %>% 
         count(name = "weight") %>% 
         ungroup()
     
     pubs_before_grant <- pubs_before_grant %>% 
         filter(vedidk != 4427920)    #one vedidk had over 800 publications, which seemed unrealistic, so I deleted it
     
     sum(pubs_before_grant$weight<11) #7 autoru ma 5 nebo mene publikaci a 47 autoru (cca 20 % vzorku) ma 10 nebo mene
     
     median(pubs_before_grant$weight) #průměr je 23.1 publikace na autora, median je 19 publikace na autora
     
     graph_before_grant <- ggplot(data=pubs_before_grant, aes(x = reorder(vedidk, -weight), y=weight)) +
         geom_col() + ggtitle("Number of publications per research (before grant)") +
         xlab("Researchers (vedidk)") + ylab("Number of publications")
     
     graph_before_grant
     
     ##plot career starting years across recipients
     
     career_start <- bg %>% 
         group_by(vedidk) %>% 
         slice(which.min(year)) %>% 
         ungroup()
     
     # career_start_2 <- bg %>% 
     #     group_by(vedidk) %>% 
     #     filter(year == min(year)) %>% 
     #     ungroup() %>% 
     #     select(-c(id_unique)) %>% 
     #     distinct()
     
     career_start_2 <- career_start %>% 
         group_by(year) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     
     graph_career_start <- ggplot(data=career_start_2, aes(x = year, y=frequency)) +
         geom_col() + ggtitle("Distribution of career starting year across focal researchers") +
         xlab("Starting year (first publication)") + ylab("Number of researchers (vedidk)")
     
     graph_career_start
     
     ##plot differences between career starting years and year of receiving grant
     
     career_start$grant_start_year <- as.numeric(career_start$grant_start_year)
     career_start$year <- as.numeric(career_start$year)
     
     career_start$difference <- career_start$grant_start_year-career_start$year
     
     difference <- career_start %>% 
         group_by(difference) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     graph_difference <- ggplot(data=difference, aes(x = difference, y=frequency)) +
         geom_col() + ggtitle("Distribution of differences between career starting year and year of receiving grant") +
         xlab("Difference (years)") + ylab("Number of researchers")
     
     graph_difference
     
     
     sum(career_start$difference>8) #interestingly, 144 people (cca 60 %) received the grant more than 8 years after their first publications --> given that the grants should only be given to people up to 8 years after finishing their PhD degree, this is very suprising
     
     median(career_start$difference) #to colour this further, median difference is 9 years, and mean 9.3, which is both above 8
     
     
     ##plotting number of publications of all supervisors
     
     all_supervisors_vector <- ids_complete$vedoucí.vedidk %>% 
         as_tibble() %>% 
         distinct() %>% #this was added - I should check that it does change any further results 
         as_vector() 
     
     all_sup_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
         select(vedidk, id_unique) %>% 
         filter(vedidk %in% all_supervisors_vector) %>% 
         distinct() %>% 
         as_tibble()
     
     # # z nejakeho duvodu ten postup nize nefunguje, vzdy skoncim se stejnym poctem publikaci jako na zacatku (7770)
     # all_sup_filtered_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()
     #     select(id_unique, pub_type) %>%
     #     filter(id_unique %in% all_sup_pubids$id_unique) %>%
     #     filter(pub_type %in% c("J","B","C","D")) %>%
     #     as_tibble()
     # 
     # all_sup_pubids_final <- DBI::dbReadTable(con, "authors_by_pubs") %>%
     #     select(id_unique, vedidk) %>%
     #     filter(id_unique %in%  all_sup_filtered_pubs$id_unique) %>%
     #     distinct() %>%
     #     filter(vedidk %in% all_supervisors_vector) %>%
     #     distinct() %>%
     #     as_tibble()
     
     all_sup_pubs <- all_sup_pubids %>% 
         group_by(vedidk) %>% 
         count(name = "weight") %>% 
         ungroup()    #this will generate only 211 vedidks, not 248 entered in the all_supervisors_vector, which is 15 % loss --> explained by duplicates in vedidks 
     
     
     # ids_complete$vedoucí.vedidk <- as.character(ids_complete$vedoucí.vedidk)
     # 
     # missing_vedouci <- anti_join(ids_complete, all_sup_pubs, by = c("vedoucí.vedidk" = "vedidk"))
     # 
     # ids_complete_duplicated_vedouci <- ids_complete %>% 
     #     filter(duplicated(vedoucí.vedidk)) 
     
     
     # #when examininsg numbers, I see that vedidk 5500095 has 890 publications, but here I am less sure whether to discard it, so I left it in
     # all_researchers_pubs <- all_researchers_pubs %>% 
     #     filter(vedidk != 5500095)    #one vedidk had over 800 publications, which seemed unrealistic, so I deleted it
     
     sum(all_sup_pubs$weight<11) #5 autoru ma 10 nebo mene publikaci (compared to 29 of core researchers)
     
     median(all_sup_pubs$weight) #průměr je 101 publikace na autora (compared to 28.9 of core researchers)
     
     graph_all_supervisors <- ggplot(data=all_sup_pubs, aes(x = reorder(vedidk, -weight), y=weight)) +
         geom_col() + ggtitle("Number of publications per supervisor (whole career)") +
         xlab("supervisors (vedidk)") + ylab("Number of publications")
     
     graph_all_supervisors
     
     # plotting career starting year of supervisors
     
    sup_pubid_years <- DBI::dbReadTable(con, "riv_disc") %>% 
         select(id_unique, year) %>% 
         filter(id_unique %in% all_sup_pubids$id_unique) %>% 
         distinct() %>% 
         as_tibble()
     
    sup_pubs_enriched <- left_join(sup_pubid_years, all_sup_pubids, by = "id_unique") %>% 
         distinct()
     
     
     career_start_sup <- sup_pubs_enriched %>% 
         group_by(vedidk) %>% 
         slice(which.min(year)) %>% 
         ungroup()
     
     # career_start_2 <- bg %>% 
     #     group_by(vedidk) %>% 
     #     filter(year == min(year)) %>% 
     #     ungroup() %>% 
     #     select(-c(id_unique)) %>% 
     #     distinct()
     
     career_start_sup_2 <- career_start_sup %>% 
         group_by(year) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     
     graph_career_start_sup <- ggplot(data=career_start_sup_2, aes(x = year, y=frequency)) +
         geom_col() + ggtitle("Distribution of career starting year across supervisors") +
         xlab("Starting year (first publication)") + ylab("Number of researchers (vedidk)")
     
     graph_career_start_sup
     
     
     # plotting differences between career starting year of researcher and supervisor
     
     career_start <- rename(career_start, year_researcher = year, vedidk_sup = vedoucí.vedidk)
     career_start_sup<- rename(career_start_sup, year_supervisor = year, vedidk_sup = vedidk)
     
     
     career_start_both <- left_join(career_start_sup, career_start, by = "vedidk_sup")
     
     career_start_both$year_researcher <- as.numeric(career_start_both$year_researcher)
     career_start_both$year_supervisor <- as.numeric(career_start_both$year_supervisor)
     
     career_start_both$difference <- career_start_both$year_researcher-career_start_both$year_supervisor
     
     career_start_both <- career_start_both %>% 
         filter(!is.na(difference))
     
     difference_both <- career_start_both %>% 
         group_by(difference) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     graph_difference_both <- ggplot(data=difference_both, aes(x = difference, y=frequency)) +
         geom_col() + ggtitle("Distribution of differences between researcher career starting year and supervisor career starting year") +
         xlab("Difference (years)") + ylab("Number of cases")
     
     graph_difference_both
     
     
     sum(career_start_both$difference<1) #interestingly, 10 pairs of people (cca 4 %) have 0 or fewer years difference, meaning that the supervisor would be younger than the foical researcher
     
     median(career_start_both$difference) #to colour this further, median difference is 9 years, and mean 9.3, which is both above 8
     
     
}


