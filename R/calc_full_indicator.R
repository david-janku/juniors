#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param independent_topics
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_full_indicator <- function(independent_topics) {
 
               
    # independent_topics$eig_centr <- as.numeric(independent_topics$eig_centr)
    # independent_topics$clustr <- as.numeric(independent_topics$clustr)
    # independent_topics$ind_pubs <- as.numeric(independent_topics$ind_pubs)
    # independent_topics$ind_topics <- as.numeric(independent_topics$ind_topics)
    
    independent_topics <- independent_topics %>% 
        # mutate_at(vars(eig_centr, clustr, ind_pubs, ind_topics), as.numeric) %>%  
        mutate(across(eig_centr:ind_topics, as.numeric))  
        # mutate_all(~ na_if(., "numeric(0)")) 
    
    independent_topics$eig_centr <- unlist(independent_topics$eig_centr)
    independent_topics$clustr <- unlist(independent_topics$clustr)
    independent_topics$ind_pubs <- unlist(independent_topics$ind_pubs)
    independent_topics$ind_topics <- unlist(independent_topics$ind_topics)
    
    
    independent_topics <- independent_topics %>% mutate(eig_centr = ifelse(is.na(eig_centr) & !is.na(sup_name), 0, eig_centr)) #this turns all the cases where there is a sup_name but the supervisor was not found in the authors coauthorship network (and therefore eig_centr turned NA) make it a 0, because it should be interpreted as "their independence is at max, because their supervisor is not even in their network anymore" 
    independent_topics <- independent_topics %>% mutate(clustr = ifelse(is.na(clustr) & !is.na(sup_name), 1, clustr)) #this turns all the cases where there is a sup_name but the supervisor was not found in the authors coauthorship network (and therefore clustr turned NA) make it a 0, because it should be interpreted as "their independence is at max, because their supervisor is not even in their network anymore" 
    
    
    independent_topics <- independent_topics %>% 
        mutate(RII = ifelse(is.na(eig_centr) | is.na(clustr) | is.na(ind_pubs) | is.na(ind_topics), NA, ((1-eig_centr)+clustr+ind_pubs+ind_topics)/4))
    
    
     # independent_topics$RII <- ifelse(is.na(independent_topics$eig_centr) | is.na(independent_topics$clustr) | is.na(independent_topics$ind_pubs) | is.na(independent_topics$ind_topics), NA, ((1-independent_topics$eig_centr)+independent_topics$clustr+independent_topics$ind_pubs+independent_topics$ind_topics*2)/4)
    
     

    # a <- right_join(bind_rows(eigen_centr), bind_rows(clustering), by = "vedidk")
    # b <- right_join(bind_rows(a), bind_rows(ind_pubs), by = "vedidk")        
    # c <- right_join(bind_rows(b), bind_rows(ind_topics), by = "vedidk")    
    # 
    # c$RII <- ((1-c$eig)+c$clustr+c$ind_pubs+c$ind_topics*2)/4
    # 
    # c
    
    
    # independent_topics$id <- seq_along(independent_topics$vedidk)
    
    
    d <- left_join(final_data, independent_topics)
    
    
    d$treatment <- as_factor(d$treatment)
    d$independence_timing <- as_factor(d$independence_timing)
    
    
    
    #some further refinements and analyses
    ##how many final RII scores is missing?
    
    sum(is.na(d$RII))
    
    sum(is.na(d$RII))/nrow(d) # this suggest about 38,5 % of all RII scores are missing 
    
    
    ##how many of that is because given researchers didnt have any publications?
    
    
    d <- d %>%
        mutate(pub_table_empty = purrr::map_lgl(pub_table, function(df) {
            if (is.data.frame(df)) {
                return(all(is.na(df)) || nrow(df) == 0)
            } else {
                return(TRUE)
            }
        }))
    
    # table(d$pub_table_empty, by = d$independence_timing)
    # table(d$pub_table_empty, by = d$treatment_year)
    # table(d$pub_table_empty, by = d$treatment)
    
     # sum(d$pub_table_empty == TRUE)
     # 
     # sum(d$pub_table_empty == TRUE) / nrow(d) # this says about 20 % of observations have no pubs 
     # 
     # sum(d$pub_table_empty == TRUE) / sum(is.na(d$RII)) # this says that missing pubs explain about 52 % of the missing RII scores  
    
    ##how many of that is because we could not find people's supervisors?
    
    # f <- d %>% filter(pub_table_empty == FALSE)
    # 
    # sum(is.na(d$sup_name))
    # sum(is.na(f$sup_name))
    # 
    # sum(is.na(d$sup_name)) / nrow(d) # this shows that X % of observations have no supervisor 
    # 
    # sum(is.na(f$sup_name)) / sum(is.na(d$RII)) # this shows that missing supervisors explain X % of all misssing RII scores  
    # sum(is.na(f$sup_name)) / sum(is.na(f$RII)) # this shows that missing supervisors explain X % of all remaining misssing RII scores  
    
    
    
    
    ##how many of that is because we could not find supervisors vedidks?
    
    
    
    # w <- f %>% filter(!is.na(sup_name))
    # 
    # sum(is.na(d$sup_vedidk))
    # sum(is.na(w$sup_vedidk))
    # 
    # sum(is.na(d$sup_vedidk)) / nrow(d) # this shows that 12 % of observations have no supervisor vedidk 
    # 
    # sum(is.na(w$sup_vedidk)) / sum(is.na(d$RII)) # this shows that missing supervisors explain 24 % of all missing RII scores
    # sum(is.na(w$sup_vedidk)) / sum(is.na(w$RII)) # this shows that missing supervisors explain 24 % of remaining missing RII scores  
    # 
    # 
    # q <- w %>% filter(!is.na(sup_vedidk)) %>% filter(is.na(RII))
    # 
    # sum(is.na(q$RII))
    # 
    # table(q$treatment, by = q$independence_timing) #this suggests that before intervention, there are still about 10 cases in each control group who didnt have RII scores, while only 1 observation in the treatment group -> that suggests that maybe the choice of supervisors for the control groups doesnt work that well, because these supervisors didnt appear in the authors coauthor networks
    
    # write.csv2(final_data, here::here("data", "derived", "final_indicators.csv"), na = "NA")
    # readr::write_excel_csv2(final_data, here::here("data", "derived", "final_indicators.csv"), na = "NA")
}
