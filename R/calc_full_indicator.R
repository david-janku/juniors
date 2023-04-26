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
 
               
    # independent_topics <- independent_topics %>% 
    #     mutate_all(~ ifelse(. == "numeric(0)", NA, .))
   
    independent_topics <- independent_topics %>% 
        mutate_all(~ na_if(., "numeric(0)"))
    
    # independent_topics <- na.omit(independent_topics)
    
    independent_topics$RII <- ((1-independent_topics$eig_centr)+independent_topics$clustr+independent_topics$ind_pubs+independent_topics$ind_topics*2)/4
    
        
    # a <- right_join(bind_rows(eigen_centr), bind_rows(clustering), by = "vedidk")
    # b <- right_join(bind_rows(a), bind_rows(ind_pubs), by = "vedidk")        
    # c <- right_join(bind_rows(b), bind_rows(ind_topics), by = "vedidk")    
    # 
    # c$RII <- ((1-c$eig)+c$clustr+c$ind_pubs+c$ind_topics*2)/4
    # 
    # c
    
    
    # write.csv2(final_data, here::here("data", "derived", "final_indicators.csv"), na = "NA")
    # readr::write_excel_csv2(final_data, here::here("data", "derived", "final_indicators.csv"), na = "NA")
}
