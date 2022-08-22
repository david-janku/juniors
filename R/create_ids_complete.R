#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
create_ids_complete <- function(ids) {

    ids_complete <- as_tibble(ids) %>% 
        select(vedidk_core_researcher, vedoucí.vedidk, grant_start_year) %>% 
        filter(!is.na(vedoucí.vedidk)) %>% 
        filter(!is.na(vedidk_core_researcher))
        
    #these row below are new - their point is to filter the cases of researchers who received 2 grants, such that only the record of receving the first grant will remain
    
    ids_complete$grant_start_year <- substring(ids_complete$grant_start_year, 7)  
    ids_complete$grant_start_year <- as.numeric(ids_complete$grant_start_year)
    
    ids_complete <- ids_complete %>% 
        group_by(vedidk_core_researcher) %>% 
        slice(which.min(grant_start_year)) %>% 
        ungroup()

}
