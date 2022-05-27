#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_ids_complete_vector <- function(ids_complete) {

    ids_complete <- as_tibble(ids_complete) %>% 
        filter(!is.na(vedoucí.vedidk)) %>% 
        filter(!is.na(vedidk_core_researcher))
    
    ids_complete_vector <- ids_complete$vedidk_core_researcher %>% 
        as_tibble() %>% 
        as_vector() %>% 
        as_tibble() %>% #from here below it was added - I should check that it does change any further results 
        distinct() %>% #here
        as_vector()  #here

}
