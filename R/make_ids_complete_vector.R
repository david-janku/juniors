#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_ids_complete_vector <- function(ids) {

    ids_complete <- as_tibble(ids) %>% 
        filter(!is.na(vedoucí.vedidk)) %>% 
        filter(!is.na(vedidk_core_researcher))
    
    ids_complete_vector <- ids_complete$vedidk_core_researcher %>% 
        as_tibble() %>% 
        as_vector() 

}
