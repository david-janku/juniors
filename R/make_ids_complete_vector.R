#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_ids_complete_vector <- function(ids_complete) {
    
    
  ids_complete_vector <- ids_complete %>% 
        dplyr::select(vedidk) %>% 
        as_tibble() %>%
        distinct() %>% 
        as_vector()
        
}
