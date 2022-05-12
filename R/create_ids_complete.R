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
        select(vedidk_core_researcher, vedoucí.vedidk) %>% 
        filter(!is.na(vedoucí.vedidk)) %>% 
        filter(!is.na(vedidk_core_researcher)) 

}
