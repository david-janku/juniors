#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param matching
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_ids_full_vector <- function(matching) {

    ids_full_vector <- matching$vedidk %>% 
        as_tibble() %>% 
        as_vector() %>% 
        as_tibble() %>% #from here below it was added - I should check that it does change any further results 
        distinct() %>% #here
        as_vector()  #here

}
