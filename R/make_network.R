#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param all_authors
#' @param 
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_network <- function(all_authors) {


coauthorship <-  all_authors %>% 
            mutate(edgelist = purrr::map(all_authors$pub_table, 
                                 function(first){make_edgelist(one_author = first) }))


    }
