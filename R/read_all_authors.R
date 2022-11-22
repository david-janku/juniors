#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param db_path
#' @param ids_complete_vector
#' @param ids_complete
#' @param matching
#' @param ids_full_vector
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
read_all_authors <- function(db_path, ids_complete_vector, ids_complete,
                             matching, ids_full_vector) {

matching <- as_tibble(matching)
# l <- list(ids_full_vector = matching$vedidk, intervention_year = matching$treatment_year, timing = matching$independence_timing)


new <-  head(matching) %>% 
        dplyr::select(vedidk, treatment_year, independence_timing) %>% 
        mutate(pub_table = purrr::pmap(.l = list(vedidk, treatment_year, independence_timing),
                                       .f = function(first, second, third){
                                           read_one_author(db_path, ids_complete_vector, ids_complete, matching, 
                                                                 ids_full_vector = first, intervention_year = second, timing = third)
                                       } ))

# intervention_year = treatment_year, timing = independence_timing
#     ids_full_vector = vedidk)

    }
