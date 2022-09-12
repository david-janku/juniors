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
new <-  head(matching) %>% 
        select(vedidk, treatment_year, independence_timing) %>% 
        mutate(pub_table = purrr::map(vedidk, intervention_year = treatment_year, timing = independence_timing, ~read_one_author(db_path, ids_complete_vector, ids_complete,
                                                                  matching, ..1, ..2, ..3)))

# intervention_year = treatment_year, timing = independence_timing
#     ids_full_vector = vedidk)

    }
