#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param db_path
#' @param ids_complete_vector
#' @param ids_complete
#' @param final_data
#' @param ids_full_vector
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
read_all_authors <- function(db_path, ids_complete_vector, ids_complete,
                             final_data, ids_full_vector) {

final_data <- final_data %>% 
     filter(!is.na(sup_name))
    
matching <- as_tibble(final_data)
# l <- list(ids_full_vector = matching$vedidk, intervention_year = matching$treatment_year, timing = matching$independence_timing)


new <-  matching %>% 
        dplyr::select(vedidk, treatment_year, independence_timing, sup_name, sup_vedidk) %>% 
        mutate(pub_table = purrr::pmap(.l = list(vedidk, treatment_year, independence_timing),
                                       .f = function(first, second, third){
                                           read_one_author(db_path, ids_complete_vector, ids_complete, matching, 
                                                                 ids_full_vector = first, intervention_year = second, timing = third)
                                       } ))

# intervention_year = treatment_year, timing = independence_timing
#     ids_full_vector = vedidk)

    }
