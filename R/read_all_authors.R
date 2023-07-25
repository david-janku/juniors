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
                             final_data, ids_full_vector, authors_arrow) {

final_data <- final_data %>% 
     filter(!is.na(sup_name))
    
matching <- as_tibble(final_data)
# l <- list(ids_full_vector = matching$vedidk, intervention_year = matching$treatment_year, timing = matching$independence_timing)


new <-  matching %>% 
        dplyr::select(id, vedidk, treatment_year, independence_timing, sup_name, sup_vedidk) %>% 
        mutate(pub_table = furrr::future_pmap(.l = list(vedidk, treatment_year, independence_timing),
                                       .f = function(first, second, third){
                                           read_one_author(db_path, ids_complete_vector, ids_complete, matching, authors_arrow,
                                                                 ids_full_vector = first, intervention_year = second, timing = third)
                                       } ))


new <- new %>%
    mutate(pubs_number = purrr::map_int(pub_table, function(df) {
        if (is.data.frame(df) && nrow(df) > 0) {
            return(length(unique(df$id_unique)))
        } else {
            return(0)
        }
    }))


 
# w <- plyr::count(all_authors$pubs_number)
# plot(w)

 new <- new %>% filter(pubs_number >= 3)

# wq <- plyr::count(new$pubs_number)
# plot(wq)
 
 new

    }
