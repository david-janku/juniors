#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param db_path
#' @param ids
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
match_obj_unfunded <- function(matching_data) {

    matching_data <- matching_data %>% filter(!(treatment == 0 & grants != 0))
    
    matched_obj_unfunded <- matchit(treatment~length+pubs_total+ws_pubs+interdisc_proportion+gender+total_coauthor_count, method="nearest", data=matching_data, distance = "mahalanobis", ratio = 1, exact = c("disc_ford", "treatment_year"), replace = TRUE)
    
   
}
