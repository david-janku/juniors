#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param matching
#' @param db_path
#' @param ids
#' @param sup_control
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
connect_data <- function(matching, db_path, ids_complete, sup_complete) {
    
    
    
    ids_complete_sup <- ids_complete %>% dplyr::select(vedidk, sup_name, sup_vedidk)
    
    sup_complete_sup <- sup_complete %>% dplyr::select(vedidk, sup_name, sup_vedidk)
    
    all_sup <- bind_rows(ids_complete_sup, sup_complete_sup)
    
    final_data <- left_join(matching, all_sup, by = "vedidk")
    
    
    # sum(is.na(final_data$sup_name))
    
}
