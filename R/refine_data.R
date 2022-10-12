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
refine_data <- function(matching, db_path, ids, sup_control) {

    ids_refined <- ids %>%
        select(vedoucí.vedidk, vedidk_core_researcher) %>%
        filter(!is.na(vedoucí.vedidk)) %>%
        filter(!is.na(vedidk_core_researcher)) %>%
        distinct()

    ids_refined$vedidk_core_researcher <- as.character(ids_refined$vedidk_core_researcher)
    ids_refined <- rename(ids_refined, sup_vedidk = vedoucí.vedidk)
    ids_refined <- rename(ids_refined, vedidk = vedidk_core_researcher)

    sup_control <- sup_control %>%
        select(vedidk, sup_vedidk) %>%
        distinct()

    sup_control$vedidk <- as.character(sup_control$vedidk)

    sup_complete <- rbind2(ids_refined, sup_control)

    complete_data <- left_join(matching, sup_complete, by = "vedidk")
    
    

}
