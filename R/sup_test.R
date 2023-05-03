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
sup_test <- function(matching, db_path, ids, sup_control, ids_complete) {

    
   #treatment group
   #ids_complete <- read.csv2(here::here("data", "raw", "supervisors.csv")) %>% filter(!is.na(vedoucí.vedidk)) %>% filter(!is.na(vedidk_core_researcher)) %>% dplyr::rename(ids_complete, sup_vedidk = vedoucí.vedidk) %>% dplyr::rename(ids_complete, vedidk = vedidk_core_researcher)
    
    matching <- matching %>% filter(vedidk %in% ids_complete$vedidk) %>% select(!independence_timing) %>% distinct()
    
    new_treatment <-  matching %>% 
        dplyr::select(vedidk) %>% 
        distinct() %>% 
        mutate(sup_details = purrr::pmap(.l = list(vedidk),
                                         .f = function(first){
                                             read_sup(db_path, 
                                                      ids_full_vector = first)
                                         } ))
    
    new_unnested_treatment <- new_treatment %>% tidyr::unnest(sup_details)
    
    # readr::write_csv2(new_unnested_treatment, here::here("data", "derived", "treatment_sup_test.csv"), na = "NA")
    # new_unnested_treatment <- read.csv2(here::here("data", "derived", "treatment_sup_test.csv"))
    
    ids_complete$vedidk <- as.character(ids_complete$vedidk)
    treatment_match <- left_join(ids_complete, new_unnested_treatment, by = "vedidk") %>% mutate(match = ifelse(sup_vedidk.x == sup_vedidk.y, 1, 0))
    
    
    plyr::count(treatment_match$match)
    treatment_match$match <- as.numeric(treatment_match$match)
    summary(treatment_match$match)
    
    
    
    # control group
    # sup_control <- read.csv2(here::here("data", "raw", "supervisors_control_final.csv"))
    
    
    
    sup_control <- sup_control %>% filter(!is.na(sup_vedidk))
    
    matching <- matching %>% filter(vedidk %in% sup_control$vedidk) %>% select(!independence_timing) %>% distinct()
    
    new2 <-  matching %>% 
        dplyr::select(vedidk) %>% 
        distinct() %>% 
        mutate(sup_details = purrr::pmap(.l = list(vedidk),
                                       .f = function(first){
                                           read_sup(db_path, 
                                                    ids_full_vector = first)
                                       } ))
    
    new_unnested <- new2 %>% tidyr::unnest(sup_details)
    
    # readr::write_csv2(new_unnested, here::here("data", "derived", "control_sup2.csv"), na = "NA")
    # new_unnested <- read.csv2(here::here("data", "derived", "control_sup2.csv"))
    
    sup_control$vedidk <- as.character(sup_control$vedidk)
    new_unnested$vedidk <- as.character(new_unnested$vedidk)
    sup_control <- semi_join(sup_control, matching, by = "vedidk") #should limit the number of rows in sup_control to 140
    
    control_sup_match <- left_join(sup_control, new_unnested, by = "vedidk") %>% mutate(match = ifelse(sup_vedidk.x == sup_vedidk.y, 1, 0))
    
    
    plyr::count(control_sup_match$match)
    control_sup_match$match <- as.numeric(control_sup_match$match)
    summary(control_sup_match$match)
    
    
    
    #independent slovenian sample 
    
    luzar <- read.csv2(here::here("data", "raw", "luzar.csv")) %>% mutate(sup = ifelse(sup_pubs>coauthor_pubs, 1, 0))
    
    plyr::count(luzar$sup)
    mean(luzar$sup)
    # sum(luzar$NUM.Publications == 0)
    luzar <- luzar %>% filter(NUM.Publications != 0)
    plyr::count(luzar$sup)
    mean(luzar$sup)
    
    
    

}
