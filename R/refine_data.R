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
refine_data <- function(matching, db_path, ids_complete, sup_control, authors_arrow) {

   #  ids_refined <- ids %>%
   #      select(vedoucí.vedidk, vedidk_core_researcher) %>%
   #      filter(!is.na(vedoucí.vedidk)) %>%
   #      filter(!is.na(vedidk_core_researcher)) %>%
   #      distinct()
   # 
   #  ids_refined$vedidk_core_researcher <- as.character(ids_refined$vedidk_core_researcher)
   #  ids_refined <- dplyr::rename(ids_refined, sup_vedidk = vedoucí.vedidk)
   #  ids_refined <- dplyr::rename(ids_refined, vedidk = vedidk_core_researcher)
   # 
   #  sup_control <- sup_control %>%
   #      select(vedidk, sup_vedidk) %>%
   #      distinct()
   # 
   #  sup_control$vedidk <- as.character(sup_control$vedidk)
   # 
   #  sup_complete <- rbind2(ids_refined, sup_control) %>% 
   #      filter(!is.na(sup_vedidk))
   #  
   #  # sup_vector <- sup_complete %>%
   #  #         filter(vedidk_core_researcher %in% ids_complete_vector) %>%
   #  #         pull(vedoucí.vedidk)
   # 
   #  con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
   #  on.exit(DBI::dbDisconnect(con))
   #  
   # sup_name <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
   #      filter(vedidk %in% sup_complete$sup_vedidk) %>%
   #      dplyr::select(id_helper, vedidk) %>%
   #      group_by(vedidk) %>% 
   #      dplyr::count(id_helper) %>%
   #      filter(n == max(n)) %>%
   #      slice_sample(n=1) %>%
   #      ungroup() %>% 
   #      dplyr::select(!n)
   #  
   #  sup_name <- dplyr::rename(sup_name, sup_name = id_helper)
   # 
   #  sup_complete$sup_vedidk <- as.character(sup_complete$sup_vedidk)
   #  
   #  sup_complete <- left_join(sup_complete, sup_name, by = c("sup_vedidk" = "vedidk"))
   #  
   # 
   #  
   # 
   #  complete_data <- left_join(matching, sup_complete, by = "vedidk")
   #  
   #  # complete <- complete_data %>% filter(!is.na(sup_name))
   #  # complete1 <- complete %>% filter(!vedidk %in% vedidk_treatment) %>% filter(treatment == 1)
   #  # d <- anti_join(complete, complete1, by = "vedidk")
   #  # # table(d$treatment, by =d$independence_timing)
   #  
    
    new2 <- matching %>% 
        filter(treatment != 1) %>% 
        dplyr::select(vedidk) %>% 
        distinct() %>% 
        mutate(sup_details = furrr::future_pmap(.l = list(vedidk),
                                       .f = function(first){
                                           read_sup(db_path, authors_arrow, 
                                                    ids_full_vector = first)
                                       } ))
    
    
    
    new_unnested <- new2 %>% tidyr::unnest(sup_details)
    
    ids_complete_sup <- ids_complete %>% dplyr::select(vedidk, sup_name, sup_vedidk)
    
    new_unnested <- bind_rows(new_unnested, ids_complete_sup)
    
    a_final_data <- left_join(matching, new_unnested, by = "vedidk") %>% 
        filter(!is.na(sup_name))
    
    b <- a_final_data %>% 
        group_by(subclass) %>%
        filter(n() == 1) %>%
        ungroup() %>%
        filter(treatment == 1)
    d <- anti_join(a_final_data, b, by = "vedidk")
    # table(d$treatment, by =d$independence_timing)
    
    #by this code below, I attach the cases in which no supervisor was found (i.e. cases where these people had no coauthors in theri first 5 publications) 
    aa <- anti_join(matching, d, by = "vedidk")
    bb <- left_join(aa, d)

    cc <- bind_rows(d, bb)
    
}
