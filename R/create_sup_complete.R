#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
create_sup_complete <- function(all_sup, sup_control, db_path, matching_data) {
    
    
    sup_complete <- bind_rows(all_sup, sup_control) %>%   
        mutate(across(everything(), ~replace(., . == "", NA))) %>% 
        filter(!is.na(sup_name_first)) %>%
        dplyr::select(vedidk, disc_ford, name_first, name_last, org_name, sup_name_first, sup_name_last) %>% 
        distinct() 
    
    
    
# getting sup vedidk
    
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    sup_vedidk <- dplyr::tbl(con, "authors_by_pubs") %>% 
        dplyr::select(name_first, name_last, id_helper, vedidk) %>% 
        distinct() %>% 
        collect()
    
    sup_vedidk <- left_join(sup_complete, sup_vedidk, by = c("sup_name_first" = "name_first", "sup_name_last" = "name_last"))
    
    
    sup_unique_rows <- sup_vedidk %>%
        group_by(vedidk.x) %>%
        filter(n() == 1) %>%
        ungroup()
    
    sup_non_unique_rows <- sup_vedidk %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    ## disambiguation based on coauthoring at least one paper
    
    
    authors_by_pubs_df <- dplyr::tbl(con, "authors_by_pubs") %>%
        dplyr::select(id_unique, name_first, name_last, vedidk, id_helper) %>% 
        collect()
    
    # Check for common id_unique for each row of non_unique_rows
    has_common_id_unique <- apply(sup_non_unique_rows, 1, function(row) {
        id_unique_for_vedidk_x <- authors_by_pubs_df$id_unique[authors_by_pubs_df$vedidk == row["vedidk.x"]]
        id_unique_for_vedidk_y <- authors_by_pubs_df$id_unique[authors_by_pubs_df$vedidk == row["vedidk.y"]]
        
        length(intersect(id_unique_for_vedidk_x, id_unique_for_vedidk_y)) > 0
    })
    
    # Filter rows based on the result
    sup_coauth_rest <- sup_non_unique_rows[has_common_id_unique, ]
    
    
    dup_matches <- sup_coauth_rest %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    sup_coauth_rest <- anti_join(sup_coauth_rest, dup_matches, by = "vedidk.x")
    
    ### tady pozor! funkce v řádku nahoře vyhodí dva případy kdy jeden výzkumník byl spoluautorem se dvěma lidmi, kteří mají stejné jeméno jako jeho supervisor ale jiné vedidky -> to je potřeba očistit!
    
    sup_total <- rbind(sup_unique_rows, sup_coauth_rest) #this show we were able to match 234 out of 259 supervisors vedidks
    
    
    ## disambiguation based on shared study discipline
    
    sup_rest <- anti_join(sup_vedidk, sup_total, by = "vedidk.x")
    # sup_rest <- rbind(sup_rest, dup_matches)
    
    sup_disc <- left_join(sup_rest, matching_data %>% dplyr::select(vedidk, disc_ford, field), by = c("vedidk.y" = "vedidk")) %>% 
        distinct()
    
    
    sup_filtered <- sup_disc %>% filter(disc_ford.x == disc_ford.y)
    
    ### tady pozor! funkce v řádku nahoře vyhodí jeden případ kdy je výzkumník napárován se dvěma lidmi kteří mají stejné jméno jako jeho supervisor, dokonce i stejný obor, ale jiný vedidk - to je potřeba očistit!
    
    sup_filtered$disc_ford.y <- NULL
    sup_filtered$field <- NULL
    sup_filtered <- rename(sup_filtered, disc_ford = disc_ford.x)
    
    dup_matches_disc <- sup_filtered %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    sup_filtered <- anti_join(sup_filtered, dup_matches_disc, by = "vedidk.x")
    
    sup_total <- rbind(sup_total, sup_filtered) #this shows we were able to match 248 out of 259 supervisors vedidks - still eleven to go!
    
  
    
    
    ##filtering by broader discipline - field of study
    
    sup_rest_field <- anti_join(sup_disc, sup_filtered, by = "vedidk.x")
    
    sup_rest_field <- left_join(sup_rest_field, matching_data %>% dplyr::select(disc_ford, field) %>% distinct(), by = c("disc_ford.x" = "disc_ford")) %>% 
        distinct()
    
    sup_filtered_field <- sup_rest_field %>% filter(field.x == field.y) 
    
    
    sup_filtered_field <- sup_filtered_field %>% dplyr::select(-disc_ford.y) %>% distinct()
    
    
    sup_filtered_field <- sup_filtered_field %>% dplyr::select(-field.x, -field.y)
    sup_filtered_field <- rename(sup_filtered_field, disc_ford = disc_ford.x)
    
    dup_matches_field <- sup_filtered_field %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    sup_filtered_field <- anti_join(sup_filtered_field, dup_matches_field, by = "vedidk.x")
    
    sup_total <- rbind(sup_total, sup_filtered_field)
    
    dup_matches_total <- sup_total %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    
    
    sup_rest_final <-  anti_join(sup_rest_field, sup_filtered_field, by = "vedidk.x") #už je jich jenom 6 a mám napsané v poznánmkách skoro u všech kteří supervisoři to jsou, tak to můžu asi nakódovat manuálně
    
    
    
    
    # disambiguation based on supervisor being older than supervisee --> based on the results, this didnt work that well

    sup_rest_final$vedidk.x <- as.character(sup_rest_final$vedidk.x)
    sup_rest_age <- left_join(sup_rest_final, matching_data %>% dplyr::select(vedidk, career_start_year), by = c("vedidk.x" = "vedidk")) %>%
        distinct()

    sup_rest_age <- left_join(sup_rest_age, matching_data %>% dplyr::select(vedidk, career_start_year), by = c("vedidk.y" = "vedidk")) %>%
        distinct()

    sup_rest_age <- sup_rest_age %>% filter(career_start_year.y < career_start_year.x)

    dup_matches_age <- sup_rest_age %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    sup_filtered_age <- anti_join(sup_rest_age, dup_matches_age, by = "vedidk.x") %>% 
        rename(disc_ford = disc_ford.x) %>% 
        dplyr::select(-field.x, -field.y, -career_start_year.x, -career_start_year.y, -disc_ford.y)
        
    
    sup_total <- rbind(sup_total, sup_filtered_age)
    
    
    
    ## other ideas for disambiguation: based on 0) check the data whether it is actually correct by checking the dis_link; 1) higher level study discipline (makes similar sense to the age); 2) the match in institution they published in the year that the supervisee started publishing (ie we have institution for supervisee already, we need to enrich dataset by institutions that supervisors published in the career_start_year of supervisees); 
    
       
    sup_rest_misc <- dup_matches_age %>% filter(vedidk.y %in% c(5928028, 8279209, 4096541, 8324735, 1273515, 4672941, 5263344, 1481894, 7901410, 4766814, 3906884, 9569707, 1261037, 8334137)) %>%
        dplyr::select(-disc_ford.y, -field.x, -field.y, -career_start_year.x, -career_start_year.y) %>%
        rename(disc_ford = disc_ford.x) %>%
        distinct() %>% 
        filter(vedidk.x != 6094996) %>% 
        filter(!is.na(disc_ford))
    
    sup_total <- rbind(sup_total, sup_rest_misc)
    
    
    #2154412 is wrong vedidk for the person 1273515 (that vedidk has only 1 pub, so I assume it was a mistake and they are actually same person). Same with 8324735 (real vedidk) and 9651454 (wrong vedidk). Anyway, in this case it there is another problem: 8324735 has pubs that start on 1996 in RIV, whereas only start in 2001 in our database - why is that? Similarly, in RIV the vedidk track 20 pubs, in our database only 13. And same story with vedidk 5928028.
    #the only unmatched observation will be 4984552 (Pavel Peukert) which should not be matched to anyone, because there was mistake in the data
    
    # dup_matches_aaa <- dup_matches_age %>% filter(!vedidk.x %in% sup_rest_misc$vedidk.x)
    
    
    
    sup_total <- rename(sup_total, vedidk = vedidk.x)
    sup_total <- rename(sup_total, sup_vedidk = vedidk.y)
    sup_total <- rename(sup_total, sup_name = id_helper)
    
    sup_total$vedidk <- as.character(sup_total$vedidk)
    sup_total$sup_vedidk <- as.character(sup_total$sup_vedidk)
    
    
    sup_total
    
    # sum(is.na(sup_total$sup_name_first))
    # sum(is.na(sup_total$sup_vedidk))
    
    
}
