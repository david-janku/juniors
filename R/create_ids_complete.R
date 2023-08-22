#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
create_ids_complete <- function(ids, db_path, matching_data) {

    ids_complete <- as_tibble(ids) %>% 
        select(vedidk_core_researcher, sup_name_first, sup_name_last, sup_vedidk, statni.prislusnost) %>% 
        mutate(across(everything(), ~replace(., . == "", NA))) %>% 
        filter(!is.na(sup_name_first)) %>% 
        filter(!is.na(vedidk_core_researcher))
        
    #adding info about discipline (based on which disciplinary committee evaluated the grant) and year the grant project started
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    DBI::dbListTables(con)
    
    GJ <- DBI::dbReadTable(con, "cep_details") %>%
        filter(program_kod == "GJ") %>% 
        select(kod, disc, ford, year_start) %>% 
        tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
        as_tibble
    
   
    GJ$disc_ford[GJ$disc_ford %in% c("BA", "BB")] <- 10100 #BD
    GJ$disc_ford[GJ$disc_ford %in% c("IN", "BC", "BD", "AF")] <- 10200
    GJ$disc_ford[GJ$disc_ford %in% c("BE", "BM", "BF", "BG", "BK", "BL", "BH", "BI","BN")] <- 10300
    GJ$disc_ford[GJ$disc_ford %in% c("CC", "CA", "CH", "CF", "CD", "CG", "CB")] <- 10400
    GJ$disc_ford[GJ$disc_ford %in% c("DA", "DB", "DC", "DE", "DG", "DO", "DK", "DL", "DM", "DI", "DJ")] <- 10500
    GJ$disc_ford[GJ$disc_ford %in% c("EA", "EB", "EE", "CE", "EB", "BO", "EF", "EG", "DA", "EH")] <- 10600
    
    GJ$disc_ford[GJ$disc_ford %in% c("JN", "JM", "GB", "AL", "JO")] <- 20100
    GJ$disc_ford[GJ$disc_ford %in% c("JA", "JB", "JW", "JD", "JC")] <- 20200
    GJ$disc_ford[GJ$disc_ford %in% c("JR", "JT", "JQ", "BJ", "JU", "JV", "JF", "JS", "JL")] <- 20300
    GJ$disc_ford[GJ$disc_ford %in% c("CI")] <- 20400
    GJ$disc_ford[GJ$disc_ford %in% c("JP", "JG", "JJ", "JH", "JI", "JK")] <- 20500
    GJ$disc_ford[GJ$disc_ford %in% c("FS")] <- 20600
    GJ$disc_ford[GJ$disc_ford %in% c("DH", "JE", "JT", "JP", "JQ")] <- 20700
    GJ$disc_ford[GJ$disc_ford %in% c("EI")] <- 20800
    GJ$disc_ford[GJ$disc_ford %in% c("EI")] <- 20900
    GJ$disc_ford[GJ$disc_ford %in% c("JJ")] <- 21000
    GJ$disc_ford[GJ$disc_ford %in% c("GM", "JI", "KA")] <- 21100
    
    GJ$disc_ford[GJ$disc_ford %in% c("EB", "EC", "FH", "FR", "ED")] <- 30100 #FP
    GJ$disc_ford[GJ$disc_ford %in% c("FA", "FB", "FC", "FD", "FF", "FG", "FI", "FJ", "FK", "FL", "FO", "FE")] <- 30200 #not added FH, FP
    GJ$disc_ford[GJ$disc_ford %in% c("FN", "FM", "DN", "AQ", "AK")] <- 30300 #FL, FP 
    GJ$disc_ford[GJ$disc_ford %in% c("EI")] <- 30400
    GJ$disc_ford[GJ$disc_ford %in% c("FP")] <- 30500
    
    GJ$disc_ford[GJ$disc_ford %in% c("GD", "GK", "GL", "DF", "GE", "GF", "GC")] <- 40100 
    GJ$disc_ford[GJ$disc_ford %in% c("GG", "GH", "GI")] <- 40200
    GJ$disc_ford[GJ$disc_ford %in% c("GJ")] <- 40300 
    GJ$disc_ford[GJ$disc_ford %in% c("EI", "GM")] <- 40400
    GJ$disc_ford[GJ$disc_ford %in% c("GM")] <- 40500
    
    GJ$disc_ford[GJ$disc_ford %in% c("AN")] <- 50100 
    GJ$disc_ford[GJ$disc_ford %in% c("AH", "GA")] <- 50200
    GJ$disc_ford[GJ$disc_ford %in% c("AM")] <- 50300 
    GJ$disc_ford[GJ$disc_ford %in% c("AO", "AC")] <- 50400
    GJ$disc_ford[GJ$disc_ford %in% c("AG")] <- 50500
    GJ$disc_ford[GJ$disc_ford %in% c("AD", "AE")] <- 50600
    GJ$disc_ford[GJ$disc_ford %in% c("DE", "AP")] <- 50700 #AO
    GJ$disc_ford[GJ$disc_ford %in% c("AJ", "AF")] <- 50800
    GJ$disc_ford[GJ$disc_ford %in% c("AK")] <- 50900
    
    GJ$disc_ford[GJ$disc_ford %in% c("AB", "AC")] <- 60100 
    GJ$disc_ford[GJ$disc_ford %in% c("AI", "AJ")] <- 60200
    GJ$disc_ford[GJ$disc_ford %in% c("AA")] <- 60300 
    GJ$disc_ford[GJ$disc_ford %in% c("AL")] <- 60400
    
    
    
    #here needs to be code that will, in the GJ table, translate old discipline tags to ford discipline tags - here is manual https://www.isvavai.cz/dokumenty/Prevodnik_oboru_Frascati_v2.pdf
    
       
    #      
    # GJ_vector <- GJ$kod %>% 
    #     as_tibble() %>% 
    #     as_vector() %>% 
    #     as_tibble() %>% #from here below it was added - I should check that it does change any further results 
    #     distinct() %>% #here
    #     as_vector()
        
    resitele <- DBI::dbReadTable(con, "cep_investigators") %>%
        filter(kod %in% GJ$kod) %>%
        select(kod, vedidk) %>% 
        distinct() %>%
        filter(!is.na(vedidk))
        
    
    nationality <- DBI::dbReadTable(con, "riv_authors") %>% 
        dplyr::select(vedidk, nationality)
    
    
    resitele <- left_join(resitele, nationality, by = "vedidk") %>% 
        filter(nationality == "CZ") %>% 
        filter(vedidk %in% ids_complete$vedidk_core_researcher)
    
    
    resitele_complet <- left_join(resitele, GJ, by = "kod")
    resitele_complet$vedidk <- as.integer(resitele_complet$vedidk)
    
    ids_complete <- left_join(ids_complete, resitele_complet, by=c("vedidk_core_researcher" = "vedidk"))
    
    #cleaning start year info and selecting only the first grant the researcher received 
    
    ids_complete$year_start <- substring(ids_complete$year_start, 7)  
    ids_complete$year_start <- as.numeric(ids_complete$year_start)
    
    ids_complete <- ids_complete %>% 
        group_by(vedidk_core_researcher) %>% 
        slice(which.min(year_start)) %>% 
        ungroup()
    
    ids_complete <- dplyr::rename(ids_complete, sup_vedidk = sup_vedidk)
    ids_complete <- dplyr::rename(ids_complete, vedidk = vedidk_core_researcher)
    ids_complete$sup_vedidk <- as.character(ids_complete$sup_vedidk)
    
    
    # control_id <-  ids_complete$sup_vedidk
    # sup_names <- dplyr::tbl(con, "authors_by_pubs") %>% # colnames()
    #     dplyr::select(id_helper, vedidk) %>% 
    #     filter(vedidk %in% control_id) %>% 
    #     dplyr::rename(sup_name = id_helper) %>%
    #     dplyr::rename(sup_vedidk = vedidk) %>%
    #     dplyr::collect() %>% 
    #     distinct() #tady ideálně ještě přidat část kódu která říká "pokud je tam nějaký vedidk více než jednou, vyber jeho nejčastější sup_name -> v našem vzorku ale není žádný vedidk více než jednou, takže ok
    # 
    # ids_complete <- left_join(ids_complete, sup_names, by = "sup_vedidk")
    #  
    # ids_complete$vedidk <- as.character(ids_complete$vedidk)
    # 
    # ids_complete
    
    
    
    sup_vedidk <- dplyr::tbl(con, "authors_by_pubs") %>% 
        dplyr::select(name_first, name_last, id_helper, vedidk) %>% 
        distinct() %>% 
        collect()
    
    sup_vedidk <- left_join(ids_complete, sup_vedidk, by = c("sup_name_first" = "name_first", "sup_name_last" = "name_last"))
    
    
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
    
    
    sup_total <- rbind(sup_unique_rows, sup_coauth_rest) #this show we were able to match 234 out of 259 supervisors vedidks
    
    
    ## disambiguation based on shared study discipline
    
    sup_rest <- anti_join(sup_vedidk, sup_total, by = "vedidk.x")
    # sup_rest <- rbind(sup_rest, dup_matches)

    sup_rest$disc_ford <- NULL    
    sup_rest$vedidk.x <- as.character(sup_rest$vedidk.x)
    
    sup_disc <- left_join(sup_rest, matching_data %>% dplyr::select(vedidk, disc_ford, field), by = c("vedidk.x" = "vedidk")) %>% 
        distinct()
    sup_disc <- left_join(sup_disc, matching_data %>% dplyr::select(vedidk, disc_ford, field), by = c("vedidk.y" = "vedidk")) %>% 
        distinct()
    
    sup_filtered <- sup_disc %>% filter(disc_ford.x == disc_ford.y) %>% 
        dplyr::select(-disc_ford.y, -field.x, -field.y) %>% 
        rename(disc_ford = disc_ford.x)    
    
    ### tady pozor! funkce v řádku nahoře vyhodí jeden případ kdy je výzkumník napárován se dvěma lidmi kteří mají stejné jméno jako jeho supervisor, dokonce i stejný obor, ale jiný vedidk - to je potřeba očistit!
    
    dup_matches_disc <- sup_filtered %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    sup_filtered <- anti_join(sup_filtered, dup_matches_disc, by = "vedidk.x")
    
    sup_total <- rbind(sup_total, sup_filtered) #this shows we were able to match 248 out of 259 supervisors vedidks - still eleven to go!
    
    
    
    
    ##filtering by broader discipline - field of study
    
    sup_rest_field <- anti_join(sup_disc, sup_filtered, by = "vedidk.x")
    
    # sup_rest_field <- left_join(sup_rest_field, matching_data %>% dplyr::select(disc_ford, field) %>% distinct(), by = c("disc_ford.x" = "disc_ford")) %>% 
    #     distinct()
    
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
    
    
    ##at this point there is still 232-218 = 14 researchers macthed to multiple supervisors
    
   
    # disambiguation based on supervisor being older than supervisee --> based on the results, this didnt work that well
    
    sup_rest_final <-  anti_join(sup_rest_field, sup_filtered_field, by = "vedidk.x") #už je jich jenom 6 a mám napsané v poznánmkách skoro u všech kteří supervisoři to jsou, tak to můžu asi nakódovat manuálně
    
    
    sup_rest_final$vedidk.x <- as.character(sup_rest_final$vedidk.x)
    sup_rest_final <- left_join(sup_rest_final, matching_data %>% dplyr::select(vedidk, career_start_year), by = c("vedidk.x" = "vedidk")) %>%
        distinct()

    sup_rest_final <- left_join(sup_rest_final, matching_data %>% dplyr::select(vedidk, career_start_year), by = c("vedidk.y" = "vedidk")) %>%
        distinct()

    sup_rest_final_filtered <- sup_rest_final %>% 
        filter(career_start_year.y < career_start_year.x) %>% 
        filter(field.x == field.y) 
    
    
    dup_matches_age <- sup_rest_final_filtered %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    sup_rest_final_filtered <- anti_join(sup_rest_final_filtered, dup_matches_age, by = "vedidk.x")
    
    sup_rest_final_filtered <-  sup_rest_final_filtered %>% 
        dplyr::select(-disc_ford.y, -field.x, -field.y, -career_start_year.x, -career_start_year.y) %>% 
        rename(disc_ford = disc_ford.x)
    
    
    sup_total <- rbind(sup_total, sup_rest_final_filtered) 
    
    
    ## now I need to disambiguate only 10 left researchers
    
    sup_unmatched <-  anti_join(sup_rest_final, sup_rest_final_filtered, by = "vedidk.x") #už je jich jenom 6 a mám napsané v poznánmkách skoro u všech kteří supervisoři to jsou, tak to můžu asi nakódovat manuálně
    
    sup_unmatched_coauth <- semi_join(sup_unmatched, dup_matches, by = "vedidk.y") %>% 
        filter(career_start_year.y < career_start_year.x) %>% 
        filter(field.x == field.y) 
    
    sup_unmatched_coauth <- sup_unmatched_coauth %>% 
        dplyr::select(-disc_ford.y, -field.x, -field.y, -career_start_year.x, -career_start_year.y) %>% 
        rename(disc_ford = disc_ford.x)
    
    sup_unmatched <- anti_join(sup_unmatched, sup_unmatched_coauth, by = "vedidk.x")
    
    sup_unmatched_age <- sup_unmatched %>% 
        filter(career_start_year.y < career_start_year.x)
    
    duo_unmatched_age <- sup_unmatched_age %>%
        group_by(vedidk.x) %>%
        filter(n() > 1) %>%
        ungroup()
    
    sup_unmatched_age <- anti_join(sup_unmatched_age, duo_unmatched_age, by = "vedidk.x")
    sup_unmatched_age <- sup_unmatched_age %>% 
        dplyr::select(-disc_ford.y, -field.x, -field.y, -career_start_year.x, -career_start_year.y) %>% 
        rename(disc_ford = disc_ford.x)
    
    sup_unmatched <- anti_join(sup_unmatched, sup_unmatched_age, by = "vedidk.x")
    
    
    sup_unmatched_disc <- sup_unmatched %>% 
        filter(disc_ford.x == disc_ford.y) %>% 
        filter(career_start_year.y < career_start_year.x) %>% 
        dplyr::select(-disc_ford.y, -field.x, -field.y, -career_start_year.x, -career_start_year.y) %>% 
        rename(disc_ford = disc_ford.x)
        
    
    sup_total <- rbind(sup_total, sup_unmatched_coauth) 
    sup_total <- rbind(sup_total, sup_unmatched_age) 
    sup_total <- rbind(sup_total, sup_unmatched_disc) 

    sup_unmatched <- anti_join(sup_unmatched, sup_unmatched_disc, by = "vedidk.x")
    
    
    sup_unmatched_final <- sup_unmatched %>% filter(vedidk.y %in% c(8318573)) %>% 
        dplyr::select(-disc_ford.y, -field.x, -field.y, -career_start_year.x, -career_start_year.y) %>% 
        rename(disc_ford = disc_ford.x) %>% 
        distinct()
    
    
    sup_total <- rbind(sup_total, sup_unmatched_final)
    
    ## I have ended up with 5 people unidentified
    
    ##maybe test how the new vedidk comapre to old vedidks?
    
    sup_total_diff <- sup_total %>%
        filter(sup_vedidk != vedidk.y)
    
    sup_total_final <- sup_total %>% 
        dplyr::select(-sup_vedidk, -statni.prislusnost, -sup_name_first, -sup_name_last) %>% 
        rename(vedidk = vedidk.x) %>% 
        rename(sup_vedidk = vedidk.y) %>% 
        rename(grant_disc_ford = disc_ford) %>% 
        rename(sup_name = id_helper)
        
    sup_total_final
    
}
