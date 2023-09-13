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
match_data_prep <- function(db_path, ids, gender, sup_control, sup_control_second) {

    
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    
    # setting up the set of all authors
    
    all_auth <- dplyr::tbl(con, "authors_by_pubs") %>% #colnames()
        dplyr::select(vedidk, id_unique) %>% 
        distinct() %>% 
        dplyr::collect() %>% 
        as_tibble()
    
    # # org affiliation
    # 
    # auth_affiliation <- DBI::dbReadTable(con, "riv_details") %>% 
    #     select(kod, id_organization)
    
    ##grants starting 2015
    #number of all publications up to 2014 (ie the year when the grant was awarded
    
    
    n_pubs_types2014 <- dplyr::tbl(con, "riv_disc") %>%  #colnames()  
        dplyr::select(id_unique, pub_type, year) %>% 
        filter(pub_type %in% c("J","B","C","D")) %>%
        filter(year < 2014) %>% 
        as_tibble() 
    
    control_id <- n_pubs_types2014$id_unique
    n_pubs_filtered2014 <- dplyr::tbl(con, "authors_by_pubs") %>% 
        dplyr::select(vedidk, id_unique) %>% 
        filter(id_unique %in% control_id) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        dplyr::collect() %>% 
        as_tibble()
    
    n_pubs_count2014 <- plyr::count(n_pubs_filtered2014, vars = "vedidk") %>% 
        as_tibble()
    
    
    #number of wos/scopus publications up to 2014 (ie the year when the grant was awarded)
    
    control_id <- n_pubs_types2014$id_unique
    n_pubs_ws2014 <- dplyr::tbl(con, "riv_disc") %>% 
        filter(id_unique %in% control_id) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        dplyr::collect() %>% 
        as_tibble()
    
    n_pubs_ws2014 <- left_join(n_pubs_ws2014, n_pubs_filtered2014, by = "id_unique") 
    
    n_pubs_ws2014 <- n_pubs_ws2014 %>% 
        filter(!is.na(vedidk))
        
    n_pubs_ws2014 <- plyr::count(n_pubs_ws2014, vars = "vedidk") %>% 
        dplyr::rename(ws_pubs = freq) %>% 
        as_tibble()
    
  
    # number of coauthors 2014
    
    
    coauthors_by_pubs2014 <- n_pubs_filtered2014 %>% 
        group_by(id_unique) %>%
        summarise(coauthor_count = n_distinct(vedidk)) %>%
        ungroup()
    
    
    coauth2014 <- DBI::dbReadTable(con, "authors_by_pubs") %>%
        left_join(coauthors_by_pubs2014, by = "id_unique") %>%
        filter(!is.na(coauthor_count)) %>% 
        group_by(vedidk) %>%
        summarise(total_coauthor_count = sum(coauthor_count, na.rm = TRUE) - n_distinct(id_unique)) %>%
        ungroup()
    
    
    
    
    
    #gender
    
    all_auth_names <- dplyr::tbl(con, "authors_by_pubs") %>% #colnames()
        dplyr::select(vedidk, name_first, name_last) %>% 
        filter(!is.na(vedidk)) %>% 
        distinct() %>% 
        group_by(vedidk) %>% 
        dplyr::count(name_first) %>% 
        filter(n == max(n)) %>%
        slice_sample(n=1) %>%
        ungroup() %>% 
        dplyr::collect() %>% 
        as_tibble()
    
    all_auth_names$name_first <- tolower(all_auth_names$name_first)
    all_auth_names$name_first <- stringi::stri_trans_general(str = all_auth_names$name_first, id = "Latin-ASCII")
        
    all_auth_names$name_first <- as.character(all_auth_names$name_first)
    gender$name_first <- as.character(gender$name_first)
    gender$name <- as.character(gender$name)
    
    gender2 <- gender %>% 
        dplyr::select(name, gender) %>%
        filter(!is.na(gender)) %>% 
        group_by(name) %>% 
        dplyr::count(gender) %>% 
        filter(n == max(n)) %>%
        slice_sample(n=1) %>%
        ungroup() %>% 
        as_tibble()
    
    
    all_auth_gender <- left_join(all_auth_names, gender2, by = c("name_first" = "name"))
    
    all_auth_gender <- all_auth_gender %>%
        dplyr::select(vedidk, gender) %>% 
        filter(!is.na(gender))
    
    
    
    #career age in 2014
    
    age_pubs <- dplyr::tbl(con, "riv_disc") %>% 
        dplyr::select(id_unique, year) %>% 
        distinct() %>% 
        as_tibble()
    
    age_data <- left_join(age_pubs, all_auth, by = "id_unique") %>% 
        distinct() 
    
    career_start <- age_data %>% 
        group_by(vedidk) %>% 
        slice(which.min(year)) %>% 
        ungroup() 
    
    career_start$id_unique <- NULL
    career_start$year <- as.numeric(career_start$year) 
    
    career_start$lenght2014 <- (2014-career_start$year)
    career_start$lenght2015 <- (2015-career_start$year)
    career_start$lenght2016 <- (2016-career_start$year)
    career_start$lenght2017 <- (2017-career_start$year)
    career_start$lenght2018 <- (2018-career_start$year)
    career_start$lenght2019 <- (2019-career_start$year)
    career_start$lenght2020 <- (2020-career_start$year)
    
    career_start2014 <- career_start %>% 
        filter(lenght2014 >= 0)
    
    
    
    #discipline
    
    discipline_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()
        dplyr::select(id_unique, pub_type, ford, field, year) %>%
        filter(pub_type %in% c("J","B","C","D")) %>%
        # tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
        as_tibble()
    
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("BA", "BB")] <- 10100 #BD
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("IN", "BC", "BD", "AF")] <- 10200
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("BE", "BM", "BF", "BG", "BK", "BL", "BH", "BI","BN")] <- 10300
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("CC", "CA", "CH", "CF", "CD", "CG", "CB")] <- 10400
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DA", "DB", "DC", "DE", "DG", "DO", "DK", "DL", "DM", "DI", "DJ")] <- 10500
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EA", "EB", "EE", "CE", "EB", "BO", "EF", "EG", "DA", "EH")] <- 10600
    # 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JN", "JM", "GB", "AL", "JO")] <- 20100
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JA", "JB", "JW", "JD", "JC")] <- 20200
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JR", "JT", "JQ", "BJ", "JU", "JV", "JF", "JS", "JL")] <- 20300
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("CI")] <- 20400
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JP", "JG", "JJ", "JH", "JI", "JK")] <- 20500
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FS")] <- 20600
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DH", "JE", "JT", "JP", "JQ")] <- 20700
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI")] <- 20800
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI")] <- 20900
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JJ")] <- 21000
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GM", "JY", "KA")] <- 21100
    # 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EB", "EC", "FH", "FR", "ED")] <- 30100 #FP
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FA", "FB", "FC", "FD", "FF", "FG", "FI", "FJ", "FK", "FL", "FO", "FE")] <- 30200 #not added FH, FP
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FN", "FM", "DN", "AQ", "AK")] <- 30300 #FL, FP 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI")] <- 30400
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FP")] <- 30500
    # 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GD", "GK", "GL", "DF", "GE", "GF", "GC")] <- 40100 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GG", "GH", "GI")] <- 40200
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GJ")] <- 40300 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI", "GM")] <- 40400
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GM")] <- 40500
    # 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AN")] <- 50100 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AH", "GA")] <- 50200
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AM")] <- 50300 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AO", "AC")] <- 50400
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AG")] <- 50500
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AD", "AE")] <- 50600
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DE", "AP")] <- 50700 #AO
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AJ", "AF")] <- 50800
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AK")] <- 50900
    # 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AB", "AC")] <- 60100 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AI", "AJ")] <- 60200
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AA")] <- 60300 
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AL")] <- 60400
    # 
    # #additional cleaning:
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Cf")] <- 10400
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Al")] <- 60400
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Am")] <- 50300
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DD")] <- 10500
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("E")] <- 10600
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FQ")] <- 30200
    # discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Gk")] <- 40100
    # 
    # discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "HD")
    # discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "XX")
    # discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "O5")
    # discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "O6")
    # discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "J")
    # discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "O9")
    # discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "")
    # 
    # plyr::count(discipline_pubs$disc_ford)
    
    discipline_pubs <- dplyr::rename(discipline_pubs, disc_ford = ford)
    
    discipline_pubs_short <- discipline_pubs
    
    # discipline_pubs$disc_short <- substring(discipline_pubs$disc_ford, 0, 3)
    # discipline_pubs_short$disc_ford <- substring(discipline_pubs_short$disc_ford, 0, 3)
    
    discipline_data <- left_join(discipline_pubs_short, all_auth, by = "id_unique") %>% 
        distinct() 
    
    discipline_data$disc_ford <- as.character(discipline_data$disc_ford)
    
    
    plyr::count(discipline_data$disc_ford) 
    
    
    disciplines2014 <- discipline_data %>% 
        dplyr::select(disc_ford, field, vedidk, year) %>% 
        filter(!is.na(disc_ford)) %>% 
        filter(year < 2014) %>% 
        group_by(vedidk) %>% 
        slice(which.max(table(disc_ford))) %>% 
        ungroup() 
    
    plyr::count(disciplines2014$disc_ford)
    
    
    #interdisciplinarity
    
    
    interdisc <- discipline_data %>% 
        dplyr::select(disc_ford, vedidk) %>% 
        filter(!is.na(disc_ford))
    
    interdisc2014 <- left_join(interdisc, disciplines2014, by = "vedidk", suffix = c("_pub", "_main"))
    
    interdisc_final2014 <- interdisc2014 %>%  
        group_by(vedidk) %>% 
        filter(disc_ford_pub != disc_ford_main) %>% 
        dplyr::count(name = "interdisc") %>% 
        ungroup() 
    
    interdisc_final2014 <- left_join(n_pubs_count2014, interdisc_final2014, by = "vedidk")
    
    interdisc_final2014$interdisc <- as.character(interdisc_final2014$interdisc)
    interdisc_final2014$interdisc <- tidyr::replace_na(interdisc_final2014$interdisc, "0")
    
    interdisc_final2014$interdisc <- as.numeric(interdisc_final2014$interdisc)
    
    
    interdisc_final2014 <- interdisc_final2014 %>% 
        mutate(interdisc_proportion = interdisc/freq*100)
    
    plot(hist(interdisc_final2014$interdisc_proportion))
    
    sum(interdisc_final2014$interdisc_proportion == 0)
    
    interdisc_complete2014 <- interdisc_final2014 %>% 
        dplyr::select(vedidk, interdisc_proportion)
    
    
    
    #number of grants before 2014
    
    
    funding_vedidk <- DBI::dbReadTable(con, "cep_investigators") %>%
        filter(!is.na(vedidk)) %>% #pozor! více než 50 % projektů v databázi nemá přiřazený vedidk řešitele!
        # filter(role_researcher == G) %>% filtruje jen hlavní řešitele (G) a ne "další řešitele" (R)
        dplyr::select(kod, vedidk) %>% 
        distinct() %>% 
        as_tibble
    
    
    funding_year <- DBI::dbReadTable(con, "cep_details") %>% 
        # filter(program_kod == "GA") %>% 
        dplyr::select(kod, year_start) %>%
        distinct() %>% 
        as_tibble
    
    
    funding_data <- left_join(funding_vedidk, funding_year, by = "kod") %>% 
        distinct()
    
    funding_data$year_start <- ifelse(nchar(funding_data$year_start, type = "chars", allowNA = FALSE, keepNA = NA)>4, substring(funding_data$year_start, 7), funding_data$year_start)
    
    funding_data$year_start <- as.numeric(funding_data$year_start)
    
    
    grants2014 <- funding_data %>% 
        filter(year_start < 2014) %>% 
        group_by(vedidk) %>% 
        dplyr::count(name = "grants2014") %>% 
        ungroup() 
    
    
    
    ##calculating the first (career) year of receiving any grant
    
    
    pi_first_year <- funding_data %>% 
        dplyr::select(vedidk, year_start) %>% 
        filter(!is.na(vedidk)) %>% 
        filter(!is.na(year_start))  %>% 
        group_by(vedidk) %>% 
        slice(which.min(year_start)) %>% 
        ungroup() 
    
    
    pi_final <- left_join(career_start, pi_first_year, by = "vedidk")
    
    pi_final$year <- as.numeric(pi_final$year)
    pi_final$year_start <- as.numeric(pi_final$year_start)
    
    pi_final$first_grant <- (pi_final$year_start - pi_final$year)
    
    plyr::count(pi_final$first_grant<0) #seems that 4738 people have negative values, meaning that they received a grant earlier than they pirst published anything - is this plausible? Further, 36483 people did not receive any grant, so have NAs, which might b probematic in Propensity score matching (I think)    
    
    pi_final <- pi_final %>% 
        dplyr::select(vedidk, first_grant)
    
    
    ##calculating intervention group 2015 (i.e. assessed in 2014)
    
    focus_grants <- DBI::dbReadTable(con, "cep_details") %>% 
        filter(program_kod == "GJ") %>% 
        dplyr::select(kod, disc, ford, year_start) %>%
        tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
        distinct() %>% 
        as_tibble
    
    focus_grants$year_start <- ifelse(nchar(focus_grants$year_start, type = "chars", allowNA = FALSE, keepNA = NA)>4, substring(focus_grants$year_start, 7), focus_grants$year_start)
    
    focus_grants$year_start <- as.numeric(focus_grants$year_start)
    
    treatment_group <- DBI::dbReadTable(con, "cep_investigators") %>%
        dplyr::select(kod, vedidk) %>% 
        filter(kod %in% focus_grants$kod) %>% 
        distinct() %>% 
        as_tibble()
    
    nationality <- DBI::dbReadTable(con, "riv_authors") %>% 
        dplyr::select(vedidk, nationality)
    
    treatment_group <- left_join(treatment_group, nationality)
    
    ids_with_sup <- as_tibble(ids) %>% 
        mutate(across(everything(), ~replace(., . == "", NA))) %>% 
        filter(!is.na(sup_name_first)) %>%
        filter(!is.na(vedidk_core_researcher))
    
    treatment_refined <- treatment_group %>% 
        filter(!is.na(vedidk)) %>% 
        filter(nationality == "CZ") %>% 
        filter(vedidk %in% ids_with_sup$vedidk_core_researcher)
        
    # This show that although the database could find 845 distinct pairs of Junior grant code and vedidk, only in 356 cases there were actual vedidks, which means that in 845-356=489 cases there were pairs of existing code but missing vedidk 
    
    treatment_refined <- left_join(treatment_refined, focus_grants, by = "kod")
    
    treatment_refined$year_start <- ifelse(nchar(treatment_refined$year_start, type = "chars", allowNA = FALSE, keepNA = NA)>4, substring(treatment_refined$year_start, 7), treatment_refined$year_start)
    
    treatment_refined$year_start <- as.numeric(treatment_refined$year_start)
    
    tr <- treatment_refined %>%  #z tohoto plyne že z puvodnich 356 dedik kodu je jich jen 349 unikatnich, tzn 7 lidí bylo napsaných na více grantech
         dplyr::select(vedidk) %>%
         distinct()
    
    treatment_refined <-  treatment_refined %>%
        group_by(vedidk) %>%
        slice(which.min(year_start)) %>%
        ungroup()
    #tímto krokem vyfiltruju pryč vedidky, které dostly více juniorských grantů a vezmu v potaz jen jejich první grant (celkem se to týká 7 lidí)

    
    treatment_data <- all_auth %>% 
        dplyr::select(vedidk) %>% 
        mutate(treatment = ifelse(vedidk %in% treatment_refined$vedidk, 1, 0)) %>% 
        distinct() 
    
    treatment_data <- left_join(treatment_data, treatment_refined %>% dplyr::select(vedidk, year_start), by = "vedidk")
    
    treatment_data <- dplyr::rename(treatment_data, treatment_year = year_start)
       
    #?v tomto kroku z původních 349 unique treatment vedidků zbyde jen 329. Když místo n_pubs_count použiju dataset all_auth jako výchozí (což v praxi znamená, že vezmu v potaz i autory, kteří mají méně něž 5 publikací), tak mi to i tak klesne na 343, což nevím proč se děje  
    
    plyr::count(treatment_data$treatment) #this shows that it matched only 329 out of 356 treatment vedidks -> not sure why? - perhaps these are people who were assigned to a vedidk in CEP but then this didnt match up with vedidk they used in RIV?
    
    
    treatment_refined2015 <- treatment_data 
    treatment_refined2015$treatment_year <- as.character(treatment_refined2015$treatment_year)
    treatment_refined2015$treatment_year <- tidyr::replace_na(treatment_refined2015$treatment_year, "2015") 
    treatment_refined2015 <- treatment_refined2015 %>% 
        filter(!treatment_year > 2015)
    
    
    #final data 2015
    
    final_data2015 <- left_join(treatment_refined2015, career_start, by = "vedidk") %>% 
        filter(lenght2014 >= 0) %>%  #this step will delete all people who havent published anything yet before 2014 - not sure how sensible it is to keep them in the analysis, I can imagine that some grants were given even to people who havent published anything yet (and thus at 2014 had negative career years) 
        filter(!is.na(vedidk))
    
    final_data2015 <- left_join(final_data2015, disciplines2014, by = "vedidk")
    final_data2015 <- left_join(final_data2015, n_pubs_count2014, by = "vedidk")
    final_data2015 <- left_join(final_data2015, n_pubs_ws2014, by = "vedidk")
    final_data2015 <- left_join(final_data2015, interdisc_complete2014, by = "vedidk")
    final_data2015 <- left_join(final_data2015, grants2014, by = "vedidk")
    final_data2015 <- left_join(final_data2015, all_auth_gender, by = "vedidk")
    final_data2015 <- left_join(final_data2015, coauth2014, by = "vedidk")
    
    final_data2015$ws_pubs <- tidyr::replace_na(final_data2015$ws_pubs, 0) 
    final_data2015$grants2014 <- tidyr::replace_na(final_data2015$grants2014, 0) 
    final_data2015 <- dplyr::rename(final_data2015, career_start_year = year.x)
    final_data2015 <- dplyr::rename(final_data2015, pubs_total = freq)
    final_data2015$treatment_year <- as.numeric(final_data2015$treatment_year)
    final_data2015 <-  final_data2015 %>% 
        dplyr::select(vedidk, treatment, treatment_year, career_start_year, lenght2014, disc_ford, field, pubs_total, ws_pubs, interdisc_proportion, grants2014, gender, total_coauthor_count)
    
    
    # #excluding rows with missing values
    # final_data2015 <- as_tibble(na.omit(final_data2015)) #when I am using "gender" as a variable, it will delete 1100 vedidks -> maybe I dont want that? 
    # sum(is.na(final_data2015))
    

    
 
    
    
    
    
    
    
    
    ##grants starting 2016
    #number of all publications up to 2015 (ie the year when the grant was awarded
    
    
    n_pubs_types2015 <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
        dplyr::select(id_unique, pub_type, year) %>% 
        filter(pub_type %in% c("J","B","C","D")) %>%
        filter(year < 2015) %>% 
        as_tibble() 
    
    n_pubs_filtered2015 <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
        dplyr::select(vedidk, id_unique) %>% 
        filter(id_unique %in% n_pubs_types2015$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_count2015 <- plyr::count(n_pubs_filtered2015, vars = "vedidk") %>% 
        as_tibble()
    
    
    #number of wos/scopus publications up to 2015 (ie the year when the grant was awarded)
    
    n_pubs_ws2015 <- DBI::dbReadTable(con, "riv_disc") %>% 
        filter(id_unique %in% n_pubs_types2015$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_ws2015 <- left_join(n_pubs_ws2015, n_pubs_filtered2015, by = "id_unique") 
    
    n_pubs_ws2015 <- n_pubs_ws2015 %>% 
        filter(!is.na(vedidk))
    
    n_pubs_ws2015 <- plyr::count(n_pubs_ws2015, vars = "vedidk") %>% 
        as_tibble()
    
    n_pubs_ws2015 <- dplyr::rename(n_pubs_ws2015, ws_pubs = freq)
    
    
    
    # number of coauthors 2015
    
    
    coauthors_by_pubs2015 <- n_pubs_filtered2015 %>% 
        group_by(id_unique) %>%
        summarise(coauthor_count = n_distinct(vedidk)) %>%
        ungroup()
    
    
    coauth2015 <- DBI::dbReadTable(con, "authors_by_pubs") %>%
        left_join(coauthors_by_pubs2015, by = "id_unique") %>%
        filter(!is.na(coauthor_count)) %>% 
        group_by(vedidk) %>%
        summarise(total_coauthor_count = sum(coauthor_count, na.rm = TRUE) - n_distinct(id_unique)) %>%
        ungroup()
    
    
    
    
    #gender - copied from previous year
    
    
    #career age in 2015
    
    career_start2015 <- career_start %>% 
        filter(lenght2015 >= 0)
    
    
    
    #discipline
    
    disciplines2015 <- discipline_data %>% 
        dplyr::select(disc_ford, field, vedidk, year) %>% 
        filter(!is.na(disc_ford)) %>% 
        filter(year < 2015) %>% 
        group_by(vedidk) %>% 
        slice(which.max(table(disc_ford))) %>% 
        ungroup() 
    
    # plyr::count(disciplines2015$disc_ford)
    
    
    #interdisciplinarity
    
    
    interdisc2015 <- left_join(interdisc, disciplines2015, by = "vedidk", suffix = c("_pub", "_main"))
    
    interdisc_final2015 <- interdisc2015 %>%  
        group_by(vedidk) %>% 
        filter(disc_ford_pub != disc_ford_main) %>% 
        dplyr::count(name = "interdisc") %>% 
        ungroup() 
    
    interdisc_final2015 <- left_join(n_pubs_count2015, interdisc_final2015, by = "vedidk")
    
    interdisc_final2015$interdisc <- as.character(interdisc_final2015$interdisc)
    interdisc_final2015$interdisc <- tidyr::replace_na(interdisc_final2015$interdisc, "0")
    
    interdisc_final2015$interdisc <- as.numeric(interdisc_final2015$interdisc)
    
    
    interdisc_final2015 <- interdisc_final2015 %>% 
        mutate(interdisc_proportion = interdisc/freq*100)
    
    # plot(hist(interdisc_final2015$interdisc_proportion))
    # 
    # sum(interdisc_final2015$interdisc_proportion == 0)
    
    interdisc_complete2015 <- interdisc_final2015 %>% 
        dplyr::select(vedidk, interdisc_proportion)
    
    
    
    #number of grants before 2015
    
    
    grants2015 <- funding_data %>% 
        filter(year_start < 2015) %>% 
        group_by(vedidk) %>% 
        dplyr::count(name = "grants2015") %>% 
        ungroup() 
    
    
    
    ##calculating the first (career) year of receiving any grant
    
    #used from previous year
    
    
    
    ##calculating intervention group 2016 (i.e. assessed in 2015)
    
    
    treatment_refined2016 <- treatment_data 
    treatment_refined2016$treatment_year <- as.character(treatment_refined2016$treatment_year)
    treatment_refined2016$treatment_year <- tidyr::replace_na(treatment_refined2016$treatment_year, "2016") 
    treatment_refined2016 <- treatment_refined2016 %>% 
        filter(treatment_year == 2016)
    
    
    #final data 2016
    
    final_data2016 <- left_join(treatment_refined2016, career_start, by = "vedidk") %>% 
        filter(lenght2015 >= 0) %>%  #this step will delete all people who havent published anything yet before 2015 - not sure how sensible it is to keep them in the analysis, I can imagine that some grants were given even to people who havent published anything yet (and thus at 2015 had negative career years) 
        filter(!is.na(vedidk))
    
    final_data2016 <- left_join(final_data2016, disciplines2015, by = "vedidk")
    final_data2016 <- left_join(final_data2016, n_pubs_count2015, by = "vedidk")
    final_data2016 <- left_join(final_data2016, n_pubs_ws2015, by = "vedidk")
    final_data2016 <- left_join(final_data2016, interdisc_complete2015, by = "vedidk")
    final_data2016 <- left_join(final_data2016, grants2015, by = "vedidk")
    final_data2016 <- left_join(final_data2016, all_auth_gender, by = "vedidk")
    final_data2016 <- left_join(final_data2016, coauth2015, by = "vedidk")
    
    final_data2016$ws_pubs <- tidyr::replace_na(final_data2016$ws_pubs, 0) 
    final_data2016$grants2015 <- tidyr::replace_na(final_data2016$grants2015, 0) 
    final_data2016 <- dplyr::rename(final_data2016, career_start_year = year.x)
    final_data2016 <- dplyr::rename(final_data2016, pubs_total = freq)
    final_data2016$treatment_year <- as.numeric(final_data2016$treatment_year)
    final_data2016 <-  final_data2016 %>% 
        dplyr::select(vedidk, treatment, treatment_year, career_start_year, lenght2015, disc_ford, field, pubs_total, ws_pubs, interdisc_proportion, grants2015, gender, total_coauthor_count)
    
    
    # #excluding rows with missing values
    # final_data2016 <- as_tibble(na.omit(final_data2016)) #when I am using "gender" as a variable, it will delete 1100 vedidks -> maybe I dont want that? 
    # sum(is.na(final_data2016))
    
   
    
    
    
    
    
    
    
    ##grants starting 2017
    #number of all publications up to 2016 (ie the year when the grant was awarded
    
    
    n_pubs_types2016 <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
        dplyr::select(id_unique, pub_type, year) %>% 
        filter(pub_type %in% c("J","B","C","D")) %>%
        filter(year < 2016) %>% 
        as_tibble() 
    
    n_pubs_filtered2016 <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
        dplyr::select(vedidk, id_unique) %>% 
        filter(id_unique %in% n_pubs_types2016$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_count2016 <- plyr::count(n_pubs_filtered2016, vars = "vedidk") %>% 
        as_tibble()
    
    
    #number of wos/scopus publications up to 2016 (ie the year when the grant was awarded)
    
    n_pubs_ws2016 <- DBI::dbReadTable(con, "riv_disc") %>% 
        filter(id_unique %in% n_pubs_types2016$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_ws2016 <- left_join(n_pubs_ws2016, n_pubs_filtered2016, by = "id_unique") 
    
    n_pubs_ws2016 <- n_pubs_ws2016 %>% 
        filter(!is.na(vedidk))
    
    n_pubs_ws2016 <- plyr::count(n_pubs_ws2016, vars = "vedidk") %>% 
        as_tibble()
    
    n_pubs_ws2016 <- dplyr::rename(n_pubs_ws2016, ws_pubs = freq)
    
    
    
    
    # number of coauthors 2016
    
    
    coauthors_by_pubs2016 <- n_pubs_filtered2016 %>% 
        group_by(id_unique) %>%
        summarise(coauthor_count = n_distinct(vedidk)) %>%
        ungroup()
    
    
    coauth2016 <- DBI::dbReadTable(con, "authors_by_pubs") %>%
        left_join(coauthors_by_pubs2016, by = "id_unique") %>%
        filter(!is.na(coauthor_count)) %>% 
        group_by(vedidk) %>%
        summarise(total_coauthor_count = sum(coauthor_count, na.rm = TRUE) - n_distinct(id_unique)) %>%
        ungroup()
    
    
    
    
    
    #gender - copied from previous year
    
    
    #career age in 2016
    
    career_start2016 <- career_start %>% 
        filter(lenght2016 >= 0)
    
    
    
    #discipline
    
    disciplines2016 <- discipline_data %>% 
        dplyr::select(disc_ford, field, vedidk, year) %>% 
        filter(!is.na(disc_ford)) %>% 
        filter(year < 2016) %>% 
        group_by(vedidk) %>% 
        slice(which.max(table(disc_ford))) %>% 
        ungroup() 
    
    # plyr::count(disciplines2016$disc_ford)
    
    
    #interdisciplinarity
    
    
    interdisc2016 <- left_join(interdisc, disciplines2016, by = "vedidk", suffix = c("_pub", "_main"))
    
    interdisc_final2016 <- interdisc2016 %>%  
        group_by(vedidk) %>% 
        filter(disc_ford_pub != disc_ford_main) %>% 
        dplyr::count(name = "interdisc") %>% 
        ungroup() 
    
    interdisc_final2016 <- left_join(n_pubs_count2016, interdisc_final2016, by = "vedidk")
    
    interdisc_final2016$interdisc <- as.character(interdisc_final2016$interdisc)
    interdisc_final2016$interdisc <- tidyr::replace_na(interdisc_final2016$interdisc, "0")
    
    interdisc_final2016$interdisc <- as.numeric(interdisc_final2016$interdisc)
    
    
    interdisc_final2016 <- interdisc_final2016 %>% 
        mutate(interdisc_proportion = interdisc/freq*100)
    
    # plot(hist(interdisc_final2016$interdisc_proportion))
    # 
    # sum(interdisc_final2016$interdisc_proportion == 0)
    
    interdisc_complete2016 <- interdisc_final2016 %>% 
        dplyr::select(vedidk, interdisc_proportion)
    
    
    
    #number of grants before 2016
    
    
    grants2016 <- funding_data %>% 
        filter(year_start < 2016) %>% 
        group_by(vedidk) %>% 
        dplyr::count(name = "grants2016") %>% 
        ungroup() 
    
    
    
    ##calculating the first (career) year of receiving any grant
    
    #used from previous year
    
    
    
    ##calculating intervention group 2017 (i.e. assessed in 2016)
    
    
    treatment_refined2017 <- treatment_data 
    treatment_refined2017$treatment_year <- as.character(treatment_refined2017$treatment_year)
    treatment_refined2017$treatment_year <- tidyr::replace_na(treatment_refined2017$treatment_year, "2017") 
    treatment_refined2017 <- treatment_refined2017 %>% 
        filter(treatment_year == 2017)
    
    
    #final data 2017
    
    final_data2017 <- left_join(treatment_refined2017, career_start, by = "vedidk") %>% 
        filter(lenght2016 >= 0) %>%  #this step will delete all people who havent published anything yet before 2016 - not sure how sensible it is to keep them in the analysis, I can imagine that some grants were given even to people who havent published anything yet (and thus at 2016 had negative career years) 
        filter(!is.na(vedidk))
    
    final_data2017 <- left_join(final_data2017, disciplines2016, by = "vedidk")
    final_data2017 <- left_join(final_data2017, n_pubs_count2016, by = "vedidk")
    final_data2017 <- left_join(final_data2017, n_pubs_ws2016, by = "vedidk")
    final_data2017 <- left_join(final_data2017, interdisc_complete2016, by = "vedidk")
    final_data2017 <- left_join(final_data2017, grants2016, by = "vedidk")
    final_data2017 <- left_join(final_data2017, all_auth_gender, by = "vedidk")
    final_data2017 <- left_join(final_data2017, coauth2016, by = "vedidk")
    
    final_data2017$ws_pubs <- tidyr::replace_na(final_data2017$ws_pubs, 0) 
    final_data2017$grants2016 <- tidyr::replace_na(final_data2017$grants2016, 0) 
    final_data2017 <- dplyr::rename(final_data2017, career_start_year = year.x)
    final_data2017 <- dplyr::rename(final_data2017, pubs_total = freq)
    final_data2017$treatment_year <- as.numeric(final_data2017$treatment_year)
    final_data2017 <-  final_data2017 %>% 
        dplyr::select(vedidk, treatment, treatment_year, career_start_year, lenght2016, disc_ford, field, pubs_total, ws_pubs, interdisc_proportion, grants2016, gender, total_coauthor_count)
    
    
    # #excluding rows with missing values
    # final_data2017 <- as_tibble(na.omit(final_data2017)) #when I am using "gender" as a variable, it will delete 1100 vedidks -> maybe I dont want that? 
    # sum(is.na(final_data2017))
    
    
    
    
    
    
    
    
    
    ##grants starting 2018
    #number of all publications up to 2017 (ie the year when the grant was awarded
    
    
    n_pubs_types2017 <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
        dplyr::select(id_unique, pub_type, year) %>% 
        filter(pub_type %in% c("J","B","C","D")) %>%
        filter(year < 2017) %>% 
        as_tibble() 
    
    n_pubs_filtered2017 <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
        dplyr::select(vedidk, id_unique) %>% 
        filter(id_unique %in% n_pubs_types2017$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_count2017 <- plyr::count(n_pubs_filtered2017, vars = "vedidk") %>% 
        as_tibble()
    
    
    #number of wos/scopus publications up to 2017 (ie the year when the grant was awarded)
    
    n_pubs_ws2017 <- DBI::dbReadTable(con, "riv_disc") %>% 
        filter(id_unique %in% n_pubs_types2017$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_ws2017 <- left_join(n_pubs_ws2017, n_pubs_filtered2017, by = "id_unique") 
    
    n_pubs_ws2017 <- n_pubs_ws2017 %>% 
        filter(!is.na(vedidk))
    
    n_pubs_ws2017 <- plyr::count(n_pubs_ws2017, vars = "vedidk") %>% 
        as_tibble()
    
    n_pubs_ws2017 <- dplyr::rename(n_pubs_ws2017, ws_pubs = freq)
    
    
    
    # number of coauthors 2017
    
    
    coauthors_by_pubs2017 <- n_pubs_filtered2017 %>% 
        group_by(id_unique) %>%
        summarise(coauthor_count = n_distinct(vedidk)) %>%
        ungroup()
    
    
    coauth2017 <- DBI::dbReadTable(con, "authors_by_pubs") %>%
        left_join(coauthors_by_pubs2017, by = "id_unique") %>%
        filter(!is.na(coauthor_count)) %>% 
        group_by(vedidk) %>%
        summarise(total_coauthor_count = sum(coauthor_count, na.rm = TRUE) - n_distinct(id_unique)) %>%
        ungroup()
    
    
    
    #gender - copied from previous year
    
    
    #career age in 2017
    
    career_start2017 <- career_start %>% 
        filter(lenght2017 >= 0)
    
    
    
    #discipline
    
    disciplines2017 <- discipline_data %>% 
        dplyr::select(disc_ford, field, vedidk, year) %>% 
        filter(!is.na(disc_ford)) %>% 
        filter(year < 2017) %>% 
        group_by(vedidk) %>% 
        slice(which.max(table(disc_ford))) %>% 
        ungroup() 
    
    # plyr::count(disciplines2017$disc_ford)
    
    
    #interdisciplinarity
    
    
    interdisc2017 <- left_join(interdisc, disciplines2017, by = "vedidk", suffix = c("_pub", "_main"))
    
    interdisc_final2017 <- interdisc2017 %>%  
        group_by(vedidk) %>% 
        filter(disc_ford_pub != disc_ford_main) %>% 
        dplyr::count(name = "interdisc") %>% 
        ungroup() 
    
    interdisc_final2017 <- left_join(n_pubs_count2017, interdisc_final2017, by = "vedidk")
    
    interdisc_final2017$interdisc <- as.character(interdisc_final2017$interdisc)
    interdisc_final2017$interdisc <- tidyr::replace_na(interdisc_final2017$interdisc, "0")
    
    interdisc_final2017$interdisc <- as.numeric(interdisc_final2017$interdisc)
    
    
    interdisc_final2017 <- interdisc_final2017 %>% 
        mutate(interdisc_proportion = interdisc/freq*100)
    
    # plot(hist(interdisc_final2017$interdisc_proportion))
    # 
    # sum(interdisc_final2017$interdisc_proportion == 0)
    
    interdisc_complete2017 <- interdisc_final2017 %>% 
        dplyr::select(vedidk, interdisc_proportion)
    
    
    
    #number of grants before 2017
    
    
    grants2017 <- funding_data %>% 
        filter(year_start < 2017) %>% 
        group_by(vedidk) %>% 
        dplyr::count(name = "grants2017") %>% 
        ungroup() 
    
    
    
    ##calculating the first (career) year of receiving any grant
    
    #used from previous year
    
    
    
    ##calculating intervention group 2018 (i.e. assessed in 2017)
    
    
    treatment_refined2018 <- treatment_data 
    treatment_refined2018$treatment_year <- as.character(treatment_refined2018$treatment_year)
    treatment_refined2018$treatment_year <- tidyr::replace_na(treatment_refined2018$treatment_year, "2018") 
    treatment_refined2018 <- treatment_refined2018 %>% 
        filter(treatment_year == 2018)
    
    
    #final data 2018
    
    final_data2018 <- left_join(treatment_refined2018, career_start, by = "vedidk") %>% 
        filter(lenght2017 >= 0) %>%  #this step will delete all people who havent published anything yet before 2017 - not sure how sensible it is to keep them in the analysis, I can imagine that some grants were given even to people who havent published anything yet (and thus at 2017 had negative career years) 
        filter(!is.na(vedidk))
    
    final_data2018 <- left_join(final_data2018, disciplines2017, by = "vedidk")
    final_data2018 <- left_join(final_data2018, n_pubs_count2017, by = "vedidk")
    final_data2018 <- left_join(final_data2018, n_pubs_ws2017, by = "vedidk")
    final_data2018 <- left_join(final_data2018, interdisc_complete2017, by = "vedidk")
    final_data2018 <- left_join(final_data2018, grants2017, by = "vedidk")
    final_data2018 <- left_join(final_data2018, all_auth_gender, by = "vedidk")
    final_data2018 <- left_join(final_data2018, coauth2017, by = "vedidk")
    
    final_data2018$ws_pubs <- tidyr::replace_na(final_data2018$ws_pubs, 0) 
    final_data2018$grants2017 <- tidyr::replace_na(final_data2018$grants2017, 0) 
    final_data2018 <- dplyr::rename(final_data2018, career_start_year = year.x)
    final_data2018 <- dplyr::rename(final_data2018, pubs_total = freq)
    final_data2018$treatment_year <- as.numeric(final_data2018$treatment_year)
    final_data2018 <-  final_data2018 %>% 
        dplyr::select(vedidk, treatment, treatment_year, career_start_year, lenght2017, disc_ford, field, pubs_total, ws_pubs, interdisc_proportion, grants2017, gender, total_coauthor_count)
    
    
    # #excluding rows with missing values
    # final_data2018 <- as_tibble(na.omit(final_data2018)) #when I am using "gender" as a variable, it will delete 1100 vedidks -> maybe I dont want that? 
    # sum(is.na(final_data2018))
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ##grants starting 2019
    #number of all publications up to 2018 (ie the year when the grant was awarded
    
    
    n_pubs_types2018 <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
        dplyr::select(id_unique, pub_type, year) %>% 
        filter(pub_type %in% c("J","B","C","D")) %>%
        filter(year < 2018) %>% 
        as_tibble() 
    
    n_pubs_filtered2018 <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
        dplyr::select(vedidk, id_unique) %>% 
        filter(id_unique %in% n_pubs_types2018$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_count2018 <- plyr::count(n_pubs_filtered2018, vars = "vedidk") %>% 
        as_tibble()
    
    
    #number of wos/scopus publications up to 2018 (ie the year when the grant was awarded)
    
    n_pubs_ws2018 <- DBI::dbReadTable(con, "riv_disc") %>% 
        filter(id_unique %in% n_pubs_types2018$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_ws2018 <- left_join(n_pubs_ws2018, n_pubs_filtered2018, by = "id_unique") 
    
    n_pubs_ws2018 <- n_pubs_ws2018 %>% 
        filter(!is.na(vedidk))
    
    n_pubs_ws2018 <- plyr::count(n_pubs_ws2018, vars = "vedidk") %>% 
        as_tibble()
    
    n_pubs_ws2018 <- dplyr::rename(n_pubs_ws2018, ws_pubs = freq)
    
    
    
    # number of coauthors 2018
    
    
    coauthors_by_pubs2018 <- n_pubs_filtered2018 %>% 
        group_by(id_unique) %>%
        summarise(coauthor_count = n_distinct(vedidk)) %>%
        ungroup()
    
    
    coauth2018 <- DBI::dbReadTable(con, "authors_by_pubs") %>%
        left_join(coauthors_by_pubs2018, by = "id_unique") %>%
        filter(!is.na(coauthor_count)) %>% 
        group_by(vedidk) %>%
        summarise(total_coauthor_count = sum(coauthor_count, na.rm = TRUE) - n_distinct(id_unique)) %>%
        ungroup()
    
    
    
    
    #gender - copied from previous year
    
    
    #career age in 2018
    
    career_start2018 <- career_start %>% 
        filter(lenght2018 >= 0)
    
    
    
    #discipline
    
    disciplines2018 <- discipline_data %>% 
        dplyr::select(disc_ford, field, vedidk, year) %>% 
        filter(!is.na(disc_ford)) %>% 
        filter(year < 2018) %>% 
        group_by(vedidk) %>% 
        slice(which.max(table(disc_ford))) %>% 
        ungroup() 
    
    # plyr::count(disciplines2018$disc_ford)
    
    
    #interdisciplinarity
    
    
    interdisc2018 <- left_join(interdisc, disciplines2018, by = "vedidk", suffix = c("_pub", "_main"))
    
    interdisc_final2018 <- interdisc2018 %>%  
        group_by(vedidk) %>% 
        filter(disc_ford_pub != disc_ford_main) %>% 
        dplyr::count(name = "interdisc") %>% 
        ungroup() 
    
    interdisc_final2018 <- left_join(n_pubs_count2018, interdisc_final2018, by = "vedidk")
    
    interdisc_final2018$interdisc <- as.character(interdisc_final2018$interdisc)
    interdisc_final2018$interdisc <- tidyr::replace_na(interdisc_final2018$interdisc, "0")
    
    interdisc_final2018$interdisc <- as.numeric(interdisc_final2018$interdisc)
    
    
    interdisc_final2018 <- interdisc_final2018 %>% 
        mutate(interdisc_proportion = interdisc/freq*100)
    
    # plot(hist(interdisc_final2018$interdisc_proportion))
    # 
    # sum(interdisc_final2018$interdisc_proportion == 0)
    
    interdisc_complete2018 <- interdisc_final2018 %>% 
        dplyr::select(vedidk, interdisc_proportion)
    
    
    
    #number of grants before 2018
    
    
    grants2018 <- funding_data %>% 
        filter(year_start < 2018) %>% 
        group_by(vedidk) %>% 
        dplyr::count(name = "grants2018") %>% 
        ungroup() 
    
    
    
    ##calculating the first (career) year of receiving any grant
    
    #used from previous year
    
    
    
    ##calculating intervention group 2019 (i.e. assessed in 2018)
    
    
    treatment_refined2019 <- treatment_data 
    treatment_refined2019$treatment_year <- as.character(treatment_refined2019$treatment_year)
    treatment_refined2019$treatment_year <- tidyr::replace_na(treatment_refined2019$treatment_year, "2019") 
    treatment_refined2019 <- treatment_refined2019 %>% 
        filter(treatment_year == 2019)
    
    
    #final data 2019
    
    final_data2019 <- left_join(treatment_refined2019, career_start, by = "vedidk") %>% 
        filter(lenght2018 >= 0) %>%  #this step will delete all people who havent published anything yet before 2018 - not sure how sensible it is to keep them in the analysis, I can imagine that some grants were given even to people who havent published anything yet (and thus at 2018 had negative career years) 
        filter(!is.na(vedidk))
    
    final_data2019 <- left_join(final_data2019, disciplines2018, by = "vedidk")
    final_data2019 <- left_join(final_data2019, n_pubs_count2018, by = "vedidk")
    final_data2019 <- left_join(final_data2019, n_pubs_ws2018, by = "vedidk")
    final_data2019 <- left_join(final_data2019, interdisc_complete2018, by = "vedidk")
    final_data2019 <- left_join(final_data2019, grants2018, by = "vedidk")
    final_data2019 <- left_join(final_data2019, all_auth_gender, by = "vedidk")
    final_data2019 <- left_join(final_data2019, coauth2018, by = "vedidk")
    
    final_data2019$ws_pubs <- tidyr::replace_na(final_data2019$ws_pubs, 0) 
    final_data2019$grants2018 <- tidyr::replace_na(final_data2019$grants2018, 0) 
    final_data2019 <- dplyr::rename(final_data2019, career_start_year = year.x)
    final_data2019 <- dplyr::rename(final_data2019, pubs_total = freq)
    final_data2019$treatment_year <- as.numeric(final_data2019$treatment_year)
    final_data2019 <-  final_data2019 %>% 
        dplyr::select(vedidk, treatment, treatment_year, career_start_year, lenght2018, disc_ford, field, pubs_total, ws_pubs, interdisc_proportion, grants2018, gender, total_coauthor_count)
    
    
    # #excluding rows with missing values
    # final_data2019 <- as_tibble(na.omit(final_data2019)) #when I am using "gender" as a variable, it will delete 1100 vedidks -> maybe I dont want that? 
    # sum(is.na(final_data2019))
    

    
    
    
    
    
    ##grants starting 2020
    #number of all publications up to 2019 (ie the year when the grant was awarded
    
    
    n_pubs_types2019 <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
        dplyr::select(id_unique, pub_type, year) %>% 
        filter(pub_type %in% c("J","B","C","D")) %>%
        filter(year < 2019) %>% 
        as_tibble() 
    
    n_pubs_filtered2019 <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
        dplyr::select(vedidk, id_unique) %>% 
        filter(id_unique %in% n_pubs_types2019$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_count2019 <- plyr::count(n_pubs_filtered2019, vars = "vedidk") %>% 
        as_tibble()
    
    
    #number of wos/scopus publications up to 2019 (ie the year when the grant was awarded)
    
    n_pubs_ws2019 <- DBI::dbReadTable(con, "riv_disc") %>% 
        filter(id_unique %in% n_pubs_types2019$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
        distinct() %>% 
        as_tibble()
    
    n_pubs_ws2019 <- left_join(n_pubs_ws2019, n_pubs_filtered2019, by = "id_unique") 
    
    n_pubs_ws2019 <- n_pubs_ws2019 %>% 
        filter(!is.na(vedidk))
    
    n_pubs_ws2019 <- plyr::count(n_pubs_ws2019, vars = "vedidk") %>% 
        as_tibble()
    
    n_pubs_ws2019 <- dplyr::rename(n_pubs_ws2019, ws_pubs = freq)
    
    
    
    # number of coauthors 2019
    
    
    coauthors_by_pubs2019 <- n_pubs_filtered2019 %>% 
        group_by(id_unique) %>%
        summarise(coauthor_count = n_distinct(vedidk)) %>%
        ungroup()
    
    
    coauth2019 <- DBI::dbReadTable(con, "authors_by_pubs") %>%
        left_join(coauthors_by_pubs2019, by = "id_unique") %>%
        filter(!is.na(coauthor_count)) %>% 
        group_by(vedidk) %>%
        summarise(total_coauthor_count = sum(coauthor_count, na.rm = TRUE) - n_distinct(id_unique)) %>%
        ungroup()
    
    
    
    #gender - copied from previous year
    
    
    #career age in 2019
    
    career_start2019 <- career_start %>% 
        filter(lenght2019 >= 0)
    
    
    
    #discipline
    
    disciplines2019 <- discipline_data %>% 
        dplyr::select(disc_ford, field, vedidk, year) %>% 
        filter(!is.na(disc_ford)) %>% 
        filter(year < 2019) %>% 
        group_by(vedidk) %>% 
        slice(which.max(table(disc_ford))) %>% 
        ungroup() 
    
    # plyr::count(disciplines2019$disc_ford)
    
    
    #interdisciplinarity
    
    
    interdisc2019 <- left_join(interdisc, disciplines2019, by = "vedidk", suffix = c("_pub", "_main"))
    
    interdisc_final2019 <- interdisc2019 %>%  
        group_by(vedidk) %>% 
        filter(disc_ford_pub != disc_ford_main) %>% 
        dplyr::count(name = "interdisc") %>% 
        ungroup() 
    
    interdisc_final2019 <- left_join(n_pubs_count2019, interdisc_final2019, by = "vedidk")
    
    interdisc_final2019$interdisc <- as.character(interdisc_final2019$interdisc)
    interdisc_final2019$interdisc <- tidyr::replace_na(interdisc_final2019$interdisc, "0")
    
    interdisc_final2019$interdisc <- as.numeric(interdisc_final2019$interdisc)
    
    
    interdisc_final2019 <- interdisc_final2019 %>% 
        mutate(interdisc_proportion = interdisc/freq*100)
    
    # plot(hist(interdisc_final2019$interdisc_proportion))
    # 
    # sum(interdisc_final2019$interdisc_proportion == 0)
    
    interdisc_complete2019 <- interdisc_final2019 %>% 
        dplyr::select(vedidk, interdisc_proportion)
    
    
    
    #number of grants before 2019
    
    
    grants2019 <- funding_data %>% 
        filter(year_start < 2019) %>% 
        group_by(vedidk) %>% 
        dplyr::count(name = "grants2019") %>% 
        ungroup() 
    
    
    
    ##calculating the first (career) year of receiving any grant
    
    #used from previous year
    
    
    
    ##calculating intervention group 2020 (i.e. assessed in 2019)
    
    
    treatment_refined2020 <- treatment_data 
    treatment_refined2020$treatment_year <- as.character(treatment_refined2020$treatment_year)
    treatment_refined2020$treatment_year <- tidyr::replace_na(treatment_refined2020$treatment_year, "2020") 
    treatment_refined2020 <- treatment_refined2020 %>% 
        filter(treatment_year == 2020)
    
    
    #final data 2020
    
    final_data2020 <- left_join(treatment_refined2020, career_start, by = "vedidk") %>% 
        filter(lenght2019 >= 0) %>%  #this step will delete all people who havent published anything yet before 2019 - not sure how sensible it is to keep them in the analysis, I can imagine that some grants were given even to people who havent published anything yet (and thus at 2019 had negative career years) 
        filter(!is.na(vedidk))
    
    final_data2020 <- left_join(final_data2020, disciplines2019, by = "vedidk")
    final_data2020 <- left_join(final_data2020, n_pubs_count2019, by = "vedidk")
    final_data2020 <- left_join(final_data2020, n_pubs_ws2019, by = "vedidk")
    final_data2020 <- left_join(final_data2020, interdisc_complete2019, by = "vedidk")
    final_data2020 <- left_join(final_data2020, grants2019, by = "vedidk")
    final_data2020 <- left_join(final_data2020, all_auth_gender, by = "vedidk")
    final_data2020 <- left_join(final_data2020, coauth2019, by = "vedidk")
    
    final_data2020$ws_pubs <- tidyr::replace_na(final_data2020$ws_pubs, 0) 
    final_data2020$grants2019 <- tidyr::replace_na(final_data2020$grants2019, 0) 
    final_data2020 <- dplyr::rename(final_data2020, career_start_year = year.x)
    final_data2020 <- dplyr::rename(final_data2020, pubs_total = freq)
    final_data2020$treatment_year <- as.numeric(final_data2020$treatment_year)
    final_data2020 <-  final_data2020 %>% 
        dplyr::select(vedidk, treatment, treatment_year, career_start_year, lenght2019, disc_ford, field, pubs_total, ws_pubs, interdisc_proportion, grants2019, gender, total_coauthor_count)
    
    
    # #excluding rows with missing values
    # final_data2020 <- as_tibble(na.omit(final_data2020)) #when I am using "gender" as a variable, it will delete 1100 vedidks -> maybe I dont want that? 
    # sum(is.na(final_data2020))
    
    
    
    

    
    
    
    
    
    
    #create complete dataset of final data
    
    final_data2015 <- rename(final_data2015, length = lenght2014)
    final_data2016 <- rename(final_data2016, length = lenght2015)
    final_data2017 <- rename(final_data2017, length = lenght2016)
    final_data2018 <- rename(final_data2018, length = lenght2017)
    final_data2019 <- rename(final_data2019, length = lenght2018)
    final_data2020 <- rename(final_data2020, length = lenght2019)
    
    final_data2015 <- rename(final_data2015, grants = grants2014)
    final_data2016 <- rename(final_data2016, grants = grants2015)
    final_data2017 <- rename(final_data2017, grants = grants2016)
    final_data2018 <- rename(final_data2018, grants = grants2017)
    final_data2019 <- rename(final_data2019, grants = grants2018)
    final_data2020 <- rename(final_data2020, grants = grants2019)
    
    final_data <- rbind2(final_data2015, final_data2016)
    final_data <- rbind2(final_data, final_data2017)
    final_data <- rbind2(final_data, final_data2018)
    final_data <- rbind2(final_data, final_data2019)
    final_data <- rbind2(final_data, final_data2020)
    
    
    final_data$gender[final_data$vedidk == "3120899"] <- "male"
    final_data$gender[final_data$vedidk == "3972305"] <- "female"
    
    
    sup_out <- sup_control %>% filter(is.na(sup_name_first))
    sup_out_second <- sup_control_second %>% filter(is.na(sup_name_first))
    # 
    final_data <- final_data %>% filter(!vedidk %in% sup_out$vedidk)
    final_data <- final_data %>% filter(!vedidk %in% sup_out_second$vedidk)
    
    
    #excluding rows with missing values
    final_data <- as_tibble(na.omit(final_data)) #when I am using "gender" as a variable, it will delete 1100 vedidks -> maybe I dont want that?
    sum(is.na(final_data))
    
    
    final_data
    
    # sum(treatment_data$treatment)
    # sum(final_data$treatment)
    
    
    #filtering out observations from treatment group which we couldnt find supervisors for
    
    # ids_out <- as_tibble(ids) %>% 
    #     filter(is.na(sup_vedidk)) %>%
    #     filter(!is.na(vedidk_core_researcher))
    # 
    # ids_out_SK <- as_tibble(ids) %>% 
    #     filter(statni.prislusnost == "SK") %>% 
    #     filter(!is.na(vedidk_core_researcher))
    # 
    # ids_out <- rbind(ids_out, ids_out_SK)
    # 
    # full_data_final_clean <- final_data %>% 
    #     filter(!vedidk %in% ids_out$vedidk_core_researcher) %>%
    #     as_tibble()
    
    # table(full_data_final_clean$treatment, by = full_data_final_clean$independence_timing)
    
    # after removing those we coulnt find supúervisors for there is only 159 experimental observations 
    
    
   
   
}
