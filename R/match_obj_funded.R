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
match_obj_funded <- function(db_path, authors_arrow, matching_data) {

    authors_arrow <- as.data.frame(authors_arrow)
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    dbWriteTable(con, "authors_by_pubs", authors_arrow, overwrite = TRUE)
    on.exit(DBI::dbDisconnect(con))
    
    DBI::dbListTables(con)
    
    # setting up the set of all authors
    
    all_auth <- dplyr::tbl(con, "authors_by_pubs") %>% #colnames()
        dplyr::select(vedidk, id_unique) %>% 
        distinct() %>% 
        dplyr::collect() %>% 
        as_tibble()
    
    
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
    
    
  
    
    
    #final data 2020 -> only funded
    
    final_data_funded <- left_join(matching_data, pi_final, by = "vedidk")
    final_data_funded <- as_tibble(na.omit(final_data_funded))
    sum(is.na(final_data_funded))
    
    
    
    out_funded <- matchit(treatment~length+pubs_total+ws_pubs+interdisc_proportion+grants+first_grant+gender, method="nearest", data=final_data_funded, distance = "mahalanobis", ratio = 1, exact = c("disc_ford", "treatment_year"), replace = TRUE)
    
   
   
}
