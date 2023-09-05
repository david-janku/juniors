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
manual_sup_search <- function(db_path, matching) {
    
    
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))



manual_sup_search <- matching %>% filter(treatment != 1) %>% dplyr::select(vedidk, disc_ford) %>% distinct()

all_auth <- dplyr::tbl(con, "authors_by_pubs") %>% 
    dplyr::collect() %>% 
    as_tibble()


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

manual_sup_search <- left_join(manual_sup_search, career_start %>% dplyr::select(vedidk, name_first, name_last, org_name), by = "vedidk")

manual_sup_search <- manual_sup_search %>% 
    mutate(theses =
               paste0("https://theses.cz/vyhledavani/?search=",
                      name_first, "+", name_last, "+disertační+práce"))
    

manual_sup_search <- manual_sup_search %>% 
    mutate(dspace_cuni =
               paste0("https://dspace.cuni.cz/discover?filtertype_1=author&filter_relational_operator_1=contains&filter_1=",
                      name_last, "%2C+", name_first))


manual_sup_search <- manual_sup_search %>% 
    mutate(google =
               paste0("https://www.google.com/search?q=",
                      name_first, "+", name_last, "+disertační+práce"))



sup_control2 <- read.csv2(here::here("data", "raw", "supervisors_control_final.csv")) 
sup_control2 <- sup_control2 %>% filter(!is.na(sup_vedidk)) %>% dplyr::select(vedidk, sup_name_first, sup_name_last, sup_vedidk, sec_sup_name_first, sec_sup_name_last, sec_sup_vedidk)
sup_control2$vedidk <- as.character(sup_control2$vedidk)


manual_sup_search <- left_join(manual_sup_search, sup_control2, by = "vedidk")


extra_search_sup <- anti_join(manual_sup_search, sup_control, by = "vedidk")


# write.csv2(extra_search_sup, file = here::here("data", "derived", "manual_sup_search.csv"))

sup <- read.csv2(here::here("data", "raw", "manual_sup_search.csv")) 


sup_out <- sup %>% filter(is.na(sup_name_first))


sup_complete <- as_tibble(sup) %>% 
    # select(vedidk_core_researcher, sup_vedidk, statni.prislusnost) %>% 
    filter(!is.na(sup_name_first)) %>% 
    filter(!is.na(vedidk))

sup_complete$vedidk <- as.character(sup_complete$vedidk)

extra_search_sup_second <- anti_join(extra_search_sup, sup_complete, by = "vedidk")


# write.csv2(extra_search_sup_second, file = here::here("data", "derived", "manual_sup_search_second.csv"))



# z <- semi_join(manual_sup_search, sup_control, by = "vedidk")
# z <- z %>% dplyr::select(-sup_vedidk) %>% mutate(dis_link = NA)
# z$vedidk <- as.integer(z$vedidk)
# 
# sup_all <- bind_rows(sup, z) %>% filter(!is.na(vedidk))
# 
# sup_all$sup_name_last[sup_all$vedidk == "9269746"] <- "Drozd"
# sup_all$dis_link[sup_all$vedidk == "9269746"] <- "https://theses.cz/id/ej7tbh/"
# 
# write.csv2(sup_all, file = here::here("data", "raw", "manual_sup_search.csv"))



## extra ids search

ids_complete <- ids_complete %>% filter(!is.na(sup_name)) %>%  dplyr::select(vedidk)

ids <- ids %>% mutate(across(everything(), ~replace(., . == "", NA))) %>% filter(!is.na(vedidk_core_researcher))

ids$vedidk_core_researcher <- as.character(ids$vedidk_core_researcher)

ids_in <- anti_join(ids, ids_complete, by = c("vedidk_core_researcher" = "vedidk")) %>% filter(statni.prislusnost == "CZ") %>% dplyr::select(vedidk_core_researcher) %>% rename(vedidk = vedidk_core_researcher) %>% distinct()

con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
on.exit(DBI::dbDisconnect(con))


all_auth <- dplyr::tbl(con, "authors_by_pubs") %>% #colnames()
    dplyr::select(vedidk, id_unique, name_first, name_last, org_name) %>% 
    distinct() %>% 
    dplyr::collect() %>% 
    as_tibble()

discipline_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()
    dplyr::select(id_unique, pub_type, ford, field, year) %>%
    filter(pub_type %in% c("J","B","C","D")) %>%
    # tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
    as_tibble()

discipline_pubs <- dplyr::rename(discipline_pubs, disc_ford = ford)

discipline_data <- left_join(discipline_pubs, all_auth, by = "id_unique") %>% 
    distinct() 

discipline_data$disc_ford <- as.character(discipline_data$disc_ford)


plyr::count(discipline_data$disc_ford) 


disciplines <- discipline_data %>% 
    dplyr::select(disc_ford, field, vedidk, year, org_name) %>% 
    filter(!is.na(disc_ford)) %>% 
    group_by(vedidk) %>% 
    slice(which.max(table(disc_ford))) %>% 
    ungroup() 


ids_in <- left_join(ids_in, disciplines %>% dplyr::select(vedidk, disc_ford) %>% distinct(), by = "vedidk")

inspect <- ids_in %>% filter(is.na(disc_ford)) #these are vedidks that didnt have any discipline in RIV, suggesting they didnt have any publication in RIV --> incorrect vedidks in CEP


career_start <- discipline_data %>% 
    group_by(vedidk) %>% 
    slice(which.min(year)) %>% 
    ungroup() 

ids_sup_search <- left_join(ids_in, career_start %>% dplyr::select(vedidk, name_first, name_last, org_name), by = "vedidk")

ids_sup_search <- ids_sup_search %>% 
    mutate(theses =
               paste0("https://theses.cz/vyhledavani/?search=",
                      name_first, "+", name_last, "+disertační+práce"))


ids_sup_search <- ids_sup_search %>% 
    mutate(dspace_cuni =
               paste0("https://dspace.cuni.cz/discover?filtertype_1=author&filter_relational_operator_1=contains&filter_1=",
                      name_last, "%2C+", name_first))


ids_sup_search <- ids_sup_search %>% 
    mutate(google =
               paste0("https://www.google.com/search?q=",
                      name_first, "+", name_last, "+disertační+práce"))


ids_sup_search$sup_name_first <- NA
ids_sup_search$sup_name_last <- NA
ids_sup_search$dis_link <- NA
ids_sup_search$sec_sup_name_first <- NA
ids_sup_search$sec_sup_name_last <- NA

# write.csv2(ids_sup_search, file = here::here("data", "derived", "sup_search_third.csv"))

ids_extra <- read.csv(here::here("data", "raw", "backup", "ids_sup_search.csv")) 

ids_extra$vedidk <- as.character(ids_extra$vedidk)

ids <- left_join(ids, ids_extra %>% dplyr::select(vedidk, sup_name_first, sup_name_last), by = c("vedidk_core_researcher" = "vedidk"))

a <- ids %>% mutate(across(everything(), ~replace(., . == "", NA))) %>% filter(!is.na(sup_name_first))
                             
combined <- ids %>%
  mutate(across(everything(), ~replace(., . == "", NA))) %>% 
  left_join(ids_extra %>% dplyr::select(vedidk, sup_name_first, sup_name_last), by = c("vedidk_core_researcher" = "vedidk"), suffix = c("", "_extra")) %>%
  mutate(
    sup_name_first = ifelse(is.na(sup_name_first), sup_name_first_extra, sup_name_first),
    sup_name_last = ifelse(is.na(sup_name_last), sup_name_last_extra, sup_name_last)
  ) %>%
  select(-sup_name_first_extra, -sup_name_last_extra)
  
b <- combined %>% mutate(across(everything(), ~replace(., . == "", NA))) %>% filter(!is.na(sup_name_first))


}