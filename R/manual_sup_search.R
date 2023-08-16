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



sup_control <- read.csv2(here::here("data", "raw", "supervisors_control_final.csv")) 
sup_control <- sup_control %>% filter(!is.na(sup_vedidk)) %>% dplyr::select(vedidk, sup_name_first, sup_name_last, sup_vedidk, sec_sup_name_first, sec_sup_name_last, sec_sup_vedidk)
sup_control$vedidk <- as.character(sup_control$vedidk)


manual_sup_search <- left_join(manual_sup_search, sup_control, by = "vedidk")


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




# kteří superrvisors neměli se svými studenty ani jednu publikaci? -- na ty se podívát více do hloubky

sup_vedidk <- dplyr::tbl(con, "authors_by_pubs") %>% 
    dplyr::select(name_first, name_last, id_helper, vedidk) %>% 
    distinct() %>% 
    collect()

sup_vedidk <- left_join(sup_complete, sup_vedidk, by = c("sup_name_first" = "name_first", "sup_name_last" = "name_last"))


# Assuming you've loaded the authors_by_pubs into a regular dataframe
authors_by_pubs_df <- dplyr::tbl(con, "authors_by_pubs") %>%
    dplyr::select(id_unique, name_first, name_last, vedidk, id_helper) %>% 
        collect()

# Check for common id_unique for each row of non_unique_rows
has_common_id_unique <- apply(sup_vedidk, 1, function(row) {
    id_unique_for_vedidk_x <- authors_by_pubs_df$id_unique[authors_by_pubs_df$vedidk == row["vedidk.x"]]
    id_unique_for_vedidk_y <- authors_by_pubs_df$id_unique[authors_by_pubs_df$vedidk == row["vedidk.y"]]

    length(intersect(id_unique_for_vedidk_x, id_unique_for_vedidk_y)) > 0
})

# Filter rows based on the result
coauth <- sup_vedidk[has_common_id_unique, ]



no_coauth <- anti_join(sup_vedidk, coauth, by = "vedidk.x")
# ---> we need to disambiguate these further -> perhaps based on sup discipline and sup institution?

no_coauth <- left_join(no_coauth, matching_data %>% dplyr::select(vedidk, disc_ford), by = c("vedidk.y" = "vedidk")) %>% 
    distinct()



non_unique_rows2 <- coauth %>%
    group_by(vedidk.x) %>%
    filter(n() > 1) %>%
    ungroup()   #this table tells us that there are 2 cases in which the researchers has published with more than 1 person who has the same name as the supervisor



# supervisors matching

sup_unique_rows <- sup_vedidk %>%
    group_by(vedidk.x) %>%
    filter(n() == 1) %>%
    ungroup()

sup_non_unique_rows <- sup_vedidk %>%
    group_by(vedidk.x) %>%
    filter(n() > 1) %>%
    ungroup()


# Check for common id_unique for each row of non_unique_rows
has_common_id_unique <- apply(sup_non_unique_rows, 1, function(row) {
    id_unique_for_vedidk_x <- authors_by_pubs_df$id_unique[authors_by_pubs_df$vedidk == row["vedidk.x"]]
    id_unique_for_vedidk_y <- authors_by_pubs_df$id_unique[authors_by_pubs_df$vedidk == row["vedidk.y"]]
    
    length(intersect(id_unique_for_vedidk_x, id_unique_for_vedidk_y)) > 0
})

# Filter rows based on the result
sup_coauth_rest <- sup_non_unique_rows[has_common_id_unique, ]

sup_total <- rbind(sup_unique_rows, sup_coauth_rest) #this show we were able to match 223 out of 247 supervisors vedidks




# non_unique_rows$vedidk.y <- as.character(non_unique_rows$vedidk.y)
# non_unique_rows$vedidk.x <- as.character(non_unique_rows$vedidk.x)







# control_id <-  sup_control$sup_vedidk
# sup_names <- dplyr::tbl(con, "authors_by_pubs") %>% # colnames()
#     dplyr::select(id_helper, vedidk) %>% 
#     filter(vedidk %in% control_id) %>% 
#     dplyr::rename(sup_name = id_helper) %>%
#     dplyr::rename(sup_vedidk = vedidk) %>%
#     dplyr::collect() %>% 
#     distinct() #tady ideálně ještě přidat část kódu která říká "pokud je tam nějaký vedidk více než jednou, vyber jeho nejčastější sup_name -> v našem vzorku ale není žádný vedidk více než jednou, takže ok
# 
# sup_control <- left_join(sup_control, sup_names, by = "sup_vedidk")

sup_control$vedidk <- as.character(sup_control$vedidk)

sup_control



}