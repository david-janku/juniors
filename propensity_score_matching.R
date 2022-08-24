con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
on.exit(DBI::dbDisconnect(con))

DBI::dbListTables(con)

# setting up the set of all authors

all_auth <- DBI::dbReadTable(con, "authors_by_pubs") %>% #colnames()
    select(vedidk) %>% 
    distinct() %>% 
    as_tibble()

## number of pubs per researcher (mainly for cleaning the extreme low and high values)

### first selecting only specific types of publications - this appear to be working, but suggests that all pubs in this brach of the database are of the tzpes that I am looking for, which is a bit weird
n_pubs_types <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
    select(id_unique, pub_type) %>% 
    filter(pub_type %in% c("J","B","C","D")) %>% 
    as_tibble() 

n_pubs_filtered <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
    select(vedidk, id_unique) %>% 
    filter(id_unique %in% n_pubs_types$id_unique) %>% #it doesn't really matter whether I use this filter or not - it always come with the same result which is weird
    distinct() %>% 
    as_tibble()

n_pubs_count <- count(n_pubs_filtered, vars = "vedidk") %>% 
    filter(freq > 4) %>%  #filtering out all authors with less than 4 publications
    as_tibble

sum(n_pubs_count$freq>400) #139 authors have more than 400 publications -> should I delete them too?


# graph_all_researchers <- ggplot(data=n_pubs_count, aes(x = reorder(vedidk, -freq), y=freq)) +
#     geom_col() + ggtitle("Number of publications per researcher (whole career)") +
#     xlab("Researchers (vedidk)") + ylab("Number of publications")
# 
# graph_all_researchers


##calculating career age for each researcher

age_pubs <- DBI::dbReadTable(con, "riv_disc") %>% 
    select(id_unique, year) %>% 
    filter(id_unique %in% n_pubs_filtered$id_unique) %>% 
    distinct() %>% 
    as_tibble()

age_data <- left_join(age_pubs, n_pubs_filtered, by = "id_unique") %>% 
    distinct() 

career_start <- age_data %>% 
    filter(vedidk %in% n_pubs_count$vedidk) %>% 
    group_by(vedidk) %>% 
    slice(which.min(year)) %>% 
    ungroup() 

career_start$id_unique <- NULL
career_start$year <- as.numeric(career_start$year) 
career_start$year <- (2022-career_start$year)

##calculating discipline - here I must set to stick exactly to the discipline when doing the matching!

discipline_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()
    select(id_unique, pub_type, disc, ford) %>%
    filter(id_unique %in% n_pubs_filtered$id_unique) %>%
    filter(pub_type %in% c("J","B","C","D")) %>%
    tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
    as_tibble()

discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("BA", "BB")] <- 10100 #BD
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("IN", "BC", "BD", "AF")] <- 10200
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("BE", "BM", "BF", "BG", "BK", "BL", "BH", "BI","BN")] <- 10300
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("CC", "CA", "CH", "CF", "CD", "CG", "CB")] <- 10400
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DA", "DB", "DC", "DE", "DG", "DO", "DK", "DL", "DM", "DI", "DJ")] <- 10500
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EA", "EB", "EE", "CE", "EB", "BO", "EF", "EG", "DA", "EH")] <- 10600

discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JN", "JM", "GB", "AL", "JO")] <- 20100
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JA", "JB", "JW", "JD", "JC")] <- 20200
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JR", "JT", "JQ", "BJ", "JU", "JV", "JF", "JS", "JL")] <- 20300
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("CI")] <- 20400
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JP", "JG", "JJ", "JH", "JI", "JK")] <- 20500
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FS")] <- 20600
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DH", "JE", "JT", "JP", "JQ")] <- 20700
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI")] <- 20800
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI")] <- 20900
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("JJ")] <- 21000
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GM", "JY", "KA")] <- 21100

discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EB", "EC", "FH", "FR", "ED")] <- 30100 #FP
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FA", "FB", "FC", "FD", "FF", "FG", "FI", "FJ", "FK", "FL", "FO", "FE")] <- 30200 #not added FH, FP
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FN", "FM", "DN", "AQ", "AK")] <- 30300 #FL, FP 
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI")] <- 30400
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FP")] <- 30500

discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GD", "GK", "GL", "DF", "GE", "GF", "GC")] <- 40100 
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GG", "GH", "GI")] <- 40200
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GJ")] <- 40300 
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("EI", "GM")] <- 40400
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("GM")] <- 40500

discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AN")] <- 50100 
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AH", "GA")] <- 50200
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AM")] <- 50300 
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AO", "AC")] <- 50400
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AG")] <- 50500
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AD", "AE")] <- 50600
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DE", "AP")] <- 50700 #AO
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AJ", "AF")] <- 50800
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AK")] <- 50900

discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AB", "AC")] <- 60100 
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AI", "AJ")] <- 60200
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AA")] <- 60300 
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("AL")] <- 60400

#additional cleaning:
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Cf")] <- 10400
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Al")] <- 60400
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Am")] <- 50300
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("DD")] <- 10500
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("E")] <- 10600
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("FQ")] <- 30200
discipline_pubs$disc_ford[discipline_pubs$disc_ford %in% c("Gk")] <- 40100

discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "HD")
discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "XX")
discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "O5")
discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "O6")
discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "J")
discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "O9")
discipline_pubs$disc_ford <- na_if(discipline_pubs$disc_ford, "")

count(discipline_pubs$disc_ford)




discipline_pubs_short <- discipline_pubs 
discipline_pubs_short$disc_ford <- substring(discipline_pubs_short$disc_ford, 0, 1)
    
discipline_data <- left_join(discipline_pubs_short, n_pubs_filtered, by = "id_unique") %>% 
    distinct() 

discipline_data$disc_ford <- as.character(discipline_data$disc_ford)


count(discipline_data$disc_ford) 


disciplines <- discipline_data %>% 
    select(disc_ford, vedidk) %>% 
    filter(!is.na(disc_ford)) %>% 
    filter(vedidk %in% n_pubs_count$vedidk) %>% 
    group_by(vedidk) %>% 
    slice(which.max(table(disc_ford))) %>% 
    ungroup() 

count(disciplines$disc_ford)

# just trying whether the above method actually produces sensible results
# df <- data.frame(id = 1:10, strings = c("A", "B","C", "A", "B", "D", "B", "C", "C", "C"))
# table(df$strings)
# slice(df, which.max(table(df$strings)))

### it would be cool to now count how many/what proportion of publications each author has within their "primary discipline" vs in other disciplines

##interdisciplinarity / specialisation

interdisc <- discipline_data %>% 
    dplyr::select(disc_ford, vedidk) %>% 
    filter(!is.na(disc_ford)) %>% 
    filter(vedidk %in% n_pubs_count$vedidk) 

interdisc <- left_join(interdisc, disciplines, by = "vedidk", suffix = c("_pub", "_main"))

interdisc_final <- interdisc %>%  
      group_by(vedidk) %>% 
      filter(disc_ford_pub != disc_ford_main) %>% 
      dplyr::count(name = "interdisc") %>% 
      ungroup() 

interdisc_final <- left_join(n_pubs_count, interdisc_final, by = "vedidk")

interdisc_final$interdisc <- replace_na(interdisc_final$interdisc, "0")

interdisc_final$interdisc <- as.numeric(interdisc_final$interdisc)


interdisc_final <- interdisc_final %>% 
    mutate(interdisc_proportion = interdisc/freq*100)

plot(hist(interdisc_final$interdisc_proportion))

sum(interdisc_final$interdisc_proportion == 0)


##calculating intervention group

GJ <- DBI::dbReadTable(con, "cep_details") %>%
    filter(program_kod == "GJ") %>% 
    select(kod, disc, ford, year_start) %>% 
    tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
    distinct() %>% 
    as_tibble

    GJ_vector <- GJ$kod %>% 
        as_tibble() %>% 
        as_vector() %>% 
        as_tibble() %>% #from here below it was added - I should check that it does change any further results 
        distinct() %>% #here
        as_vector()
    
    treatment_group <- DBI::dbReadTable(con, "cep_investigators") %>%
        select(kod, vedidk) %>% 
        filter(kod %in% GJ_vector) %>% 
        distinct() %>% 
        as_tibble()
    
treatment_refined <- treatment_group %>% 
     filter(!is.na(vedidk)) 
# This show that although the database could find 845 distinct pairs of Junior grant code and vedidk, only in 356 cases there were actual vedidks, which means that in 845-356=489 cases there were pairs of existing code but missing vedidk 

treatment_data <- n_pubs_count %>% 
    select(vedidk) %>% 
    mutate(treatment = ifelse(vedidk %in% treatment_refined$vedidk, 1, 0)) %>% 
    as_tibble()

count(treatment_data$treatment) #this shows that it matched only 329 out of 356 treatment vedidks -> not sure why?


##calculating first (career) year of receiving any grant


career_start_year <- age_data %>% 
    dplyr::select(vedidk, year) %>% 
    filter(vedidk %in% n_pubs_count$vedidk) %>% 
    group_by(vedidk) %>% 
    slice(which.min(year)) %>% 
    ungroup() 

# career_start$id_unique <- NULL
# career_start$year <- as.numeric(career_start$year) 
# career_start$year <- (2022-career_start$year)


funding_codes <- DBI::dbReadTable(con, "cep_investigators") %>%
    filter(vedidk %in% career_start_year$vedidk) %>% 
    filter(!is.na(vedidk)) %>% #pozor! více než 50 % projektů v databázi nemá přiřazený vedidk řešitele!
    # filter(role_researcher == G) %>% filtruje jen hlavní řešitele (G) a ne "další řešitele" (R)
    dplyr::select(kod, vedidk) %>% 
    # distinct() %>% 
    as_tibble

# count(funding_codes$role_researcher)

funding_year <- DBI::dbReadTable(con, "cep_details") %>% 
    filter(kod %in% funding_codes$kod) %>% #pozor! opět mi to našlo cca jen 50 % z oho přechozího datasetu, který obsahoval všechny projekty které mají vedidky. Nevím proč 
    dplyr::select(kod, disc, ford, year_start) %>% 
    # tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
    distinct() %>% 
    as_tibble

pi_year <- left_join(funding_year, funding_codes, by = "kod")

pi_year$year_start <- ifelse(nchar(pi_year$year_start, type = "chars", allowNA = FALSE, keepNA = NA)>4, substring(pi_year$year_start, 7), pi_year$year_start)
    
pi_year$year_start <- as.numeric(pi_year$year_start)



pi_first_year <- pi_year %>% 
    dplyr::select(vedidk, year_start) %>% 
    filter(!is.na(vedidk)) %>% 
    filter(!is.na(year_start))  %>% 
    group_by(vedidk) %>% 
    slice(which.min(year_start)) %>% 
    ungroup() 


pi_final <- left_join(career_start_year, pi_first_year, by = "vedidk")

pi_final$year <- as.numeric(pi_final$year)
pi_final$year_start <- as.numeric(pi_final$year_start)

pi_final$first_grant <- (pi_final$year_start - pi_final$year)

count(pi_final$first_grant<0) #seems that 4738 people have negative values, meaning that they received a grant earlier than they pirst published anything - is this plausible? Further, 36483 people did not receive any grant, so have NAs, which might b probematic in Propensity score matching (I think)    




#final data

final_data <- left_join(treatment_data, career_start, by = "vedidk")
final_data <- left_join(final_data, disciplines, by = "vedidk")
final_data <- left_join(final_data, n_pubs_count, by = "vedidk")

##excluding rows with missing values
final_data <- as_tibble(na.omit(final_data))
sum(is.na(final_data))

##excluding ids from treatment group for which we couldnt find a supervisors manually  
ids_out <- read.csv2(here::here("data", "raw", "supervisors.csv"))  
   
ids_out <- as_tibble(ids_out) %>% 
    filter(is.na(vedoucí.vedidk)) %>% 
    filter(!is.na(vedidk_core_researcher))

final_data <- final_data %>% 
    filter(!vedidk %in% ids_out$vedidk_core_researcher)
    

# final_data <- final_data %>% 
    # filter(!freq>400)

                        
# final_data <- left_join(treatment_data, career_start, disciplines, n_pubs_count, by = "vedidk")


#matching using matchit

summary(final_data$treatment)
summary(final_data$year)
summary(final_data$disc_ford)
summary(final_data$freq)

#matching with whole population
out <- matchit(treatment~year+freq, method="nearest", data=final_data, ratio = 1, exact = "disc_ford", replace = TRUE)

matched_data <- match.data(out) # this actually spits out exactly the list of treated nad mtached untreated people in a way that I can easily use it!

table(matched_data$disc_ford, by = matched_data$treatment)

summary(out)

plot(out)
plot(out, type="hist")

#matching only with those who previously received grant

pi_final$year <- NULL
pi_final$year_start <- NULL
final_data_funded <- left_join(final_data, pi_final, by = "vedidk")
final_data_funded <- as_tibble(na.omit(final_data_funded))
sum(is.na(final_data_funded))

out_funded <- matchit(treatment~year+freq+first_grant, method="nearest", data=final_data_funded, ratio = 1, exact = "disc_ford", replace = TRUE)

matched_data_funded <- match.data(out_funded) # this actually spits out exactly the list of treated nad mtached untreated people in a way that I can easily use it!

table(matched_data_funded$disc_ford, by = matched_data_funded$treatment)

summary(out_funded)


#another method (which was harder to tweak so I didnt end up using it)

glml <- glm(treatment~year+freq+disc_ford, family = binomial, data = final_data)

summary(glml)


# tr <- cbind(final_data$treatment)
# x <- cbind(final_data$year, final_data$disc_ford, final_data$freq)
# var1 <- final_data$disc_ford

rr1 <- Match(Tr = final_data$treatment, X = myMatchMat$PS, exact = c(0,1))
summary(rr1)

MatchBalance(treatment~year+freq, match.out = rr1, nboots = 0, data = final_data)

myMatchMat <- data.frame("PS"=glml$fitted.values, "trim"=final_data$disc_ford)

glm_data <- data.frame("PS"=glml$fitted.values)

# I can try using Matchby() - that seems like a flavour I would need
