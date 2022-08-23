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


count(discipline_data$disc_ford) #test showed I ned to look closer to things that contain D and F and perhaps J and then perhaps delete all the letters

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


##calculating intervention

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


##calculating whether they have received any grant //in some time period// - maybe first (career) year of receving their grant?


#final data

final_data <- left_join(treatment_data, career_start, by = "vedidk")
final_data <- left_join(final_data, disciplines, by = "vedidk")
final_data <- left_join(final_data, n_pubs_count, by = "vedidk")

final_data <- as_tibble(na.omit(final_data))
sum(is.na(final_data))

# final_data <- final_data %>% 
    # filter(!freq>400)

                        
# final_data <- left_join(treatment_data, career_start, disciplines, n_pubs_count, by = "vedidk")


#matching using matchit

summary(final_data$treatment)
summary(final_data$year)
summary(final_data$disc_ford)
summary(final_data$freq)

out <- matchit(treatment~year+freq, method="nearest", data=final_data, ratio = 1, exact = "disc_ford", replace = TRUE)

matched_data <- match.data(out) # this actually spits out exactly the list of treated nad mtached untreated people in a way that I can easily use it!

table(matched_data$disc_ford, by = matched_data$treatment)

plot(out)
plot(out, type="hist")

#another method:

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

Matchby()
