#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param eigen_centr
#' @param clustering
#' @param ind_topics
#' @param ind_pubs
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
just_test <- function(db_path, full_indicator) {

 
    pubs_treatment <- full_data3 %>% 
        filter(treatment == 1) 
    mean(pubs_treatment$pubs_after_grant, na.rm = FALSE)
    
    pubs_control <- full_data3 %>% 
        filter(!treatment == 1) 
    mean(pubs_control$pubs_after_grant, na.rm = FALSE)
    
    by(full_data3$pubs_after_grant, INDICES = full_data3$treatment, FUN = summary) 
    
    
    summary(pubs_control$pubs_after_grant > pubs_treatment$pubs_after_grant) #pro 2826 unfunded people (37,3 % of all unfunded) it is true that they have more publications than funded people
    sum(pubs_control$pubs_after_grant > mean(pubs_treatment$pubs_after_grant)) #pro 1670 unfunded people (22 % of all unfunded) it is true that they have more publications than average number of publications of funded people
    sum(pubs_control$pubs_after_grant > median(pubs_treatment$pubs_after_grant)) #pro 2505 unfunded people (33 % of all unfunded) it is true that they have more publications than median number of publications of funded people
    
    # library(stats)
    # cor.test(x = pubs_treatment$n_pubs_grant, y = pubs_control$n_pubs_grant, 
    #          method = c("pearson"), 
    #          conf.level = 0.95)
    # 
    # cor(pubs_treatment$pubs_after_grant, pubs_control$pubs_after_grant)
    # 
    # plot(pubs_treatment$n_pubs_grant, pubs_control$n_pubs_grant)
    
    bartlett.test(full_data3$pubs_after_grant ~ full_data3$treatment)
    #seems there is significant difference in homogeneity- that is a problem - what should I do? Should I not use t-test?
    
    boxplot(full_data3$pubs_after_grant ~ full_data3$treatment)
    # this further proves that there are more outliers in the control group, which could skew the results
    
    t.test(formula = full_data3$pubs_after_grant ~ full_data3$treatment,
           alternative = "two.sided",
           paired = FALSE,   
           var.equal = TRUE,
           conf.level = 0.95)
    
    
    cohen <- rstatix::cohens_d(full_data3, 
                               pubs_after_grant ~ treatment, 
                               paired = FALSE, ci = TRUE,
                               conf.level = 0.95,
                               ci.type = "norm",
                               nboot = 1000)
    
    # sum(matched_data$pubs_total>400) #139 authors have more than 400 publications -> should I delete them too?
    
    ##maybe it would be better to use linear mixed model rather than t-test? https://stats.stackexchange.com/questions/319504/hypothesis-testing-with-control-and-treatment-group-which-statistical-analysis
    
    
    #t-test graph (other nice ideas how to plot it here: https://stats.stackexchange.com/questions/190223/how-to-visualize-independent-two-sample-t-test)
    a        = c(mean(pubs_treatment$pubs_after_grant), mean(pubs_control$pubs_after_grant))
    names(a) = c("financovaní", "nefinancovaní")
    se       = c(sd(pubs_treatment$pubs_after_grant)/sqrt(length(pubs_treatment$pubs_after_grant)), 
                 sd(pubs_control$pubs_after_grant)/sqrt(length(pubs_control$pubs_after_grant)))
    windows()
    bp = barplot(a, ylim=c(0, 30), xpd=FALSE, main = "Srovnání počtu publikací v období 5 let od obdržení grantu", ylab = "Počet publikací v období 5 let od obdržení grantu")
    box()
    arrows(x0=bp, y0=a-se, y1=a+se, code=3, angle=90)
    
    
    plot(density(pubs_control$pubs_after_grant), col = "red")
    lines(density(pubs_treatment$pubs_after_grant), col = "blue")
    
    
    #even better way to visualize overlapping densities below, inspiration from https://stackoverflow.com/questions/6939136/how-to-overlay-density-plots-in-r 
    library(ggplot2)
    
    c(pubs_treatment$pubs_after_grant, pubs_control$pubs_after_grant)
    #Sample data
    dat <- data.frame(dens = c(rnorm(100), rnorm(100, 10, 5))
                      , lines = rep(c("a", "b"), each = 100))
    
    dat <- data.frame(dens = c(as.vector(pubs_treatment$pubs_after_grant), as.vector(pubs_control$pubs_after_grant)), lines = rep(c("a", "b"), each = 100))
    #Plot.
    ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)
    
    ggplot(reshape2::melt(full_data3), mapping = aes(fill = treatment, x = pubs_after_grant)) + geom_density (alpha = .5)
    
    
    
    
    
    
    
    
    
    
    
    
    ##plotting 
    
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    DBI::dbListTables(con)
    
    ##plotting number of publications of all core researchers
    
    ids <- read.csv2(here::here("data", "raw", "supervisors.csv"))
    
     ids_complete <- as_tibble(ids) %>% 
             select(vedidk_core_researcher, vedoucí.vedidk, grant_start_year) %>% 
             filter(!is.na(vedoucí.vedidk)) %>% 
             filter(!is.na(vedidk_core_researcher)) 
     
     all_researchers_vector <- ids_complete$vedidk_core_researcher %>% 
         as_tibble() %>% 
         distinct() %>% #this was added - I should check that it does change any further results 
         as_vector() 
     
     all_researchers_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
         select(vedidk, id_unique) %>% 
         filter(vedidk %in% all_researchers_vector) %>% 
         distinct() %>% 
         as_tibble()
     
     ## z nejakeho duvodu ten postup nize nefunguje, vzdy skoncim se stejnym poctem publikaci jako na zacatku (7770) 
     # all_researchers_filtered_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()  
     #     select(id_unique, pub_type) %>% 
     #     filter(id_unique %in% all_researchers_pubids$id_unique) %>% 
     #     filter(pub_type %in% c("J","B","C","D")) %>% 
     #     as_tibble() 
     # 
     # all_researchers_pubids_final <- DBI::dbReadTable(con, "authors_by_pubs") %>%
     #     select(id_unique, vedidk) %>%
     #     filter(id_unique %in%  all_researchers_filtered_pubs$id_unique) %>%
     #     distinct() %>%
     #     filter(vedidk %in% all_researchers_vector) %>% 
     #     distinct() %>% 
     #     as_tibble()
         
     all_researchers_pubs <- all_researchers_pubids %>% 
         group_by(vedidk) %>% 
         count(name = "weight") %>% 
         ungroup() #this will generate only 239 vedidks, not 248 entered in the all_researchers_vector, which is 4 % --> explained by duplicates in vedidks (people who received 2 grants)
     
     
     #important things that I need to work on further
     
     ids_complete$vedidk_core_researcher <- as.character(ids_complete$vedidk_core_researcher)
     
     missing_researchers <- anti_join(ids_complete, all_researchers_pubs, by = c("vedidk_core_researcher" = "vedidk"))
     
     ids_complete_duplicated <- ids_complete %>% 
         filter(duplicated(vedidk_core_researcher))  #I should filter out the double vedidks at the beginning (i.e. from the ids_complete dataset) and only include the earlier one (i.e. the date of the first grant, not the second)
     
     
     
     all_researchers_pubs <- all_researchers_pubs %>% 
         filter(vedidk != 4427920)    #one vedidk had over 800 publications, which seemed unrealistic, so I deleted it
     
     sum(all_researchers_pubs$weight<11) #4 autori maji mene nez 5 publikaci a 29 autoru ma mene nez 10 --> nevim jestli je to pro nas problem? jestli bychom meli dat nejaky cut-off?
     
     median(all_researchers_pubs$weight) #průměr je 28.9 publikace na autora, median je 22,5 publikace na autora
     
     graph_all_researchers <- ggplot(data=all_researchers_pubs, aes(x = reorder(vedidk, -weight), y=weight)) +
         geom_col() + ggtitle("Number of publications per researcher (whole career)") +
         xlab("Researchers (vedidk)") + ylab("Number of publications")
     
     graph_all_researchers
     
     
     ##plotting number of publications per researcher prior to receiving grant 
     
     before_grant_researchers_pubids <- DBI::dbReadTable(con, "riv_disc") %>% 
         select(id_unique, year) %>% 
         filter(id_unique %in% all_researchers_pubids$id_unique) %>% 
         distinct() %>% 
         as_tibble()
     
     bg <- left_join(before_grant_researchers_pubids, all_researchers_pubids, by = "id_unique") %>% 
         distinct()  #když přidám  filter(duplicated(id_unique)) tam mi to ukaze tabulku, kde ale zadne duplikovane id nejsou, takze nevim jestli je problem v te funkci nebo v cem
        
         
     
    ids_complete$vedidk_core_researcher <- as.character(ids_complete$vedidk_core_researcher)
         
     bg <- left_join(bg, ids_complete, by = c("vedidk" = "vedidk_core_researcher")) #v tomhle kroku z nejakeho duvodu pribzde asi 170 radku
     
                   
     bg <- bg %>% 
         # select(-c("vedoucí.vedidk")) %>% 
         distinct() %>% 
         filter(!duplicated(id_unique))
     
     bg$grant_start_year <- substring(bg$grant_start_year, 7) 
     
     pubs_before_grant_complete <- bg %>% 
         filter(grant_start_year>=year) #this has reduced the number of rows (which should be equal to unique ids) from 7700(/7585 not duplicated) to 6295, suggesting that only 1290 (17 %) publikaci vzniklo po zahajeni danych projektu. 83% tedy vzniklo pred zahajenim projektu
     
     #more sensible would be to only count publications that were published in the secord (or even third) year of the grant and beyond (e.g. if the grant strat year was 2015, we should only count publications published in year >= 2016)
     pubs_before_grant_extended <- bg %>% 
         filter(grant_start_year>=year) #funkce filter nedovoluje davat numebricke funknce, musim to zvresit nejak jinak
     
     #tohle je vzorek ktery budeme porovnavat -> ted se zda maly (pouze 17%), ale jakmile Radim uploaduje databazi, pribudou tam publikaci za dalsi 3 posledni roky a rekl bych ze to vyrazne vzroste klidne az ke 40 % -> zatim to tedy pocitat nebudu a do grafu davat taky ne, protozeje to zavadejici
     pubs_after_grant <- bg %>% 
         filter(grant_start_year<year)
     
     
     pubs_before_grant <- pubs_before_grant_complete %>% 
         group_by(vedidk) %>% 
         count(name = "weight") %>% 
         ungroup()
     
     pubs_before_grant <- pubs_before_grant %>% 
         filter(vedidk != 4427920)    #one vedidk had over 800 publications, which seemed unrealistic, so I deleted it
     
     sum(pubs_before_grant$weight<11) #7 autoru ma 5 nebo mene publikaci a 47 autoru (cca 20 % vzorku) ma 10 nebo mene
     
     median(pubs_before_grant$weight) #průměr je 23.1 publikace na autora, median je 19 publikace na autora
     
     graph_before_grant <- ggplot(data=pubs_before_grant, aes(x = reorder(vedidk, -weight), y=weight)) +
         geom_col() + ggtitle("Number of publications per research (before grant)") +
         xlab("Researchers (vedidk)") + ylab("Number of publications")
     
     graph_before_grant
     
     ##plot career starting years across recipients
     
     career_start <- bg %>% 
         group_by(vedidk) %>% 
         slice(which.min(year)) %>% 
         ungroup()
     
     # career_start_2 <- bg %>% 
     #     group_by(vedidk) %>% 
     #     filter(year == min(year)) %>% 
     #     ungroup() %>% 
     #     select(-c(id_unique)) %>% 
     #     distinct()
     
     career_start_2 <- career_start %>% 
         group_by(year) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     
     graph_career_start <- ggplot(data=career_start_2, aes(x = year, y=frequency)) +
         geom_col() + ggtitle("Distribution of career starting year across focal researchers") +
         xlab("Starting year (first publication)") + ylab("Number of researchers (vedidk)")
     
     graph_career_start
     
     ##plot differences between career starting years and year of receiving grant
     
     career_start$grant_start_year <- as.numeric(career_start$grant_start_year)
     career_start$year <- as.numeric(career_start$year)
     
     career_start$difference <- career_start$grant_start_year-career_start$year
     
     difference <- career_start %>% 
         group_by(difference) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     graph_difference <- ggplot(data=difference, aes(x = difference, y=frequency)) +
         geom_col() + ggtitle("Distribution of differences between career starting year and year of receiving grant") +
         xlab("Difference (years)") + ylab("Number of researchers")
     
     graph_difference
     
     
     sum(career_start$difference>8) #interestingly, 144 people (cca 60 %) received the grant more than 8 years after their first publications --> given that the grants should only be given to people up to 8 years after finishing their PhD degree, this is very suprising
     
     median(career_start$difference) #to colour this further, median difference is 9 years, and mean 9.3, which is both above 8
     
     
     ##plotting number of publications of all supervisors
     
     all_supervisors_vector <- ids_complete$vedoucí.vedidk %>% 
         as_tibble() %>% 
         distinct() %>% #this was added - I should check that it does change any further results 
         as_vector() 
     
     all_sup_pubids <- DBI::dbReadTable(con, "authors_by_pubs") %>% 
         select(vedidk, id_unique) %>% 
         filter(vedidk %in% all_supervisors_vector) %>% 
         distinct() %>% 
         as_tibble()
     
     # # z nejakeho duvodu ten postup nize nefunguje, vzdy skoncim se stejnym poctem publikaci jako na zacatku (7770)
     # all_sup_filtered_pubs <- DBI::dbReadTable(con, "riv_disc") %>%  #colnames()
     #     select(id_unique, pub_type) %>%
     #     filter(id_unique %in% all_sup_pubids$id_unique) %>%
     #     filter(pub_type %in% c("J","B","C","D")) %>%
     #     as_tibble()
     # 
     # all_sup_pubids_final <- DBI::dbReadTable(con, "authors_by_pubs") %>%
     #     select(id_unique, vedidk) %>%
     #     filter(id_unique %in%  all_sup_filtered_pubs$id_unique) %>%
     #     distinct() %>%
     #     filter(vedidk %in% all_supervisors_vector) %>%
     #     distinct() %>%
     #     as_tibble()
     
     all_sup_pubs <- all_sup_pubids %>% 
         group_by(vedidk) %>% 
         count(name = "weight") %>% 
         ungroup()    #this will generate only 211 vedidks, not 248 entered in the all_supervisors_vector, which is 15 % loss --> explained by duplicates in vedidks 
     
     
     # ids_complete$vedoucí.vedidk <- as.character(ids_complete$vedoucí.vedidk)
     # 
     # missing_vedouci <- anti_join(ids_complete, all_sup_pubs, by = c("vedoucí.vedidk" = "vedidk"))
     # 
     # ids_complete_duplicated_vedouci <- ids_complete %>% 
     #     filter(duplicated(vedoucí.vedidk)) 
     
     
     # #when examininsg numbers, I see that vedidk 5500095 has 890 publications, but here I am less sure whether to discard it, so I left it in
     # all_researchers_pubs <- all_researchers_pubs %>% 
     #     filter(vedidk != 5500095)    #one vedidk had over 800 publications, which seemed unrealistic, so I deleted it
     
     sum(all_sup_pubs$weight<11) #5 autoru ma 10 nebo mene publikaci (compared to 29 of core researchers)
     
     median(all_sup_pubs$weight) #průměr je 101 publikace na autora (compared to 28.9 of core researchers)
     
     graph_all_supervisors <- ggplot(data=all_sup_pubs, aes(x = reorder(vedidk, -weight), y=weight)) +
         geom_col() + ggtitle("Number of publications per supervisor (whole career)") +
         xlab("supervisors (vedidk)") + ylab("Number of publications")
     
     graph_all_supervisors
     
     # plotting career starting year of supervisors
     
    sup_pubid_years <- DBI::dbReadTable(con, "riv_disc") %>% 
         select(id_unique, year) %>% 
         filter(id_unique %in% all_sup_pubids$id_unique) %>% 
         distinct() %>% 
         as_tibble()
     
    sup_pubs_enriched <- left_join(sup_pubid_years, all_sup_pubids, by = "id_unique") %>% 
         distinct()
     
     
     career_start_sup <- sup_pubs_enriched %>% 
         group_by(vedidk) %>% 
         slice(which.min(year)) %>% 
         ungroup()
     
     # career_start_2 <- bg %>% 
     #     group_by(vedidk) %>% 
     #     filter(year == min(year)) %>% 
     #     ungroup() %>% 
     #     select(-c(id_unique)) %>% 
     #     distinct()
     
     career_start_sup_2 <- career_start_sup %>% 
         group_by(year) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     
     graph_career_start_sup <- ggplot(data=career_start_sup_2, aes(x = year, y=frequency)) +
         geom_col() + ggtitle("Distribution of career starting year across supervisors") +
         xlab("Starting year (first publication)") + ylab("Number of researchers (vedidk)")
     
     graph_career_start_sup
     
     
     # plotting differences between career starting year of researcher and supervisor
     
     career_start <- rename(career_start, year_researcher = year, vedidk_sup = vedoucí.vedidk)
     career_start_sup<- rename(career_start_sup, year_supervisor = year, vedidk_sup = vedidk)
     
     
     career_start_both <- left_join(career_start_sup, career_start, by = "vedidk_sup")
     
     career_start_both$year_researcher <- as.numeric(career_start_both$year_researcher)
     career_start_both$year_supervisor <- as.numeric(career_start_both$year_supervisor)
     
     career_start_both$difference <- career_start_both$year_researcher-career_start_both$year_supervisor
     
     career_start_both <- career_start_both %>% 
         filter(!is.na(difference))
     
     difference_both <- career_start_both %>% 
         group_by(difference) %>% 
         count(name = "frequency") %>% 
         ungroup()
     
     graph_difference_both <- ggplot(data=difference_both, aes(x = difference, y=frequency)) +
         geom_col() + ggtitle("Distribution of differences between researcher career starting year and supervisor career starting year") +
         xlab("Difference (years)") + ylab("Number of cases")
     
     graph_difference_both
     
     
     sum(career_start_both$difference<1) #interestingly, 10 pairs of people (cca 4 %) have 0 or fewer years difference, meaning that the supervisor would be younger than the foical researcher
     
     median(career_start_both$difference) #to colour this further, median difference is 9 years, and mean 9.3, which is both above 8
     
     
     
     
     
     
     
     ## descriptives
     
     final_data$id <- seq_along(final_data$vedidk)
     full_indicator$id <- seq_along(full_indicator$vedidk)
     
     
     d <- left_join(final_data, full_indicator)
     
     d$treatment <- as_factor(d$treatment)
     d$independence_timing <- as_factor(d$independence_timing)
     
     
     d_before <- d %>% filter(independence_timing == "before_intervention") %>% select(-vedidk, -vedidk_treatment, -pub_table, -edgelist, -graph, -independence_timing, -sup_name, -sup_vedidk)
     d_after <- d %>% filter(independence_timing == "after_intervention") %>% select(-vedidk, -vedidk_treatment, -pub_table, -edgelist, -graph, -independence_timing, -sup_name, -sup_vedidk)

     library(tidyverse)
     library(psych)
     
     table_before_0 <- d_before %>% filter(treatment == 0) %>% describe() %>% round(., digits = 3)
     table_before_0$na_count <- colSums(is.na(d_before %>% filter(treatment == 0)))
     table_before_1 <- d_before %>% filter(treatment == 1) %>% describe() %>% round(., digits = 3)
     table_before_1$na_count <- colSums(is.na(d_before %>% filter(treatment == 1)))
     table_before_2 <- d_before %>% filter(treatment == 2) %>% describe() %>% round(., digits = 3)
     table_before_2$na_count <- colSums(is.na(d_before %>% filter(treatment == 2)))
     
     table_after_0 <- d_after %>% filter(treatment == 0) %>% describe() %>% round(., digits = 3)
     table_after_0$na_count <- colSums(is.na(d_after %>% filter(treatment == 0)))
     table_after_1 <- d_after %>% filter(treatment == 1) %>% describe() %>% round(., digits = 3)
     table_after_1$na_count <- colSums(is.na(d_after %>% filter(treatment == 1)))
     table_after_2 <- d_after %>% filter(treatment == 2) %>% describe() %>% round(., digits = 3)
     table_after_2$na_count <- colSums(is.na(d_after %>% filter(treatment == 2)))
     
     table_before_0$na_count_total <- colSums(is.na(d_before))
     table_after_0$na_count_total <- colSums(is.na(d_after))
     
     
     
     table_before_2 <- describeBy(d_before, group = d_before$treatment, mat = FALSE)
     table_before2 <- as_tibble(do.call(cbind, table_before_2))
     
     write.csv(table, file = "descriptives.csv")
     
     
     table_sum_before <- by(d_before, INDICES = d_before$treatment, FUN = summary)
     table_sum_before <- as_tibble(do.call(cbind, table_sum_before))
     
     
    
     ## hypothesis testing
     
     ## differences in disciplines
     
     d$disc_ford <- substring(d$disc_ford, 0, 1)
     d$disc_ford <- as_factor(d$disc_ford)
     table(d$disc_ford, by = d$treatment)
     
     disc <- aov(RII ~ disc_ford, data = d)
     summary(disc)
     
     
     
     ### difference before intervention
     
     set.seed(123)
     
     by(d_before, INDICES = d_before$treatment, FUN = summary)
     
     d_before$treatment <- as_factor(d_before$treatment)
     
     before_treatment <- aov(RII ~ treatment, data = d_before)
     summary(before_treatment)
     #anyway it says that if the groups are not balanced, we might want to used other test --> the groups does not seem balanced: NAs: 36, 10, 19 
    
     
     means_before_treatment <- aggregate(RII ~ treatment, data = d_before, FUN = mean)
     
     
     ggplot(data = means_before_treatment, aes(x = treatment, y = RII)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "Treatment", y = "Mean RII") +
         theme_bw()
     
    # replications(RII ~ treatment, data = d_before) 
    #  
    #  before_lme <- nlme::lme(RII ~ treatment, data = d_before, na.action = na.omit(d_before))
    #  summary(before_lme)
    #  
    #  model.tables(before, type = "means")
     
     
     
     
     # by(d_before, INDICES = d_before$disc_ford, FUN = summary)
     
     table(d_before$disc_ford, by = d_before$treatment)
     sum(is.na(d_before$RII))
     d_before <- d_before %>% filter(!is.na(RII))
     sum(is.na(d_before$RII))
     table(d_before$disc_ford, by = d_before$treatment)
     #seems like there are not always exact macthes in discipline groups --> should find out why is that
     
     d_before$disc_ford <- substring(d_before$disc_ford, 0, 1)
     d_before$disc_ford <- as_factor(d_before$disc_ford)
     
     
     d_before$treatment <- as_factor(d_before$treatment)
     
     
     disc_before <- aov(RII ~ disc_ford, data = d_before)
     summary(disc_before)
     
     means_before_disc <- aggregate(RII ~ disc_ford, data = d_before, FUN = mean)
     
     
     ggplot(data = means_before_disc, aes(x = disc_ford, y = RII)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "disc_ford", y = "Mean RII") +
         theme_bw()
     
     
     
     
     before_disc_interaction <- aov(RII ~ treatment*disc_ford, data = d_before)
     summary(before_disc_interaction)
     
     tukey_results <- TukeyHSD(before_disc_interaction)
     tukey_results
     
     # interaction.plot(x.factor = d_before$treatment, 
     #                  trace.factor = d_before$disc_ford, 
     #                  response = d_before$RII,
     #                  type = "b", 
     #                  legend = TRUE, 
     #                  col = rainbow(length(levels(d_before$disc_ford))))
     # 
     # 
     # # Set the plot margins to be smaller
     # par(mar=c(5,5,2,2))
     # 
     # # Create the plot
     # plot(tukey_results, las=2, main="Multiple Comparisons")
     # 
     # # Reset the plot margins to default
     # par(mar=c(5, 4, 4, 2) + 0.1)
     # 
     
     means_before_disc_interaction <- aggregate(RII ~ treatment + disc_ford, data = d_before, FUN = mean)
     
     # Plot grouped bar plot
     ggplot(data = means_before_disc_interaction, aes(x = treatment, y = RII, fill = disc_ford)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "Treatment", y = "Mean RII") +
         scale_fill_discrete(name = "disc_ford") +
         theme_bw()
     
     ggplot(data = means_before_disc_interaction, aes(x = disc_ford, y = RII, fill = treatment)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "disc_ford", y = "Mean RII") +
         scale_fill_discrete(name = "treatment") +
         theme_bw()
     
     
     ### difference after intervention
     
     set.seed(123)
     
     by(d_after, INDICES = d_before$treatment, FUN = summary)
     
     d_after$treatment <- as_factor(d_after$treatment)
     
     after <- aov(RII ~ treatment, data = d_after)
     summary(after)
     #anyway it says that if the groups are not balanced, we might want to used other test --> the groups does not seem balanced: NAs: 183, 130, 160
     
     means_after_treatment <- aggregate(RII ~ treatment, data = d_after, FUN = mean)
     
     
     ggplot(data = means_after_treatment, aes(x = treatment, y = RII)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "Treatment", y = "Mean RII") +
         theme_bw()
     
     
     d_after$disc_ford <- substring(d_after$disc_ford, 0, 1)
     d_after$disc_ford <- as_factor(d_after$disc_ford)
     
     disc_after <- aov(RII ~ disc_ford, data = d_after)
     summary(disc_after)
     
     means_after_disc <- aggregate(RII ~ disc_ford, data = d_after, FUN = mean)
     
     ggplot(data = means_after_disc, aes(x = disc_ford, y = RII)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "disc_ford", y = "Mean RII") +
         theme_bw()
     
     
     
     after_disc_interaction <- aov(RII ~ treatment*disc_ford, data = d_after)
     summary(after_disc_interaction)
     
     tukey_results <- TukeyHSD(after_disc_interaction)
     tukey_results
     
     
     means_after_disc_interaction <- aggregate(RII ~ treatment + disc_ford, data = d_after, FUN = mean)
     
     # Plot grouped bar plot
     ggplot(data = means_after_disc_interaction, aes(x = treatment, y = RII, fill = disc_ford)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "Treatment", y = "Mean RII") +
         scale_fill_discrete(name = "disc_ford") +
         theme_bw()
     
     ggplot(data = means_after_disc_interaction, aes(x = disc_ford, y = RII, fill = treatment)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "disc_ford", y = "Mean RII") +
         scale_fill_discrete(name = "treatment") +
         theme_bw()
     
     
     ##intervention effect:
     
     boxplot(d$RII ~ d$independence_timing)
     # this further proves that there are more outliers in the control group, which could skew the results
     
     t.test(formula = d$RII ~ d$independence_timing,
            alternative = "two.sided",
            paired = FALSE,   
            var.equal = TRUE,
            conf.level = 0.95)
     
     
     cohen_timing <- rstatix::cohens_d(d, 
                                RII ~ independence_timing, 
                                paired = FALSE, ci = TRUE,
                                conf.level = 0.95,
                                ci.type = "norm",
                                nboot = 1000)
     
     cohen_treatment <- rstatix::cohens_d(d, 
                                       RII ~ treatment, 
                                       paired = FALSE, ci = TRUE,
                                       conf.level = 0.95,
                                       ci.type = "norm",
                                       nboot = 1000)
     
   
     # d_2 <- d
     # d_2$treatment <- gsub("0", "3", d_2$treatment)
     # table(d_2$treatment)
     # 
      model <- lm(RII ~ treatment * independence_timing, data = d)
      summary(model)
     # 
     
     
     d$treatment <- as_factor(d$treatment)
     d$independence_timing <- as_factor(d$independence_timing)
     
     interaction <- aov(RII ~ treatment * independence_timing, data = d)
     # interaction <- aov(RII ~ treatment * independence_timing + Error(vedidk/independence_timing), data = d_2)
     summary(interaction)
     heplots::etasq(interaction, anova = TRUE)
     
     # m <- left_join(d_before, d_after, by = join_by(treatment, treatment_year, career_start_year,
     #                                                career_lenght, pubs_total, ws_pubs, interdisc_proportion, grants, gender), suffix = c(".x", ".y"))
     # cor(d$RII, d$independence_timing)
     # 
     
     
     
     # lm_interaction <- lm(RII ~ treatment * independence_timing, data = d)
     # rsq::rsq(lm_interaction)
     # rsq::rsq(lm_interaction, type = "marginal")
     # rsq::r.squared(lm_interaction, by_term = TRUE)
     
     ###calculating r-squared - generated by chatGPT, so not sure this is correct
     
     n <- nrow(d)
     p <- length(coef(interaction))
     SSresid <- sum(resid(interaction)^2)
     SStotal <- sum((na.omit(d$RII) - mean(d$RII, na.rm = TRUE))^2)
     R2 <- 1 - (SSresid / SStotal)
     adjR2 <- 1 - ((1 - R2) * (n - 1) / (n - p - 1))
     
     # would be interesting see generate R2 for each variable{term rather tahn for the model as a whole.
     
     ### visualisations
     
     
     means <- aggregate(RII ~ treatment + independence_timing, data = d, FUN = mean)
     
    
    ggplot(means, aes(x = independence_timing, y = RII, color = treatment)) +
                geom_point() +
                geom_line() +
                labs(x = "Independence Timing", y = "RII", color = "Treatment") +
                theme_bw()
     
    
     ggplot(data = interaction, aes(x = treatment, y = RII, fill = independence_timing)) +
         geom_violin(scale = "width") +
         scale_fill_discrete(name = "Independence Timing", labels = c("After intervention", "Before intervention")) +
         xlab("Treatment Group") +
         ylab("RII Score") +
         ggtitle("Violin Plot of RII Scores by Treatment and Time") +
         theme_bw()
     
     # ggplot(data = interaction, aes(x = treatment, y = RII, fill = factor(independence_timing, levels = rev(levels(interaction$independence_timing))))) +
     #     geom_violin() +
     #     labs(x = "Treatment group", y = "RII scores", fill = "Time") +
     #     scale_fill_discrete(labels = c("Before intervention", "After intervention"))
     # 
     # 
     # 
     # 
     # 
     # ggplot(data = d_before, aes(x = treatment, y = response_variable)) +
     #     geom_boxplot() +
     #     labs(title = "Distribution of response variable across treatment groups",
     #          x = "Treatment group", y = "Response variable")
     # 
     # 
     # 
     # 
     # 
     # 
     # d_before_summary <- d_before %>% 
     #     group_by(treatment) %>% 
     #     summarise(mean_response_variable = mean(response_variable),
     #               se_response_variable = sd(response_variable) / sqrt(n()))
     # 
     # ggplot(data = d_before_summary, aes(x = treatment, y = mean_response_variable)) +
     #     geom_bar(stat = "identity", fill = "blue") +
     #     geom_errorbar(aes(ymin = mean_response_variable - se_response_variable,
     #                       ymax = mean_response_variable + se_response_variable),
     #                   width = 0.2) +
     #     labs(title = "Means of response variable across treatment groups",
     #          x = "Treatment group", y = "Mean response variable")
     # 
     # 
     
     }


