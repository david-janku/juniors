#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param 
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
analyse <- function(full_indicator) {

     ## descriptives
    
     d <- full_indicator
    
     d_before <- d %>% filter(independence_timing == "before_intervention") %>% select(-vedidk, -vedidk_treatment, -pub_table, -edgelist, -graph, -independence_timing, -sup_name, -sup_vedidk)
     d_after <- d %>% filter(independence_timing == "after_intervention") %>% select(-vedidk, -vedidk_treatment, -pub_table, -edgelist, -graph, -independence_timing, -sup_name, -sup_vedidk)
     
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
     
     write.csv(table_before_2, file = here::here("data", "derived", "table_before_2.csv"))
     
     
     table_sum_before <- by(d_before, INDICES = d_before$treatment, FUN = summary)
     table_sum_before <- as_tibble(do.call(cbind, table_sum_before))
     
     
    
     ## hypothesis testing
     
    
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
     table(d_before$disc_ford, by = d_before$treatment)
     
     
     disc_before <- aov(RII ~ disc_ford, data = d_before)
     summary(disc_before)
     
     means_before_disc <- aggregate(RII ~ disc_ford, data = d_before, FUN = mean)
     
     
     ggplot(data = means_before_disc, aes(x = disc_ford, y = RII)) +
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "disc_ford", y = "Mean RII") +
         theme_bw()
     
     tukey_results <- TukeyHSD(disc_before)
     tukey_results
     
     
     
     
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
     table(d_after$disc_ford, by = d_after$treatment) 
     
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


