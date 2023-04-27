#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_topic_model <- function(topic_model_input, topic_number) {

    
    # s <- topic_number %>% 
    #     filter(Griffiths2004 == max(Griffiths2004), CaoJuan2009 == min(CaoJuan2009), Deveaud2014 == min(Deveaud2014)) %>% 
    #     pull(topics)
    # 
    # 
    s <- topic_number %>%
        filter(Griffiths2004 == max(Griffiths2004)) %>%
        pull(topics)
    
    dtm <- convert(topic_model_input,to="topicmodels")
  
    lda.model <- LDA(dtm,k = s, control = list(seed = 123),alpha = 0.1, beta = 0.01 , verbose=1) 
    
    # lda.matrix <- posterior(lda.model,topic_model_input)$topics
    # dim(lda.matrix) 
    # 
    # data <- as_tibble(t(lda.matrix))
    
    

    # write.csv2(data, file = here::here("data", "derived", "topic_model_matrix.csv"))
    # write_excel_csv2(data, here("data", "derived", "tmm.csv"), na = "NA")
    # save(lda.matrix, file = paste(here("data", "derived", c("topic_dist.Rdata")), sep=""))

}    
    