#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_topic_model <- function(topic_model_input) {

    
    dfm <- dfm[which(rowSums(dfm) > 0),]
    topic_model_input <- convert(dfm,to="topicmodels")
    # dim(topic_model_input)
    lda.model <- LDA(topic_model_input,k = 60, control = list(seed = 123),alpha = 0.1, beta = 0.01 , verbose=1) 
    
    lda.matrix <- posterior(lda.model,dfm)$topics
    dim(lda.matrix) 
    
    data <- as_tibble(t(lda.matrix))

    # write.csv2(data, file = here::here("data", "derived", "topic_model_matrix.csv"))
    # write_excel_csv2(data, here("data", "derived", "tmm.csv"), na = "NA")
    # save(lda.matrix, file = paste(here("data", "derived", c("topic_dist.Rdata")), sep=""))

}    
    