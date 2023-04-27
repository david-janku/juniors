#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_topic_model_addition <- function(topic_model_input_addition, topic_model_original) {

    
    new_dtm <- convert(topic_model_input_addition,to="topicmodels")
    
    new_topics <- posterior(topic_model_original, new_dtm)$topics
    # dim(new_topics)
    data <- as_tibble(t(new_topics))
    

    # write.csv2(data, file = here::here("data", "derived", "topic_model_matrix.csv"))
    # write_excel_csv2(data, here("data", "derived", "tmm.csv"), na = "NA")
    # save(lda.matrix, file = paste(here("data", "derived", c("topic_dist.Rdata")), sep=""))

}    
    