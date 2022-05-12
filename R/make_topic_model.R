#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_topic_model <- function(topic_model_input) {

    result <- FindTopicsNumber(
        topic_model_input,
        topics = c(30, 100, 200, 300, 500, 1000),
        metrics = c("Griffiths2004", "CaoJuan2009", "Deveaud2014"),
        method = "Gibbs",
        control = list(seed = 77),
        mc.cores = parallel::detectCores()-1,
        verbose = TRUE
    )
    
    FindTopicsNumber_plot(result)
    
    dfm = dfm[which(rowSums(dfm) > 0),]
    dtm = convert(dfm,to="topicmodels")
    dim(dtm)
    lda.model = LDA(dtm,k = 10, control = list(seed = 123),alpha = 0.1, beta = 0.01 , verbose=1) 
    
    lda.matrix = posterior(lda.model,dfm)$topics
    dim(lda.matrix) 
    
    data <- as_tibble(t(lda.matrix))

}
