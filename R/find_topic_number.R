#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topic_model_input
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
find_topic_number <- function(topic_model_input) {

    
    dtm <- convert(topic_model_input,to="topicmodels")
    # dim(dtm)
    
    result <- FindTopicsNumber(
        dtm,
        topics = c(15, 30, 60, 100, 500),
        metrics = c("Griffiths2004", "CaoJuan2009", "Deveaud2014"),
        method = "Gibbs",                         #rozhodnout se jestli použít VEM nebo Gibbs
        control = list(seed = 77),
        mc.cores = parallel::detectCores()-1,
        verbose = TRUE
    )
    
    # FindTopicsNumber_plot(result)
    

}
