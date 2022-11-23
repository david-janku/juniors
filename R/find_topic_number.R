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

    result <- FindTopicsNumber(
        topic_model_input,
        topics = c(15, 30, 100),
        metrics = c("Griffiths2004", "CaoJuan2009", "Deveaud2014"),
        method = "Gibbs",
        control = list(seed = 77),
        mc.cores = parallel::detectCores()-1,
        verbose = TRUE
    )
    
    FindTopicsNumber_plot(result)
    

}
