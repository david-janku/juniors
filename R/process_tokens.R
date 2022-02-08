#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param corpus
#' @return
#' @author hlageek
#' @export
process_tokens <- function(corpus, texts) {

    tokens <-  tokens(corpus, what = "word", 
                      remove_numbers = F, remove_punct = T,
                      remove_symbols = F)
    tokens <-  tokens_wordstem(tokens, language="english")
    stopwords("english")
    sw = unique(c( quanteda::stopwords("english"),"also","e.g", "can","includ","said","first","wherein","other","made","make", "later",  "copyright", "fig", "figur", "tabl", "description", "describ"))
    tokens =  tokens_select(tokens, sw,  selection = "remove")
    
    names(tokens) = texts$ID_core_pubs

    tokens
}
