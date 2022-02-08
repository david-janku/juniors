#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tokens
#' @return
#' @author hlageek
#' @export
create_dfm <- function(tokens_input) {

    
    tokens.dfm <- quanteda::dfm(tokens_input, tolower = FALSE)
    
    dfm_trim(tokens.dfm, min_termfreq = 10, termfreq_type="count")

}
