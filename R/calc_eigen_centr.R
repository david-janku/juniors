#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_eigen_centr <- function(graph) {

    ecentr <- eigen_centrality(
        graph,
        directed = FALSE,
        scale = TRUE,
        weights = E(graph)$weight,
        options = arpack_defaults
    )
    
    centr <- enframe(ecentr$vector, name = "author", value = "eigen_ctr")
    
    sup <- centr %>% 
        filter(author=="klapilovak")
    
    
    sup$eigen_ctr

}
