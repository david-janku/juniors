#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param one_author
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_ind_pubs <- function(one_author) {

    ind_pubs <- filter(one_author, !str_detect(one_author$list, "Klapilová, Kateřina"))
    
    rpubs = nrow(ind_pubs)/nrow(one_author)

}
