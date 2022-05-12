#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param one_author
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_edgelist <- function(one_author) {

   
    # split_authors <- strsplit(as.character(one_author$list), ':')
    # 
    # dat <- enframe(split_authors) %>% unnest(cols = "value")
    
        one_author %>% group_by(id_unique) %>% 
        right_join(one_author, by = "id_unique") %>% 
        filter(id_helper.x != id_helper.y) %>% 
        mutate(from = pmin(id_helper.x, id_helper.y),
               to = pmax(id_helper.x, id_helper.y)) %>% 
        select(pub_id = id_unique, from, to) %>% 
        distinct() %>% 
        # ungroup() %>% 
        
        # mutate(from = tolower(stringi::stri_trans_general(from, "latin-ascii")),
        #        to = tolower(stringi::stri_trans_general(to, "latin-ascii"))) %>% 
        # mutate(from = str_trim(str_replace(from, "(.*), (.?){1}.*", "\\1\\2")),
        #        to = str_trim(str_replace(to, "(.*), (.?){1}.*", "\\1\\2"))) %>% 
        
        relocate(pub_id, .after = last_col()) %>% 
        group_by(from, to) %>% 
        count(name = "weight") %>% 
        ungroup()
    
}
