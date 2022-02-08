extract_corpus <- function(texts) {
    
    texts %>% 
        pull(pubs_title) %>% 
        tolower()
    
}