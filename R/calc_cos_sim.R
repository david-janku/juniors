#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topic_model
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_cos_sim <- function(topic_model, two_authors) {

    topic_model <- as.matrix(topic_model) 
    cosine_matrix <- lsa::cosine(topic_model) %>% 
        as.data.frame() %>% 
        rownames_to_column("Kód.výsledku") %>% 
        as.data.table() 
    
       
    finaldata <- merge.data.table(two_authors, cosine_matrix, by.y = "Kód.výsledku", by.x = "Kód.výsledku", all = TRUE) #tady páruju ty datasety
    
    finaldata <- filter(finaldata, !duplicated(finaldata$Kód.výsledku))
    
    finaldata <- filter(finaldata, str_detect(finaldata$list, "Binter")) #v tenhle moment mi to hází 21 výsledků i když původne mel mit Binter jen 20, tak nevím co s tim
    
    finaldata$avg <- rowMeans(finaldata[ ,84:140]) #tady jsem si ale musel manualne najit poradi sloupců -> melo by to jit nějak vic automatizovane 
    
    mean(finaldata$avg, na.rm = TRUE)
}
