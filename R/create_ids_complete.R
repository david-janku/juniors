#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
create_ids_complete <- function(ids) {

    ids_complete <- as_tibble(ids) %>% 
        select(vedidk_core_researcher, vedoucí.vedidk) %>% 
        filter(!is.na(vedoucí.vedidk)) %>% 
        filter(!is.na(vedidk_core_researcher))
        
    #adding info about discipline (based on which disciplinary committee evaluated the grant) and year the grant project started
    con <-  DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))
    
    DBI::dbListTables(con)
    
    GJ <- DBI::dbReadTable(con, "cep_details") %>%
        filter(program_kod == "GJ") %>% 
        select(kod, disc, ford, year_start) %>% 
        tidyr::unite(disc_ford, c(disc, ford), na.rm = TRUE) %>%
        as_tibble
    
   
    GJ$disc_ford[GJ$disc_ford %in% c("BA", "BB")] <- 10100 #BD
    GJ$disc_ford[GJ$disc_ford %in% c("IN", "BC", "BD", "AF")] <- 10200
    GJ$disc_ford[GJ$disc_ford %in% c("BE", "BM", "BF", "BG", "BK", "BL", "BH", "BI","BN")] <- 10300
    GJ$disc_ford[GJ$disc_ford %in% c("CC", "CA", "CH", "CF", "CD", "CG", "CB")] <- 10400
    GJ$disc_ford[GJ$disc_ford %in% c("DA", "DB", "DC", "DE", "DG", "DO", "DK", "DL", "DM", "DI", "DJ")] <- 10500
    GJ$disc_ford[GJ$disc_ford %in% c("EA", "EB", "EE", "CE", "EB", "BO", "EF", "EG", "DA", "EH")] <- 10600
    
    GJ$disc_ford[GJ$disc_ford %in% c("JN", "JM", "GB", "AL", "JO")] <- 20100
    GJ$disc_ford[GJ$disc_ford %in% c("JA", "JB", "JW", "JD", "JC")] <- 20200
    GJ$disc_ford[GJ$disc_ford %in% c("JR", "JT", "JQ", "BJ", "JU", "JV", "JF", "JS", "JL")] <- 20300
    GJ$disc_ford[GJ$disc_ford %in% c("CI")] <- 20400
    GJ$disc_ford[GJ$disc_ford %in% c("JP", "JG", "JJ", "JH", "JI", "JK")] <- 20500
    GJ$disc_ford[GJ$disc_ford %in% c("FS")] <- 20600
    GJ$disc_ford[GJ$disc_ford %in% c("DH", "JE", "JT", "JP", "JQ")] <- 20700
    GJ$disc_ford[GJ$disc_ford %in% c("EI")] <- 20800
    GJ$disc_ford[GJ$disc_ford %in% c("EI")] <- 20900
    GJ$disc_ford[GJ$disc_ford %in% c("JJ")] <- 21000
    GJ$disc_ford[GJ$disc_ford %in% c("GM", "JI", "KA")] <- 21100
    
    GJ$disc_ford[GJ$disc_ford %in% c("EB", "EC", "FH", "FR", "ED")] <- 30100 #FP
    GJ$disc_ford[GJ$disc_ford %in% c("FA", "FB", "FC", "FD", "FF", "FG", "FI", "FJ", "FK", "FL", "FO", "FE")] <- 30200 #not added FH, FP
    GJ$disc_ford[GJ$disc_ford %in% c("FN", "FM", "DN", "AQ", "AK")] <- 30300 #FL, FP 
    GJ$disc_ford[GJ$disc_ford %in% c("EI")] <- 30400
    GJ$disc_ford[GJ$disc_ford %in% c("FP")] <- 30500
    
    GJ$disc_ford[GJ$disc_ford %in% c("GD", "GK", "GL", "DF", "GE", "GF", "GC")] <- 40100 
    GJ$disc_ford[GJ$disc_ford %in% c("GG", "GH", "GI")] <- 40200
    GJ$disc_ford[GJ$disc_ford %in% c("GJ")] <- 40300 
    GJ$disc_ford[GJ$disc_ford %in% c("EI", "GM")] <- 40400
    GJ$disc_ford[GJ$disc_ford %in% c("GM")] <- 40500
    
    GJ$disc_ford[GJ$disc_ford %in% c("AN")] <- 50100 
    GJ$disc_ford[GJ$disc_ford %in% c("AH", "GA")] <- 50200
    GJ$disc_ford[GJ$disc_ford %in% c("AM")] <- 50300 
    GJ$disc_ford[GJ$disc_ford %in% c("AO", "AC")] <- 50400
    GJ$disc_ford[GJ$disc_ford %in% c("AG")] <- 50500
    GJ$disc_ford[GJ$disc_ford %in% c("AD", "AE")] <- 50600
    GJ$disc_ford[GJ$disc_ford %in% c("DE", "AP")] <- 50700 #AO
    GJ$disc_ford[GJ$disc_ford %in% c("AJ", "AF")] <- 50800
    GJ$disc_ford[GJ$disc_ford %in% c("AK")] <- 50900
    
    GJ$disc_ford[GJ$disc_ford %in% c("AB", "AC")] <- 60100 
    GJ$disc_ford[GJ$disc_ford %in% c("AI", "AJ")] <- 60200
    GJ$disc_ford[GJ$disc_ford %in% c("AA")] <- 60300 
    GJ$disc_ford[GJ$disc_ford %in% c("AL")] <- 60400
    
    
    
    #here needs to be code that will, in the GJ table, translate old discipline tags to ford discipline tags - here is manual https://www.isvavai.cz/dokumenty/Prevodnik_oboru_Frascati_v2.pdf
    
       
         
    GJ_vector <- GJ$kod %>% 
        as_tibble() %>% 
        as_vector() %>% 
        as_tibble() %>% #from here below it was added - I should check that it does change any further results 
        distinct() %>% #here
        as_vector()
        
    resitele <- DBI::dbReadTable(con, "cep_investigators") %>%
        filter(kod %in% GJ_vector) %>%
        filter(vedidk %in% ids_complete$vedidk_core_researcher) %>%
        select(kod, vedidk)
    
    resitele_complet <- left_join(resitele, GJ, by = "kod")
    resitele_complet$vedidk <- as.integer(resitele_complet$vedidk)
    
    ids_complete <- left_join(ids_complete, resitele_complet, by=c("vedidk_core_researcher" = "vedidk"))
    
    #cleaning start year info and selecting only the first grant the researcher received 
    
    ids_complete$year_start <- substring(ids_complete$year_start, 7)  
    ids_complete$year_start <- as.numeric(ids_complete$year_start)
    
    ids_complete <- ids_complete %>% 
        group_by(vedidk_core_researcher) %>% 
        slice(which.min(year_start)) %>% 
        ungroup()
    
}
