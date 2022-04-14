library(httr)
body1 <- list(token = "6ee5d1a11081df3986423ec97f0d8d86425efb9e", oblast = "riv", rezim = "filtr-detail", "tv-identifikator" = "1907603")
result <- POST(url = "https://api.isvavai.cz/api.php", body = body1, encode = c("form"))
?POST
content(result)


#install.packages("httr")

#install.packages("xml2")
#install.packages("rjson")
#install.packages("tidyverse")

#install.packages("RJSONIO")

library(httr)
library(tidyverse)
library(rjson)
library(purrr)
library(dplyr)

unnest_all <- function(df) {
    list_columns <- df %>% keep(is.list) %>% names()
    
    if (length(list_columns) == 0) {
        return(df)
    }
    
    for (list_column in list_columns) {
        df <-
            df %>%
            unnest_wider(list_column, names_sep = "_")
    }
    unnest_all(df)
}

#authorization

vedidkFilePath <- "C:\\R\\API\\vedidk_juniori.csv"
vedidk <- read.csv(vedidkFilePath)


for (i in 1:dim(vedidk)[1]) {
    #getting data

    body1 <- list(token = "6ee5d1a11081df3986423ec97f0d8d86425efb9e", oblast = "riv", rezim = "filtr-detail", "tv-identifikator" = vedidk[i,])
    print(paste(i, vedidk[i,]))
    result <- POST(url = "https://api.isvavai.cz/api.php", body = body1, encode = c("form"))

    dataAsRawjson <- content(result, as="text")
    dataAsjson <- as.list(fromJSON(dataAsRawjson))
}

allpubsjs <- toJSON(dataAsjson)
write(allpubsjs, "C:\\R\\API\\allpubsjs.json")



#zkouska nacitani externiho JSON souboru
    #datatest <- fromJSON(file="C:\\R\\API\\juniori_api_final", simplify = TRUE)

#navod z https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/ ale nerozděluje dataset po publikacich, což je problem a nevim jak to udělat
    test_df <- enframe(unlist(dataAsjson$data))
    test_df %>% separate(name, into = c(paste0("x", 1:10)))
    
    rgx_split <- "\\."
    n_cols_max <-
        test_df %>%
        pull(name) %>% 
        str_split(rgx_split) %>% 
        map_dbl(~length(.)) %>% 
        max()
    n_cols_max
    
    nms_sep <- paste0("name", 1:n_cols_max)
    data_sep <-
        test_df %>% 
        separate(name, into = nms_sep, sep = rgx_split, fill = "right")
    
    ##other things that didnt work
    
    #test_df <- map_dfr(datatest$data, as_tibble)
    #annotations_df <- map_dfr(datatest$annotations, as.tibble)
    
    #myData_df %>% 
       # select(-annotations) %>% 
       # bind_cols(annotations_df)
    
    
    #test_df <- as.data.frame(datatest)
    #print(test_data_frame)
     #   fromJSON()
    
    write.csv(data_sep,"C:\\R\\API\\pubs.csv", row.names = TRUE, append = TRUE)
    
    #pubs = data.frame()
    
    ## following code is mostly adapted from https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
    #relevantDataUnprocessed = tibble::as_tibble(datatest) %>%
      
    
    #dataWider <- relevantDataUnprocessed %>%
     #   unnest_wider(datatest) %>%
     #  unnest_all()
    
    #if (dim(relevantDataUnprocessed)[1] != dim(dataWider)[1]) {
     #   stop("incorrectectly widened data. final data probably contains duplicates now.")
    }
    #pubs <- bind_rows(pubs, dataWider)
}