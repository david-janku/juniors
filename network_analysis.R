
library(tidyverse)
library(dplyr)
library(igraph)
library(targets)
library(DiagrammeR)



#this seems useful: https://www.jessesadler.com/post/network-analysis-with-r/
#and this also seems useful: https://amirhosblog.wordpress.com/2016/09/29/co-authorship-analysis-in-r/
#and this: https://eiko-fried.com/create-your-collaborator-network-in-r/

#creating coauthorship network
oneauth <- read.csv2(here::here("data", "raw", "Binter_pubs.csv"))

split_authors <- strsplit(as.character(oneauth$list), ':')
head(split_authors)

dat <- enframe(split_authors) %>% unnest(cols = "value") #co to dela> vem list a udelej y nej data frame /. defualtne to udela sloupce name a value. fce unnest pak veyme v2ci co jsou v listovym formatu a rozbali je a pokud je tam víc hodnot tak pro ně vytvoři nove řadky 

# radim kod - vyhazuje mi to error se kterým nevím co dělat
 edgelist_df <- dat %>% group_by(name) %>% 
   right_join(dat, by = "name") %>% 
   filter(value.x != value.y) %>% 
   mutate(from = pmin(value.x, value.y),
          to = pmax(value.x, value.y)) %>% 
   select(pub_id = name, from, to) %>% 
   distinct() %>% 
   ungroup() %>% 
     
     mutate(from = tolower(stringi::stri_trans_general(from, "latin-ascii")),
            to = tolower(stringi::stri_trans_general(to, "latin-ascii"))) %>% 
        mutate(from = str_trim(str_replace(from, "(.*), (.?){1}.*", "\\1\\2")),
               to = str_trim(str_replace(to, "(.*), (.?){1}.*", "\\1\\2"))) %>% 
    
     relocate(pub_id, .after = last_col()) %>% 
    group_by(from, to) %>% 
    count(name = "weight") %>% 
 ungroup()
 

 
authorgraph <- graph_from_data_frame(edgelist_df, directed = FALSE) 



#alternative to creating coauthorship network - this worked but I didnt use it - adapted from: https://stackoverflow.com/questions/57487704/how-to-split-a-string-of-author-names-by-comma-into-a-data-frame-and-generate-an

    #AuthorCombinations <- sapply(split_authors,function(x){combn(unlist(x),m = 2)})
    #AuthorEdges <- rapply(AuthorCombinations,unlist)
    #names(AuthorEdges) <- NULL

    #AuthorEdges <- trimws(AuthorEdges)

    #AuthorGraph <- graph(AuthorEdges, directed = FALSE)


#plotting the graph
par("mar")       #this was to prevent one error: https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
par(mar=c(1,1,1,1))
set.seed(123)
plot(authorgraph)

#taking one subpart of the network and plotting it
Excerpt <- induced_subgraph(authorgraph,c("Binter, Jakub", "Prossinger, Hermann"))
par("mar")
par(mar=c(1,1,1,1))
plot(Excerpt)


#alternative to creating coauthorship network - this didnt work - below trying adapting code from: https://stackoverflow.com/questions/33540449/creating-an-edge-list-from-co-authorship-data


##next steps: check whether the created network actually makes sense and is correct. Also - is it weighted graph or not?

#calculating eigenvector centrality using https://igraph.org/r/doc/eigen_centrality.html
ecentr <- eigen_centrality(
    authorgraph,
    directed = FALSE,
    scale = TRUE,
    weights = E(authorgraph)$weight,
    options = arpack_defaults
)

centr <- enframe(ecentr$vector, name = "author", value = "eigen_ctr")

# centr%>%arrange(desc(eigen_ctr)) 

sup <- centr %>% 
    filter(author=="klapilovak")

supeig <- sup$eigen_ctr

#calculating clustering coefficient of the former supervisor in the ego-network of the researcher: https://igraph.org/r/doc/transitivity.html

list.vertex.attributes(authorgraph)   #from https://stackoverflow.com/questions/20209303/how-to-get-vertex-ids-back-from-graph
which(V(authorgraph)$name == "klapilovak")

clustering <-
    transitivity(
        authorgraph,
        type = c("local"),
        vids = which(V(authorgraph)$name == "klapilovak"),
        weights = E(authorgraph)$weight,
        isolates = c("NaN", "zero")
)

clustering <-
    transitivity(
        authorgraph,
        type = c("local"),
        vids = NULL,
        weights = E(authorgraph)$weight,
        isolates = c("NaN", "zero")
    )

centr_full <- centr %>% bind_cols("transitivity" = clustering)

network_independence <- function(supeig, clustering){((1-supeig)+clustering)/2}

#vypočtení skoru independence z coauthorské síte
centr_ind <- centr_full %>% 
    mutate(ind = network_independence(eigen_ctr, transitivity))

#checking robustness
    #centr_ind %>% arrange(ind)
    #centr_ind %>% arrange(desc(ind))
    #cor(centr_full$transitivity, centr_full$eigen_ctr)

# treti cast indikatoru: pocet publikaci daneho vyzkumnika bez vedouciho jako spoluautora deleno poctem vsech jeho publikaci

ind_pubs <- filter(oneauth, !str_detect(oneauth$list, "Klapilová, Kateřina"))

rpubs = nrow(ind_pubs)/nrow(oneauth)

##upravit jmena uz v puvodnim datasetu a tady dat to jmeno vecouciho


#vypočtení tematickeho překryvu
##ideálně by se dopic model mel dělat ze všech publikací všech autorů a jejich vedoucích a pak az z toho kompletního modelu tahat ta témata
###je potreba pracovat s unikatnima publikacema --> takze jeste chybi krok ktery protridi vsechny publikace autora a vedouciho a vyhodi ty ktere jsou stejne


library(data.table)
require(data.table)
root.direct = here::here()
text = fread(here::here("data", "raw", 'Binter_Kvapilova_pubs.csv'))
all.text = paste(text$pubs_title_eng,
                 text$abstract, 
                 str_replace_all(text$keywords, "[:;,]", " "), sep = " ")
corpus = tolower(all.text)

library(quanteda)
tokens <-  tokens(corpus, what = "word", 
                  remove_numbers = T, remove_punct = T,
                  remove_symbols = F, remove_hyphens = F)
tokens <-  tokens_wordstem(tokens, language="english")
stopwords("english")
sw = unique(c(stopwords("english"),"also","e.g", "can","includ","said","first","wherein","other","made","make", "later",  "copyright", "fig", "figur", "tabl", "description", "describ"))
tokens = tokens_select(tokens, sw,  selection = "remove")

names(tokens) = text$ID_core_pubs

tokens.dfm = dfm(tokens, tolower = FALSE)
dfm = dfm_trim(tokens.dfm, min_termfreq = 3, termfreq_type="count") #tady asi dává smysl dát tam nějaké nízké číslo, protože sposutu autorů nebude mít zas tolik publikací a pokud se i třeba jen 10 % svých publikací (což můžou být třeba jen  publikace jako u tohoto autora) odlišují od vedoucího, chcem to zachytit) 
dim(dfm) 
# library(tidyverse)
# tidytext::tidy(dfm) %>%
#   group_by(term) %>%
#  summarize(count = sum(count)) %>%
#  arrange(count) %>%
#  view()
     
library("topicmodels")
library("ggplot2")
library("scales")
library("ldatuning")
set.seed(123)

dfm <- dfm[which(rowSums(dfm) > 0),] #v tomto momentě zmizí 10 datapointů protože mají nulovou hodnotu (tzn používaly pouze "stopwords? to je nějaké divné) 
dim(dfm)
dtm <- convert(dfm,to="topicmodels")
dim(dtm)

result <- FindTopicsNumber(
    dtm,
    topics = c(2, 5, 10, 15, 30),
    metrics = c("Griffiths2004", "CaoJuan2009", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE
)

FindTopicsNumber_plot(result)

library("topicmodels")
library("tidytext")
library("tm")

dfm = dfm[which(rowSums(dfm) > 0),]
dtm = convert(dfm,to="topicmodels")
dim(dtm)
lda.model = LDA(dtm,k = 10, control = list(seed = 123),alpha = 0.1, beta = 0.01 , verbose=1) 
rowSums(lda.matrix)

lda.matrix = posterior(lda.model,dfm)$topics
dim(lda.matrix) 

library(data.table) 
library(ggplot2)

library(lsa)
data <- as.data.frame(t(lda.matrix))
data <- as.matrix(data)


##1. zpusob vypoctu indikatoru -> cosinova vzdalenost publikaci daneho autora od publikaci jeho vedouciho  
cosine_matrix <- lsa::cosine(data)

#zpracování souboru a merge do jedné tabulky
data[,-1] %>% dim()

data_test <- cosine_matrix %>% 
    as.data.frame() %>% 
    rownames_to_column("name") %>% 
    as_tibble() %>%
    inner_join(enframe(all.text, value = "title") %>% mutate(name = as.character(name)))

text <- as.data.table(text)
data_test <- as.data.table(data_test)

data_test$name <- as.integer(data_test$name)

finaldata <- merge.data.table(text, data_test, by.y = "name", by.x = "ID_core_pubs", all = TRUE) #tady páruju ty datasety


# následující funkce se snaží 1) udělat průměr ze všech cosinů dané publikace se všemi publikacemi vedoucího 
finaldata <- filter(finaldata, finaldata$author==1)

finaldata$avg <- rowMeans(finaldata[ ,95:151]) #tady jsem si ale musel manualne najit poradi sloupců -> melo by to jit nějak vic automatizovane 

# tady počítám průměr všech přechozích průměrů, což by měl být nejspíš ta finální metrika -> jedná se o průměrnou cosine similarity všech publikací daného autora od všech publikací jeho vedoucího (čím menší, tím více je autor nezávislý, čím větší tím méně je nezávislý) )

topic_independence_cosine = mean(finaldata$avg, na.rm = TRUE)

# vypocet celého indexu nezávislosti 

RII_1 = ((1-supeig)+clustering+(1-topic_independence_cosine))/3

##2. zpusob vypoctu indikatoru -> pocet temat ve kterých je autor jedinecne zastoupen

topics <- as_tibble(t(data))
colnames(topics) <- paste0("topic_", colnames(topics))


two_auth <- read.csv2(here::here("data", "raw", "Binter_Kvapilova_pubs.csv"))

split_authors2 <- strsplit(as.character(two_auth$list), ':')
head(split_authors2)

dat2 <- enframe(split_authors2) %>% unnest(cols = "value") #co to dela> vem list a udelej y nej data frame /. defualtne to udela sloupce name a value. fce unnest pak veyme v2ci co jsou v listovym formatu a rozbali je a pokud je tam víc hodnot tak pro ně vytvoři nove řadky 


supervised_df <- dat2 %>% group_by(name) %>% 
    right_join(dat2, by = "name") %>% 
    filter(value.x != value.y) %>% 
    mutate(from = pmin(value.x, value.y),
           to = pmax(value.x, value.y)) %>% 
    select(pub_id = name, from, to) %>% 
    distinct() %>% 
    ungroup() %>% 
    mutate(pub_id = as.character(pub_id)) %>% 
    mutate(from = tolower(stringi::stri_trans_general(from, "latin-ascii")),
           to = tolower(stringi::stri_trans_general(to, "latin-ascii"))) %>% 
    mutate(from = str_trim(str_replace(from, "(.*), (.?){1}.*", "\\1\\2")),
           to = str_trim(str_replace(to, "(.*), (.?){1}.*", "\\1\\2"))) %>% 
    group_by(pub_id) %>% 
    summarize(supervised = list(from, to)) %>% 
    mutate(supervised = map_lgl(supervised, ~ifelse("klapilovak" %in% .x&"binterj" %in% .x, TRUE, FALSE))) %>% 
    ungroup() %>% 
    
filter(supervised == TRUE) %>% 
    distinct()


topics %>% 
    mutate(across(starts_with("topic"), ~ifelse(.x>0.1, 1, 0))) %>% 
    rownames_to_column("pub_id") %>% 
    left_join() %>% 
filter(from %in% c("kvapilovak", "binterj")|to %in% c("kvapilovak", "binterj") ) %>%
 
view()


mutate(supervised = case_when(from == "kvapilovak"~TRUE, 
                              to == "kvapilovak"~TRUE 
                              )) %>%  
                                group_by(pub_id) %>% 
                                slice(n=1) %>% 
                                ungroup() %>% 
    view()

rm(topics)



all_pubs <- as_tibble(t(data)) %>% 
    rownames_to_column("pub_id") %>% 
    mutate(pub_id = as.integer(pub_id)) %>% 
    left_join(two_authors %>% 
                   select(pub_id = ID_core_pubs, list)) 


binter_pubs <- filter(all_pubs, str_detect(all_pubs$list, "Binter")) %>% 
    mutate(author = "binter")

sup_pubs <- filter(all_pubs, str_detect(all_pubs$list, "Klapilová")) %>% 
    mutate(author = "klap")

pubs <- bind_rows(binter_pubs, sup_pubs)


#tahle funkce říká: pokud v daném tématu nemá sledovaný výzkumník ani jednu publikaci s loadingem  více než 0,1, pak mi vytiskni "0", pokud má alespoň jednu, ale zároveň i jeho supervisors má alsepoň jednu, tak mi vytiskni "2", a pokud má alespoň jednu ale supervisor nemá žádnou, tak vytiskni "1". Takhle funkce bohužel není škálovatelná, takže se bude muset přepsat.   
topics$topic_one <- if(sum(ifelse(data[1,1:20]>0.1, 1, 0))<1){print("0")
} else if(sum(ifelse(data[1,21:77]>0.1, 1, 0))>1){print("2")
} else {
    print("1")
}

topics$topic_two <- if(sum(ifelse(data[2,1:20]>0.1, 1, 0))<1){print("0")
} else if(sum(ifelse(data[2,21:77]>0.1, 1, 0))>1){print("2")
} else {
    print("1")
}

topics$topic_three <- if(sum(ifelse(data[3,1:20]>0.1, 1, 0))<1){print("0")
} else if(sum(ifelse(data[3,21:77]>0.1, 1, 0))>1){print("2")
} else {
    print("1")
}

topics$topic_four<- if(sum(ifelse(data[4,1:20]>0.1, 1, 0))<1){print("0")
} else if(sum(ifelse(data[4,21:77]>0.1, 1, 0))>1){print("2")
} else {
    print("1")
}

topics$topic_five <- if(sum(ifelse(data[5,1:20]>0.1, 1, 0))<1){print("0")
} else if(sum(ifelse(data[5,21:77]>0.1, 1, 0))>1){print("2")
} else {
    print("1")
}

topics <- rm(topics$topics)

topic_independence_categories = sum(ifelse(topics==1, 1, 0))/(sum(ifelse(topics==2, 1, 0)))+(sum(ifelse(topics==1, 1, 0)))

#podle https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0202712#sec002 se topic independence categories násobí 2x  
RII_2 = ((1-supeig)+clustering+rpubs+topic_independence_categories*2)/4

