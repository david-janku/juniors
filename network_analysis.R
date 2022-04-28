
install.packages("igraph")
install.packages("DiagrammeR")

library(tidyverse)
library(dplyr)
library(igraph)
library(DiagrammeR)


#this seems useful: https://www.jessesadler.com/post/network-analysis-with-r/
#and this also seems useful: https://amirhosblog.wordpress.com/2016/09/29/co-authorship-analysis-in-r/
#and this: https://eiko-fried.com/create-your-collaborator-network-in-r/

#creating coauthorship network - this didnt work - below trying adapting code from: https://stackoverflow.com/questions/33540449/creating-an-edge-list-from-co-authorship-data

oneauth <- read.csv2(here::here("data", "raw", "Binter_pubs.csv"))

split_authors <- strsplit(as.character(oneauth$list), ':')
head(split_authors)

dat <- rbind(split_authors)

#this is the kind of structure I need to run this code:
# dat <- rbind(c("Miyazaki T.", "Akisawa A.", "Saha B.B.", "El-Sharkawy I.I.", "Chakraborty A."),
  #           c("Saha B.B.", "Chakraborty A.", "Koyama S.", "Aristov Y.I.", NA),
   #          c("Ali S.M.", "Chakraborty A.", NA, NA, NA))

# loop through all rows of dat (all papers, I presume)
#transformed.dat <- lapply(1:nrow(dat), function(row.num) {
    
    #row.el <- dat[row.num, ] # the row element that will be used in this loop
    
    # number of authors per paper
    #n.authors <- length(row.el[!is.na(row.el)])
    
    # creates a matrix with all possible combinations (play around with n.authors, to see what it does)
    #pairings <- combn(n.authors, 2)
    
    # loop through all pairs and return a vector with one row and two columns
    #res <- apply(pairings, 2, function(vec) {
   #     return(t(row.el[vec]))
   # })
    
    # create a data.frame with names aut1 and aut2
   # res <- data.frame(aut1 = res[1, ],
    #                  aut2 = res[2, ])
                      
    
   # return(res)
#})

#final.dat <- data.table::rbindlist(transformed.dat)

#to make it work, I would need to either transform the split authors list to data.frame, or I would need to find a way how to put NAs into the empty fields in the list) 

#max.authors <- max(sapply(split_authors, length))

#dat <- data.frame(aut1 = res[1, ],
    #              aut2 = res[2, ])

#creating coauthorship network - this worked! - adapted from: https://stackoverflow.com/questions/57487704/how-to-split-a-string-of-author-names-by-comma-into-a-data-frame-and-generate-an

AuthorCombinations <- sapply(split_authors,function(x){combn(unlist(x),m = 2)})
AuthorEdges <- rapply(AuthorCombinations,unlist)
names(AuthorEdges) <- NULL

AuthorEdges <- trimws(AuthorEdges)

AuthorGraph <- graph(AuthorEdges, directed = FALSE)

#plotting the graph
par("mar")       #this was to prevent one error: https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
par(mar=c(1,1,1,1))

plot(AuthorGraph)


#taking one subpart of the network and plotting it
Excerpt <- induced_subgraph(AuthorGraph,c("Binter, Jakub", "Prossinger, Hermann"))
par("mar")
par(mar=c(1,1,1,1))
plot(Excerpt)

##next steps: check whether the created network actually makes sense and is correct. Also - is it weighted graph or not?

##calculating the metrics according to https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0202712#sec002)

#calculating eigenvector centrality using https://igraph.org/r/doc/eigen_centrality.html
ecentr <- eigen_centrality(
    AuthorGraph,
    directed = FALSE,
    scale = TRUE,
    weights = NULL,
    options = arpack_defaults
)

centr <- ecentr %>% 
    as.data.frame() %>% 
    rownames_to_column("name")

sup <- centr %>% 
    filter(centr$name=="KlapilovĂˇ, KateĹ™ina")

supeig <- sup$vector

#calculating clustering coefficient of the former supervisor in the ego-network of the researcher: https://igraph.org/r/doc/transitivity.html

list.vertex.attributes(AuthorGraph)   #from https://stackoverflow.com/questions/20209303/how-to-get-vertex-ids-back-from-graph
which(V(AuthorGraph)$name == "KlapilovĂˇ, KateĹ™ina")

clustering <-
    transitivity(
        AuthorGraph,
        type = c("local"),
        vids = 29,
        weights = NULL,
        isolates = c("NaN", "zero")
)

#vypočtení skoru independence z coauthorské síte
network_independence = ((1-supeig)+clustering)/2

#vypočtení tematickeho překryvu

library(data.table)
require(data.table)
root.direct = here::here()
text = fread(here::here("data", "raw", 'Binter_Kvapilova_pubs.csv'))
all.text = text$pubs_title
corpus = tolower(all.text)

library(quanteda)
tokens <-  tokens(corpus, what = "word", 
                  remove_numbers = F, remove_punct = T,
                  remove_symbols = F, remove_hyphens = F)
tokens <-  tokens_wordstem(tokens, language="english")
stopwords("english")
sw = unique(c(stopwords("english"),"also","e.g", "can","includ","said","first","wherein","other","made","make", "later",  "copyright", "fig", "figur", "tabl", "description", "describ"))
tokens = tokens_select(tokens, sw,  selection = "remove")

names(tokens) = text$ID_core_pubs

tokens.dfm = dfm(tokens, tolower = FALSE)
dfm = dfm_trim(tokens.dfm, min_termfreq = 2, termfreq_type="count") #tady asi dává smysl dát tam nějaké nízké číslo, protože sposutu autorů nebude mít zas tolik publikací a pokud se i třeba jen 10 % svých publikací (což můžou být třeba jen  publikace jako u tohoto autora) odlišují od vedoucího, chcem to zachytit) 
dim(dfm) 
terms = colnames(dfm)
singles = terms[nchar(terms)==1]
terms = terms[nchar(terms)!=1]
doubles = terms[nchar(terms)==2]
terms = terms[nchar(terms)!=2]
C = terms[grep("[0-9][0-9][A-Za-z]",terms,perl = T)]
D = terms[grep("[A-Za-z][0-9][0-9]",terms,perl = T)] 
dfm = dfm[,terms]
dim(dfm) 
save(dfm, file = here::here("data", "derived","bitner_dfm_min5_full_text.Rdata"))

library("topicmodels")
library("ggplot2")
library("scales")
library("ldatuning")
load(here::here("data", "derived", "bitner_dfm_min5_full_text.Rdata"))
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
lda.model = LDA(dtm,k = 5, control = list(seed = 123),alpha = 0.1, beta = 0.01 , verbose=1) 
save(lda.model, file = here::here("data", "derived", "bitner_lda_full_text_5_topics_min5.RDS"))

rowSums(lda.matrix)

lda.matrix = posterior(lda.model,dfm)$topics
dim(lda.matrix) 

save(lda.matrix, file = here::here("data", "derived", "bitner_topic_dist_full_text_5_topics_min5.Rdata"))
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

topics <- c(0)
topics <- as.data.table(topics)

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
RII_2 = ((1-supeig)+clustering+topic_independence_categories*2)/3



# radim kod
# edgelist_df <- dat %>% group_by(name) %>% 
#   right_join(dat, by = "name") %>% 
#   filter(value.x != value.y) %>% 
#   mutate(from = pmin(value.x, value.y),
#          to = pmax(value.x, value.y)) %>% 
#   select(pub_id = name, from, to) %>% 
#   distinct() %>% 
#   ungroup()
# AuthorGraph2 <- graph_from_edgelist(as.matrix(edgelist_df %>% select(-pub_id)), directed = FALSE)

tolower(edgelist_df) #funkce k uprave jmen
remove_punct = T,
remove_symbols = F, remove_hyphens = F