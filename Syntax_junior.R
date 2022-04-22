# install.packages("cli", repos = "https://cran.rstudio.com/")
# install.packages("dplyr")


#postup na základě tohoto kódu https://bixuansunphd.com/N-R_tutorial.html#overview

# install.packages("data.table")
library(data.table)
require(data.table)
root.direct = here::here()
text = fread(here::here("data", "raw", "data_core_pubs_trim.csv"))
all.text = text$pubs_title
corpus = tolower(all.text)

# install.packages("quanteda")
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
dfm = dfm_trim(tokens.dfm, min_termfreq = 10, termfreq_type="count")
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
save(dfm, file = here::here(root.direct,"dfm_min10_full_text.Rdata"))
# install.packages("topicmodels")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("ldatuning")
library("topicmodels")
library("ggplot2")
library("scales")
library("ldatuning")
load(here::here(root.direct, "dfm_min10_full_text.Rdata"))
set.seed(123)

dfm <- dfm[which(rowSums(dfm) > 0),] #tady v tomto momentě mi zmizí asi 100 datapointů a nevím proč - vytvoří to ale komplikaci níže 
dim(dfm)
dtm <- convert(dfm,to="topicmodels")
dim(dtm)

result <- FindTopicsNumber(
  dtm,
  topics = c(5, 15, 50, 75, 100, 125, 150, 175),
  metrics = c("Griffiths2004", "CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
) #tady jsem vymazal metodu Arun2010, protože mi z nějakého důvodu nefungovala 


FindTopicsNumber_plot(result)

# install.packages("tidytext")
library("topicmodels")
library("tidytext")
library("tm")

dfm = dfm[which(rowSums(dfm) > 0),]
dtm = convert(dfm,to="topicmodels")
dim(dtm)
lda.model = LDA(dtm,k = 15, control = list(seed = 123),alpha = 0.1, beta = 0.01 , verbose=1) 
save(lda.model, file = here::here(root.direct, "lda_full_text_15_topics_min10.RDS"))

rowSums(lda.matrix)

lda.matrix = posterior(lda.model,dfm)$topics
dim(lda.matrix) 

save(lda.matrix, file = here::here(root.direct, "topic_dist_full_text_15_topics_min10.Rdata"))
library(data.table) 
library(ggplot2)

#tady už se odchyluju od toho původního kódu a snažím se spočítat cosine distatnce s použitím loadingů každé publikace ke každému z 15 témat 

# install.packages("lsa")
library(lsa)
# data <- as.matrix(lda.matrix)
data <- as.data.frame(t(lda.matrix))
data <- as.matrix(data)

cosine_matrix <- lsa::cosine(data)

 
data_test[,-1] %>% dim()

data_test <- cosine_matrix %>% 
    as.data.frame() %>% 
    rownames_to_column("name") %>% 
    as_tibble() %>%
    inner_join(enframe(all.text, value = "title") %>% mutate(name = as.character(name)))


#tady jsem narazil na ten výše zmiňovaný problém - mám matici cosinů 465 publikací, ale můj celkový vzorek se kterým se to snažím matchovat má 561 publikací. V matici cosinů má každá publikace "name" které odpovídá ID_core_pubs v tom setu 561 publikací, ale nenašel jsem způsob jak spárovat tyhle dva datasety.

total <- cosine[,0]
total$avg <- rowMeans(cosine) #tady průměruju všechny cosiny od každé publikace zvlášť - průměr možná není to co bych s tím měl dělat, ale potřeboval jsem z toho dostat jedno číslo za každou publikaci abych mohl udělat t-test
total$id <- c(1:465) #tady jsem vzdal párování datasetů a prostě jsem každé publikaci se setu 465 publikací dal ID od 1-465a, bych to konečně mohl aspon nějak spárovat s tím setem 561 publikací


#random poznamky a věci co jsem zkoušel a nefungovali


total$id <- text$ID_recipient 

total$id <- cosine[,0]
cosine <- as.data.frame(cosine)

total[,0]

names(cosine)=c("ID")
names(text)=c("ID")

names(cosine)

names(text)
names(total)

?names

# konec random poznamek


save(total, file = here::here(root.direct, "total.Rdata"))

text <- as.data.table(text)
total <- as.data.table(total)

total2 <- merge.data.table(text, total, by.y = "id", by.x = "ID_core_pubs", all = TRUE) #tady páruju ty datasety


# tady binarizuju celkově 8 řešitelů do 2 skupin (arbitrárně) 
total2$ID_recipient <- replace(total2$ID_recipient, total2$ID_recipient>4,6) 
  
total2$ID_recipient <- replace(total2$ID_recipient, total2$ID_recipient<5, 1) 

total2$ID_recipient <- replace(total2$ID_recipient,total2$ID_recipient>4,0) 

# tady dělám t-test těch 2 skupin
boxplot(total2$avg~total2$ID_recipient)

t.test(total2$avg~total2$ID_recipient, paired=FALSE)

# něco v tom testu vyjde, ale ve skutečnosti to smysl nedává, protože 1) skupiny které porovnávám jsou vybráné arbitrárně a není za tím žádná teorie; 2) nepodařilo se mi zpárovat těchz 465 publikací se skutečnými 561 publikacemi, takže se to spárovalo nesprávně a tedy to není vypovídající 

#test version control
1245