#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
make_topic_model <- function(all_pubs) {

   # text <-  allpubs
   
   # two_authors %>% 
   #      filter(!duplicated(Kód.výsledku)) %>%
   #      as_tibble()
    
    all.text <-  paste(all_pubs$title_eng,
                     all_pubs$abstract_eng,
                     all_pubs$keywords)
                     # str_replace_all(text$keywords, "[:;,]", " "), sep = " ")
    corpus <-  tolower(all.text)
    
    tokens <-  tokens(corpus, what = "word", 
                      remove_numbers = T, remove_punct = T,
                      remove_symbols = F, remove_hyphens = F)
    tokens <-  tokens_wordstem(tokens, language="english")
    stopwords("english")
    sw = unique(c(stopwords("english"),"also","e.g", "can","includ","said","first","wherein","other","made","make", "later",  "copyright", "fig", "figur", "tabl", "description", "describ", "Annotation", "not", "available", "N/A", "XXX", "xxx"))
    tokens = tokens_select(tokens, sw,  selection = "remove")
    
    names(tokens) = all_pubs$id_unique
    
    
    tokens.dfm = dfm(tokens, tolower = FALSE)
    dim(tokens.dfm)
    dfm = dfm_trim(tokens.dfm, min_termfreq = 10, termfreq_type="count") # tady tímto krokem se připravím o 104137-13971= 90000 tokenů, což je cca 90% - je to ok? zda se to jako hodne ---------tady asi dává smysl dát tam nějaké nízké číslo, protože sposutu autorů nebude mít zas tolik publikací a pokud se i třeba jen 10 % svých publikací (což můžou být třeba jen  publikace jako u tohoto autora) odlišují od vedoucího, chcem to zachytit) 
    dim(dfm) #měl bych tady nastavit ze minimalni delka je 2-3 characters, remove symbols a hyphens --> nebo aspon nejaky jako +, =, |, 
    # library(tidyverse)
     tidytext::tidy(dfm) %>%
     group_by(term) %>%
     summarize(count = sum(count)) %>%
     arrange(count) %>%
     view()
    
     terms = colnames(dfm)
     
    #delete single and two letter tokens
     
    singles = terms[nchar(terms)==1]
    terms = terms[nchar(terms)!=1]  #v tomto kroku se odstrani 53 tokenů 
    doubles = terms[nchar(terms)==2]
    terms = terms[nchar(terms)!=2] #tenhle krok odstrani 497 tokenu - not sure we want this?. 
    
    #V tomto kroku tam pak pridaji jeste specificke zkratky chemikalii ktere maji 2 characters: https://bixuansunphd.com/N-R_tutorial.html#overview 
    
    #terms with numbers
    
    C = terms[grep("[0-9][0-9][A-Za-z]",terms,perl = T)] #not sure we want to use this filter
    D = terms[grep("[A-Za-z][0-9][0-9]",terms,perl = T)] #not sure we want to use this filter
    
    C[nchar(C)>=3 & nchar(C)<=9]
    D[nchar(D)<=4 & nchar(D)>=3]
    
    deleted.num = c(C[nchar(C)>=3 & nchar(C)<=9], D[nchar(D)<=4 & nchar(D)>=3])
    terms = terms[!(terms %in% deleted.num)]
    
    #delete all terms starting with purpose
    terms = terms[!(terms %in% terms[grep("purpose:",terms,perl=T)])]
    
    # remove terms with two-digit number or more
    E = unique(c(terms[grep("[0-9][0-9]",terms,perl = T)],terms[grep("[0-9],[0-9]",terms,perl = T)]
                 ,terms[grep("[0-9]-[0-9]",terms,perl = T)], terms[grep("[0-9]\\.[0-9]",terms,perl = T)]))
    E[grep("[A-Za-z]",E,perl = T)]
    terms = terms[!(terms %in% E[-grep("[A-Za-z]",E,perl = T)])] #keep the ones with letters
    
    # remove all units pf distance
    G = c("0.1m",
          terms[grep("[0-9]mm",terms,perl = T)],
          terms[grep("[0-9]mum",terms,perl = T)],
          terms[grep("[0-9]mol",terms,perl = T)],
          terms[grep("[0-9]cm",terms,perl = T)],
          terms[grep("[0-9]nm",terms,perl = T)],
          terms[grep("[0-9]æ¼ã¸²",terms,perl = T)],
          terms[grep("[0-9]angstrom",terms,perl = T)])
    
    terms = terms[!(terms %in% G)]
    
    ## trim the matrix
    
    dfm = dfm[,terms]
    dim(dfm) 
    
    set.seed(123)
    
    dfm <- dfm[which(rowSums(dfm) > 0),] #v tomto momentě zmizí 10 datapointů protože mají nulovou hodnotu (tzn používaly pouze "stopwords? to je nějaké divné) 
    dim(dfm)
    dtm <- convert(dfm,to="topicmodels")
    dim(dtm)
    
    result <- FindTopicsNumber(
        dtm,
        topics = c(30, 100, 200, 300, 500, 1000),
        metrics = c("Griffiths2004", "CaoJuan2009", "Deveaud2014"),
        method = "Gibbs",
        control = list(seed = 77),
        mc.cores = 2L,
        verbose = TRUE
    )
    
    FindTopicsNumber_plot(result)
    
    dfm = dfm[which(rowSums(dfm) > 0),]
    dtm = convert(dfm,to="topicmodels")
    dim(dtm)
    lda.model = LDA(dtm,k = 10, control = list(seed = 123),alpha = 0.1, beta = 0.01 , verbose=1) 
    
    lda.matrix = posterior(lda.model,dfm)$topics
    dim(lda.matrix) 
    
    data <- as.data.frame(t(lda.matrix))

}
