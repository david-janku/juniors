
install.packages("igraph")

library(tidyverse)
library(dplyr)
library(igraph)

#this seems useful: https://www.jessesadler.com/post/network-analysis-with-r/
#and this also seems useful: https://amirhosblog.wordpress.com/2016/09/29/co-authorship-analysis-in-r/
#and this: https://eiko-fried.com/create-your-collaborator-network-in-r/

#creating coauthorship network - this didnt work - below trying adapting code from: https://stackoverflow.com/questions/33540449/creating-an-edge-list-from-co-authorship-data

oneauth <- read.csv2("C:\\R\\Juniors\\Binter_pubs.csv")

split_authors <- strsplit(as.character(oneauth$list), ':')
head(split_authors)

dat <- rbind(split_authors)

#this is the kind of structure I need to run this code:
# dat <- rbind(c("Miyazaki T.", "Akisawa A.", "Saha B.B.", "El-Sharkawy I.I.", "Chakraborty A."),
  #           c("Saha B.B.", "Chakraborty A.", "Koyama S.", "Aristov Y.I.", NA),
   #          c("Ali S.M.", "Chakraborty A.", NA, NA, NA))

# loop through all rows of dat (all papers, I presume)
transformed.dat <- lapply(1:nrow(dat), function(row.num) {
    
    row.el <- dat[row.num, ] # the row element that will be used in this loop
    
    # number of authors per paper
    n.authors <- length(row.el[!is.na(row.el)])
    
    # creates a matrix with all possible combinations (play around with n.authors, to see what it does)
    pairings <- combn(n.authors, 2)
    
    # loop through all pairs and return a vector with one row and two columns
    res <- apply(pairings, 2, function(vec) {
        return(t(row.el[vec]))
    })
    
    # create a data.frame with names aut1 and aut2
    res <- data.frame(aut1 = res[1, ],
                      aut2 = res[2, ])
                      
    
    return(res)
})

final.dat <- data.table::rbindlist(transformed.dat)

#to make it work, I would need to either transform the split authors list to data.frame, or I would need to find a way how to put NAs into the empty fields in the list) 

max.authors <- max(sapply(split_authors, length))

dat <- data.frame(aut1 = res[1, ],
                  aut2 = res[2, ])

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

##next steps: check whether the created network actually makes sense and is correct

###next steps: calculate the i) the eigenvector centrality of the former supervisor in the co-author ego-network of the researcher, and (ii) the clustering coefficient of the former supervisor in the ego-network of the researcher. (see here for context: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0202712#sec002)
