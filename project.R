library(tm)
library(ngram)

count_words <- function(x) {
  s <- gsub(' {2,}',' ', x)
  length(strsplit(s,' ')[[1]])
}

#load 
corpusPath <- "Coursera-SwiftKey/final/en_US"
source <- DirSource(corpusPath)
myCorpus <- Corpus(source)

myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# Get a sample of lines from one of the file
doc <- myCorpus[[1]]$content
lines <- sample(seq(1,length(doc)), 0.01 * length(doc))
line <- do.call(paste, as.list(doc[lines]))

ng <- ngram(line, n=3)
p_table <- get.phrasetable(ng)

predict_word <- function(x) {
  words <- count_words(x)
  z <- p_table[grep(x, lapply(p_table$ngrams, substr, 0, nchar(x)), fixed=TRUE),]
  lapply(strsplit(z[1:3,"ngrams"], " ", fixed=TRUE), function(x) x[words + 1])
}
