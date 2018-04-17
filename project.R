library(tm)
library(ngram)

count_words <- function(x) {
  s <- gsub(' {2,}',' ', x)
  length(strsplit(s,' ')[[1]])
}

# Sample n lines from a list of lines
sample_lines(lines, n) {
    lines[sample(length(lines), n)]
}

# Sample n lines from a file  
sample_file <- function(fileName, n) {
    conn <- file(fileName, "r")
    lines <- readLines(conn)
    close(conn)
    sample_lines(lines, n)
}

#load 
corpusPath <- "/Users/a1152802/Projects/Data/corpus"
source <- DirSource(corpusPath)
myCorpus <- Corpus(source)

#clean
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, removePunctuation)
#myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#model
doc <- do.call(paste, as.list(myCorpus))
ng <- ngram(doc, n=3)
p_table <- get.phrasetable(ng)

predict_word <- function(x) {
  words <- count_words(x)
  z <- p_table[grep(x, lapply(p_table$ngrams, substr, 0, nchar(x)), fixed=TRUE),]
  lapply(strsplit(z[1:3,"ngrams"], " ", fixed=TRUE), function(x) x[words + 1])
}


