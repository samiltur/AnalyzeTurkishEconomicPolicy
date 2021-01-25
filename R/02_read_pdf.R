here::i_am("R/02_read_pdf.R")

library(here)
library(pdftools)
library(data.table)
library(tm)
library(SnowballC)
library(textreadr)
library(magrittr)
library(stopwords)

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

#directory <- "annual_programs"
files2 <- list.files(path = here(), recursive = T, include.dirs = T, pattern = "pdf$")

textmin <- Corpus(URISource(files2),
                  readerControl = list(reader = readPDF,
                                       language = "tr"))

textmin <- tm_map(textmin, removePunctuation, ucp = TRUE)

opinions.tdm <- TermDocumentMatrix(textmin,
                                   control =
                                     list(
                                          language = "tr",
                                          removePunctuation = TRUE,
                                          stopwords = FALSE,
                                          tolower = TRUE,
                                          stemming = FALSE,
                                          removeNumbers = TRUE,
                                          stripWhitespace = TRUE,
                                          bounds = list(global = c(1, Inf))))

inspect(opinions.tdm[1:100,])
findFreqTerms(opinions.tdm, lowfreq = 10, highfreq = Inf)

findMostFreqTerms(opinions.tdm)

# turn tdm into dense matrix and create frequency vector. 
freq <- (as.matrix(opinions.tdm))
sums <- rowSums(freq)

findFreq <- function(word) 
{
  return(freq[word,])
}

search_terms <- c("yenilenebilir", "rüzgar", "güneş", "nükleer", "hidroelektrik", "jeotermal")
findFreq(search_terms)
sums[search_terms]

findFreq("enerji")
sums["enerji"]

# all words starting with öğr Adjust regex to find what you need.
sums[grep("^öğr", names(sums))]