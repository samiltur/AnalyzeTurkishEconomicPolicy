here::i_am("R/02_read_pdf.R")

library(conflicted)
library(here)
library(pdftools)
library(data.table)
library(tm)
library(SnowballC)
library(textreadr)
library(magrittr)
library(stopwords)

stopwords::stopwords_getlanguages(source = "stopwords-iso")

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

directory <- "annual_programs"

programs <- read_dir(
  path = here(directory),
  pattern = NULL,
  doc.col = "doc_id",
  all.files = FALSE,
  recursive = FALSE,
  ignore.case = FALSE,
  verbose = TRUE,
  skip = 0,
  remove.empty = TRUE,
  trim = TRUE,
  combine = TRUE,
  format = FALSE,
  ocr = TRUE
)
colnames(programs) <- c("doc_id", "text")
textmin <- Corpus(DataframeSource(programs))

textmin <- tm_map(textmin, removePunctuation, ucp = TRUE)
textmin <- tm_map(textmin, removeWords, stopwords::stopwords("tr", source = "stopwords-iso"))

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