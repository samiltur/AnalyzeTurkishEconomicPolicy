my_packages <- c("here", "pdftools", "data.table", "tm", "SnowballC", 
                 "textreadr", "magrittr", "stopwords", "tesseract")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org")

here::i_am("R/02_read_pdf.R")

library(here)
library(pdftools)
library(data.table)
library(tm)
library(SnowballC)
library(textreadr)
library(magrittr)
library(stopwords)
library(tesseract)

if (file.exists("freq.csv"))
{
  table(count.fields('freq.csv',sep=','))
  freq2 <- read.csv("freq.csv", row.names = NULL, header = T)

  freq2tt <- t(freq2)
  freq_new <- freq2tt[-1,]
  colnames(freq_new) <- freq2tt[1,]
  rownames(freq_new) <- sub("^X", "", rownames(freq_new))
  f3 <- gsub(" ", "", freq_new, fixed = TRUE)
  freq <- matrix(, nrow = nrow(f3), ncol = ncol(f3))
  for (i in 1:ncol(f3))
  {
    
    f0 <- as.integer(f3[,i])
    freq[,i] <- f0
  }
  
  class(freq[2,5])
  freq[5,2]
  rownames(freq) <- rownames(f3)
  colnames(freq) <- colnames(f3)
  freq <- as.matrix(freq)
  
  class(freq)
  
}else
{
  if (!("tur" %in% tesseract_info()$available))
  {
    tesseract_download("tur")
  }
  
  
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
    ocr = FALSE
  )
  colnames(programs) <- c("doc_id", "text")
  
  for (i in 1:nrow(programs))
  {
    programs$doc_id[[i]] <-  unlist(strsplit(programs$doc_id[[i]], "_"))[1]
  }
  
  applyOcr <- function(program_id)
  {
    pngfiles <- pdftools::pdf_convert(here(directory, program_id),
                                      format = "png",
                                      pages = NULL,
                                      dpi = 600,
                                      antialias = TRUE,
                                      filenames = "page_%04d.%s",
                                      verbose = TRUE)
    
    program <- ocr(pngfiles, tesseract("tur"))
    unlink(pngfiles, TRUE, TRUE, TRUE)
    program <- textshape::combine(program)
    program <- gsub("\r?\n|\r", " ", program)
    df <- data.frame(doc_id = unlist(strsplit(program_id, "_"))[1], text = program)
    return(df)
  }
  a <- unlist(strsplit(programs$doc_id[[22]], "_"))[1]
  for (i in 1:nrow(programs))
  {
    if(nchar(programs$text[[i]]) == 0)
    {
      df <- applyOcr(list.files(here(directory), pattern = paste0(programs$doc_id[[i]],"_*")))

      programs <- rbind(programs, df)
    }
  }
  
  programs[programs==""] <- NA
  programs <- na.omit(programs)
  programs <- programs[order(programs$doc_id),]
  rownames(programs) <- 1:nrow(programs)
  
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
  write.csv(freq, "freq.csv", quote = FALSE)
  freq <- t(freq)
}

sums <- colSums(freq)

findFreq <- function(word) 
{
  return(freq[,word])
}

