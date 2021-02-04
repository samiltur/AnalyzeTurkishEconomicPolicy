my_packages <- c("here", "rvest")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org")

here::i_am("R/01_download_pdf.R")

library(here)
library(utils)
library(rvest)

root_url <- "https://www.sbb.gov.tr"
annual_programs_url <- paste0(root_url, "/yillik-programlar/")


directory <- "annual_programs"
if (!file.exists(directory)){
  dir.create(file.path(here(), directory))
}

links <- sapply(annual_programs_url, function(x) {x %>% html_session() %>% html_nodes("a") %>% html_attr("href")})
select <- NULL
for (i in 1:length(links)){
  select[i] <-(grepl("programı", tolower(links[i]), fixed = TRUE) || grepl("Programi", links[i], fixed = TRUE))
}
program_links <- links[select]
program_links

for (i in 1:length(program_links)){
  if(!file.exists(here(directory, basename(program_links[i])))){
    download.file(paste0(root_url, program_links[i]), here(directory, basename(program_links[i])), mode = "wb")
  }
}
