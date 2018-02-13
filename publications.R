###################################
## Big Data HW 1
## Publication Question
## By Blain Morin and Jess Kaminsky
###################################

### Load Required packages

library(stringr)
library(rebus)
library(RCurl)
library(xml2)
library(httr)
library(XML)
library(dplyr)


### Read in csv
NIH = read.csv("NIHHarvard.csv")


###Detects rows where activity starts with T or F
T_F_rows = str_detect(NIH$Activity, pattern = START %R% or("T", "F"))


### Creates new dataframe where the T and F rows are removed
T_F_gone = NIH[!T_F_rows,]


### Creates vector of unique author names
authors = unique(as.character(T_F_gone$Contact.PI...Project.Leader))


### Removes middle names

namepattern = START %R% one_or_more( or(ALPHA, "-", " ")) %R% ", " %R% one_or_more(ALPHA)

crop_authors = str_extract(authors, namepattern)

### Some authors are duplicated because there was a SPACE character at the end of their string
### This removes the duplicates
crop_authors = unique(crop_authors)


### Create a function that retrieves number of pubs

numbpubs = function(author){
  
  url <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=", author,  "[Author] AND Harvard[Affiliation]")
  url <- str_replace_all(url, " ", "%20")
  html = htmlParse(getURL(url),encoding="UTF-8")
  
  page = as(html, "character") ###Converts html to one long string
  page = gsub('\"', "", page)
  
  if (str_detect(page, "No items found") == TRUE) {
    count = 0
  } else {
    zoompage = str_extract(page, pattern = "ncbi_resultcount content=" %R% one_or_more(DIGIT))
    count = as.numeric(str_extract(zoompage, one_or_more(DIGIT)))
  }
  return(count)
  Sys.sleep(2+runif(1)*20)
  
}


### Apply the numpubs funtion over the list of author names
author_pubs = sapply(crop_authors, numbpubs)

### Assemble the vectors into a data frame
forexport = as.data.frame(cbind(Authors = crop_authors, N_Publications =  author_pubs))

### Write our results as a csv
write.csv(forexport, "authorpubs.csv", row.names = FALSE)
