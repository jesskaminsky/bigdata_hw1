#PHP2650 - HW1
#Question 1 - S & P 100 Stock Prices
#Jess Kaminsky + Blain Morin

#Load required packages
library(XML)
library(httr)
library(RCurl)
library(RSelenium)

#Download stock data from Wikipedia
link = "http://en.wikipedia.org/wiki/S%26P_100"
doc <- readHTMLTable(rawToChar(GET(link)$content))

stock_table <- doc[[3]]
stock_table$Symbol <- as.character(stock_table$Symbol)
stock_table$Symbol[17] <-"BRK-B"

#Extract symbol names from downloaded stock data
symbol_names <- stock_table$Symbol

#Start selenium server and browser to download data
rs <- rsDriver(extraCapabilities = list(
  chromeOptions = 
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = "C:/Users/jkamins1/Desktop/stockcsv"
    )
    )
))

rsc <- rs$client

#Funciton for downloading stock data from yahoo based on Symbol
downloadHPYahoo <- function(symbol) {
  Sys.sleep(10+runif(1)*20)
  rsc$navigate(paste0("https://finance.yahoo.com/quote/", symbol, "/history?p=", symbol))
  hpcsv <- rsc$findElement("xpath", "//a[contains(@download, 'csv')]")
  hpcsv$clickElement()
}

#Download data for stock symbols extracted from wikipedia
for (s in symbol_names) {
  downloadHPYahoo(s)
}

#Add column to csv files
files <- list.files(path="stockcsv", pattern="*.csv", full.names=T, recursive=FALSE)
for (i in 1:length(files)) {
  # load file  
  t <- read.csv(files[i], header=T)
  symbol <- substr(files[i], 1, nchar(files[i]) - 4)
  symbol <- substr(symbol, 10, nchar(symbol))
  t <- cbind(Symbol = symbol, t)
  # write to file
  write.csv(t, files[i], quote=F, row.names=F, col.names=T)
}

#Merge all csv files
all_price <- data.frame()
for (i in 1:length(files)) {
  if(i == 1) { 
    result <- TRUE
  }
  temp <- read.csv(files[i], header=result)
  all_price <- rbind(all_price, temp)
}

#Export csv of merged data
write.csv(all_price, "stockcsv/all_price.csv", quote = F, row.names = F, col.names = F)