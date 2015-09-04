require(httr)
require(XML)

# Global handle to use across multiple requests to yahoo
yahoo <- handle("http://yahoo.com")

# Function to log in to login.yahoo.com
yahooLogin <- function(user, pw
                       ,proxyUrl=NULL
                       ,proxyPort=NULL
                       ,htmlResult=NULL
                       ){
   url <- "https://login.yahoo.com"
   headers <- getYahooHeaders()
   r <- 
      if(!is.null(proxyUrl)){
         GET(url
               ,add_headers(.headers = headers)
               ,use_proxy(proxyUrl,proxyPort) 
               ,handle=yahoo
         )            
      }
      else{
         GET(url
             ,add_headers(.headers = headers)
             ,handle=yahoo
             #,verbose()
         )    
      }
   
   
   
   body <- getYahooLoginPostBody(user, pw)
   body <- c(body, getHiddenInputs(r))
   body <- dedupListByName(body)
   
   r <- 
      if(!is.null(proxyUrl)){
         POST(url, body=body, encode="form"
                ,add_headers(.headers = headers)
                ,use_proxy(proxyUrl,proxyPort)
                ,handle=yahoo
         )
      }
      else{
         POST(url, body=body, encode="form"
              ,add_headers(.headers = headers)
              ,handle=yahoo
              #,verbose()
         )
      }
   
   if(!is.null(htmlResult)){
      write(content(r, "text"), file(htmlResult))
   }
}

yahooGetHtml <- function(url
                         ,proxyUrl=NULL
                         ,proxyPort=NULL
                         ,htmlResult=NULL){
   headers <- getYahooHeaders()
   r <- 
      if(!is.null(proxyUrl)){
         GET(url
             ,add_headers(.headers = headers)
             ,use_proxy(proxyUrl,proxyPort)
             ,handle=yahoo
         )
      }
      else{
         GET(url
             ,add_headers(.headers = headers)
             ,handle=yahoo
         )
      }

   
   return(content(r, "text"))
}

# Function to deduplicate elements of a list leaving the names intact
dedupListByName <- function(l){
   l <- l[!duplicated(names(l))]
   return(l)
}

# Function to take an HTML result from the Yahoo login,
# and return a list of inputs and values
getHiddenInputs <- function(r){
   doc <- htmlParse(content(r, "text"), asText=TRUE)
   inputNames <- xpathSApply(doc, '//div[@id="hiddens"]//input/@name')
   inputVals <- xpathSApply(doc, '//div[@id="hiddens"]//input/@value')
   names(inputVals) = inputNames
   return(inputVals)
}

# Function to return working headers for communication with yahoo
getYahooHeaders <- function() {
   headers = c(Accept = "*/*",
               "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4) AppleWebKit/600.7.12 (KHTML, like Gecko) Version/8.0.7 Safari/600.7.12",
               "Referer" = "https://login.yahoo.com/",
               Connection = "keep-alive",
               "Accept-Encoding" = "gzip, deflate",
               "Accept-Language" = "en-us"
   )
   
   return(headers)
}

# Function to build the proper login.yahoo.com body post data
getYahooLoginPostBody <- function(user, pw) {
   body <- list(
      countrycode = "1",
      username = user,
      passwd = pw,
      signin = ""
   )
   return(body)
}

testLogin <- function(useProxy=F, writeResult=F) {
   source(paste(getwd(), "/sensitive/yahooLoginInfo.R", sep=""))
   yahooLogin(yahooUser,yahooPass
              ,proxyUrl = if(useProxy) "127.0.0.1" else NULL
              ,proxyPort = if(useProxy) 8080 else NULL
              ,htmlResult= if(writeResult) "outputHTTR_LOGIN.html" else NULL
   )
}