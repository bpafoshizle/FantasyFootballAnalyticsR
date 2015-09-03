require(httr)
require(XML)

url <- "http://fantasyfootballcalculator.com/completed_drafts.php?format=ppr&teams=10"
r <- GET(url)

tab <- readHTMLTable(content(r, "text"), header=T, stringsAsFactors=FALSE)[[1]]
