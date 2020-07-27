# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("NLP")
library("data.table")
library("wordcloud2")
library("Rcpp")
install.packages("dplyr")
data <-  fread("~/Desktop/twitterdata.csv")
colnames(data)
my_df <- dplyr::select(data,Tweet_Text)
docs <- Corpus(VectorSource(my_df))

to_space <- content_transformer(function(x,pattern) gsub(pattern," ",x))
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
docs <- tm_map(docs,to_space,"/")
docs <- tm_map(docs,to_space,"@")
docs <- tm_map(docs,to_space,"\\|")
docs <- tm_map(docs,to_space,"#")
docs <- tm_map(docs,content_transformer(removeURL))


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("covid","blabla1", "blabla2","can","now","people","will","must","way")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
colnames(d)


#Removing some stupid special characters here.
Encoding( x = d$word ) <- "UTF-8"
d$word <-
  iconv( x = d$word
         , from = "UTF-8"
         , to = "UTF-8"
         , sub = "" )


require(devtools)
install_github("lchiffon/wordcloud2")
set.seed(123)

d$word <- as.character(d$word)
wordcloud2(d,color="random-light",backgroundColor="black",
           shape='star',size=2)
head(demoFreq,n=5)
class(d)
head(d,n=5)
