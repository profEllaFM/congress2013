install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RCurl")
install.packages("tm")

cleaned <- function(corpus){
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  corpus
}  #function because we do this a lot over and over


library(tm)
library(SnowballC)
library(wordcloud)
library(foreign)   
library(RCurl) 

url = "https://raw.githubusercontent.com/profEllaFM/congress2013/master/billNomStatus.csv"
rawData = getURL(url)  
bills113 = read.csv(textConnection(rawData))

titleCorpus <- Corpus(VectorSource(bills113$billName)) #create corpus object
titleCorpus <- tm_map(titleCorpus, PlainTextDocument) #remove formatting
titleCorpus <- tm_map(titleCorpus, removePunctuation) # remove punctuation
titleCorpus <- tm_map(titleCorpus, removeWords, stopwords('english'))  #remove common words
titleCorpus <- tm_map(titleCorpus, stemDocument)  #get rid of word endings
wordcloud(titleCorpus, max.words = 100, random.order = FALSE)

#remove act, 2013, and 2014
titleCorpus <- tm_map(titleCorpus, removeWords, c('act', '2013', '2014', stopwords('english')))
wordcloud(titleCorpus, max.words = 100, random.order = FALSE)

#Why is "act" still there? Let's get rid of it another way
#create the actual matrix of terms that the word cloud is drawing from
termDocMatrix <- TermDocumentMatrix(titleCorpus)
matrixTDM <- as.matrix(termDocMatrix)
vectorTDM <- sort(rowSums(matrixTDM),decreasing=TRUE)
wordDF <- data.frame(word = names(vectorTDM),freq=vectorTDM)
View(wordDF)
wordDF <- wordDF[-1, ]

wordcloud(wordDF$word, wordDF$freq, max.words = 100, random.order = FALSE)

#ok, but now we want to get rid of the offending terms from the bill name column overall
#REGULAR EXPRESSIONS come in handy
#useful cheat sheet: https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/ 

# basic substitutions
tmpWd <- 'practice act actor enact factor nothing'
tmpWd
help(gsub)
gsub("act ","-- ",tmpWd, ignore.case=TRUE )
gsub(" act"," --",tmpWd, ignore.case=TRUE )
gsub("act","--",tmpWd, ignore.case=TRUE )
gsub("\\<act\\>","--",tmpWd, ignore.case=TRUE )  #grab only words that have act after beginning 
        #and before the end of the word
#applying this to the entire column of bill names
bills113$billName <- gsub("\\<act\\>"," ", bills113$billName, ignore.case=TRUE ) 

#remove some numbers
tmpWd <- '1003 2013 regular expressions'
tmpWd
gsub("201\\d","--",tmpWd, ignore.case=TRUE )  #grab 201(some digit)
gsub("\\d","--",tmpWd, ignore.case=TRUE )   #grab all digits
gsub(' \\d+'," --",tmpWd, ignore.case=TRUE )  #only grab digits after a space
gsub(' \\d{1}'," -",tmpWd, ignore.case=TRUE )  #only grab one digit after a space
#applying this to the entire column fo bill names
bills113$billName <- gsub("201\\d","", bills113$billName, ignore.case=TRUE )


#compare democrat and republican bills
demBills <- subset(bills113, party=="Democrat")
repBills <- subset(bills113, party=="Republican")

titleCorpusD <- Corpus(VectorSource(demBills$billName))
titleCorpusD <- tm_map(titleCorpusD, PlainTextDocument)
titleCorpusD <- tm_map(titleCorpusD, removePunctuation)
titleCorpusD <- tm_map(titleCorpusD, removeWords, stopwords('english'))
titleCorpusD <- tm_map(titleCorpusD, stemDocument)  #we've done this a lot. It's cluttering our code
wordcloud(titleCorpusD, max.words = 100, random.order = FALSE)

titleCorpusR <- Corpus(VectorSource(repBills$billName))
titleCorpusR <- cleaned(titleCorpusR)  #so I put those lines into a function. Now I call 1 line, instead of4
wordcloud(titleCorpusR, max.words = 100, random.order = FALSE)

#just the 10 most common words
wordcloud(titleCorpusD, max.words = 10, random.order = FALSE)
wordcloud(titleCorpusR, max.words = 10,  random.order = FALSE)

#Comparing democrats and republicans
#need to create one entry for Democrats, one for Republicans
demTitlesCorp <- Corpus(VectorSource(paste(demBills$billName,collapse=" ") )) 
demTitlesCorp <- cleaned(demTitlesCorp)
repTitlesCorp <- Corpus(VectorSource(paste(repBills$billName,collapse=" ") ))
repTitlesCorp <- cleaned(repTitlesCorp)

compareParty <- c(demTitlesCorp, repTitlesCorp)
termDocMatrixComp = TermDocumentMatrix(compareParty)
termDocMatrixComp = as.matrix(termDocMatrixComp)
colnames(termDocMatrixComp) <- c("Democrats","Republicans")
examineTermMatrix <- as.data.frame(termDocMatrixComp)
examineTermMatrix <-   examineTermMatrix[order(examineTermMatrix$Republicans),]

comparison.cloud(termDocMatrixComp,max.words=300,random.order=FALSE)
commonality.cloud(termDocMatrixComp,max.word= 100, random.order=FALSE)

#DESCRIPTIONS INSTEAD OF BILL TITLES
#I also have the decription of bills according to Poole and Rosenthal
descripCorpus <- Corpus(VectorSource(bills113$descrip))
descripCorpus <- cleaned(descripCorpus)  #so I put those lines into a function. Now I call 1 line, instead of4
wordcloud(descripCorpus, max.words = 100, random.order = FALSE)

bills113$descrip <- gsub("\\<pass\\>"," ", bills113$descrip, ignore.case=TRUE )
demBills <- subset(bills113, party=="Democrat")
repBills <- subset(bills113, party=="Republican")

descripCorpusD <- Corpus(VectorSource(demBills$descrip))
descripCorpusD <- cleaned(descripCorpusD) 
wordcloud(descripCorpusD, max.words = 100, random.order = FALSE)

descripCorpusR <- Corpus(VectorSource(repBills$descrip))
descripCorpusR <- cleaned(descripCorpusR)  #so I put those lines into a function. Now I call 1 line, instead of4
wordcloud(descripCorpusR, max.words = 100, random.order = FALSE)


#Comparing democrats and republicans
#need to create one entry for Democrats, one for Republicans
demDescripCorp <- Corpus(VectorSource(paste(demBills$descrip,collapse=" ") )) 
demDescripCorp <- cleaned(demDescripCorp)
repDescripCorp <- Corpus(VectorSource(paste(repBills$descrip,collapse=" ") ))
repDescripCorp <- cleaned(repDescripCorp)

compareParty <- c(demDescripCorp, repDescripCorp)
termDocMatrixComp = TermDocumentMatrix(compareParty)
termDocMatrixComp = as.matrix(termDocMatrixComp)
colnames(termDocMatrixComp) <- c("Democrats","Republicans")
examineTermMatrix <- as.data.frame(termDocMatrixComp)
examineTermMatrix <-   examineTermMatrix[order(examineTermMatrix$Republicans),]

comparison.cloud(termDocMatrixComp,max.words=300,random.order=FALSE)
commonality.cloud(termDocMatrixComp,max.word= 100, random.order=FALSE)
