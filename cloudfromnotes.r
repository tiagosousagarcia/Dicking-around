#Experimenting with R -> making a wordcloud from my notes in Zotero
#install.packages("wordcloud")
library(wordcloud)
library(stringr)

stop_words <- scan("conditions/stopwords.txt", what = "character", sep = "\n")

fullBiblio <- read.csv("data/GPA.csv", colClasses = "character") #load complete bibliography; colClasses reads everything as string
notesVector <- fullBiblio$Notes #extracts notes into a character vector
notesVector <- notesVector[which(notesVector != "")] #removes empty strings
notesWordsVector <- tolower(paste(notesVector, collapse = " ")) #puts everything into a single string
notesWordsVector <- str_remove_all(notesWordsVector, "<.*?>") #removes html tags from string
notesWordsVector <- str_remove_all(notesWordsVector, "[0-9]") #removes numbers from from string
notesWordsVector <- str_remove_all(notesWordsVector, "â") #removing this from string, some encoding problem doesn't figure it out
notesWordsList <- strsplit(notesWordsVector, "\\W") #tokenizes string into a list
notesWordsVector <- unlist(notesWordsList) #turns it back into a string vector
notesWordsVector <- notesWordsVector[which(notesWordsVector != "")] #removes empty indexes
notesWordsVector <- notesWordsVector[which(!notesWordsVector %in% stop_words)] #removes stop words
notesWordsFreqTable <- sort(table(notesWordsVector), decreasing = TRUE) #turns it into a frequency table

#plotting word cloud from most frequent words

notesDF <- as.data.frame(notesWordsFreqTable)

wordcloud(notesDF$notesWordsVector, notesDF$Freq, max.words = 100, colors = TRUE)
