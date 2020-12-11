#read data
quotes_df <- read.csv("data/dayofquotes_full.txt", sep = "\n")

#select random number
#which_quote <- sample(nrow(quotes_df), 1)
#display quote
#print(quotes_df[which_quote,])

#all in one line
#print(quotes_df[sample(nrow(quotes_df), 1),])

#removing the name and date
#definition <- quotes_df[sample(nrow(quotes_df), 1),]
#finds any alphabetic character that immediately follows a ',' plus all characters after that, replaces it with empty string ""
#definition <- gsub(",[a-zA-Z].*", "", definition)
#print(definition)

#making this into a function
random_definition <- function() {
  definition <- quotes_df[sample(nrow(quotes_df), 1),]
  definition <- gsub(",[a-zA-Z].*", "", definition)
  print(definition)
}