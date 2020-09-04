#This script tries to figure out whether the physical disposition of the page (i.e., recto or verso)
#has any influence in the amount of notes taken.

#First, it figures out whether there are any differences between the amount of notes taken from a recto (odd page number) vs
#the amount of notes from a verso (even page number).

#If so, it attempts to verify if this continues regardless of whether the notes come from a physical book (i.e,. without attachement/URL)
#vs a digital copy (with attachment/URL)

#There are a few assumptions made by this model: 
# - recto always equals odd page numbers
# - URL or Attachment always equates to physical books.
library(stringr)

fullBiblio <- read.csv("data/GPA.csv", colClasses = "character") #load complete bibliography; colClasses reads everything as string

#### 1: TOTAL VALUES
# notesVector <- fullBiblio$Notes #extracts notes into a character vector
# notesVector <- notesVector[which(notesVector != "")] #removes empty strings
# notesVector <- tolower(paste(notesVector, collapse = " ")) #puts everything into a single string
# page_numbers_v <- str_extract_all(notesVector, "\\(p?\\.?\\d+\\-?\\d+\\)", simplify = TRUE) #extracts everything that is not numbers (or number ranges) within () from string -- original REGEX "\(p?.?\d+-?\d+\)"
# page_numbers_v <- as.vector(page_numbers_v) #turns it back into character vector
# page_numbers_v <- str_remove_all(page_numbers_v, "\\(|\\)|p|\\.") #removes parenthesis, p's and .s
# page_numbers_v <- str_remove_all(page_numbers_v, "\\-.*") #removes everything after hyphen (i.e., keeps only first page)
# page_numbers_v <- as.numeric(page_numbers_v) #Turns it into a number vector
# 
# odd_total <- 0
# even_total <- 0
# 
# for (i in 1:length(page_numbers_v)){
#   if (page_numbers_v[i] %% 2 == 0) {
#     even_total <- even_total + 1
#   }
#   else {
#     odd_total <- odd_total + 1
#   }
# }
# 
# recto_per <- 100 * (odd_total / length(page_numbers_v))
# verso_per <- 100 * (even_total / length(page_numbers_v))
# 
# barplot(c(recto_per, verso_per), 
#         main = "Percentage of notes from Recto and Verso pages (total)", 
#         names.arg = c("Recto", "Verso"), 
#         ylab = "Percentage", ylim = c(0,100))

#### VALUES ONLY FROM PHYSICAL BOOKS
# fullBiblio <- subset(fullBiblio, fullBiblio$Url == "" & fullBiblio$File.Attachments == "")
# notesVector <- fullBiblio$Notes #extracts notes into a character vector
# notesVector <- notesVector[which(notesVector != "")] #removes empty strings
# notesVector <- tolower(paste(notesVector, collapse = " ")) #puts everything into a single string
# page_numbers_v <- str_extract_all(notesVector, "\\(p?\\.?\\d+\\-?\\d+\\)", simplify = TRUE) #extracts everything that is not numbers (or number ranges) within () from string -- original REGEX "\(p?.?\d+-?\d+\)"
# page_numbers_v <- as.vector(page_numbers_v) #turns it back into character vector
# page_numbers_v <- str_remove_all(page_numbers_v, "\\(|\\)|p|\\.") #removes parenthesis, p's and .s
# page_numbers_v <- str_remove_all(page_numbers_v, "\\-.*") #removes everything after hyphen (i.e., keeps only first page)
# page_numbers_v <- as.numeric(page_numbers_v) #Turns it into a number vector
# 
# odd_total <- 0
# even_total <- 0
# 
# for (i in 1:length(page_numbers_v)){
#   if (page_numbers_v[i] %% 2 == 0) {
#     even_total <- even_total + 1
#   }
#   else {
#     odd_total <- odd_total + 1
#   }
# }
# 
# recto_per <- 100 * (odd_total / length(page_numbers_v))
# verso_per <- 100 * (even_total / length(page_numbers_v))
# 
# barplot(c(recto_per, verso_per), 
#         main = "Percentage of notes from Recto and Verso pages (physical books)", 
#         names.arg = c("Recto", "Verso"), 
#         ylab = "Percentage", ylim = c(0,100))

#### VALUES ONLY FROM DIGITAL BOOKS
fullBiblio <- subset(fullBiblio, fullBiblio$Url != "" | fullBiblio$File.Attachments != "")
notesVector <- fullBiblio$Notes #extracts notes into a character vector
notesVector <- notesVector[which(notesVector != "")] #removes empty strings
notesVector <- tolower(paste(notesVector, collapse = " ")) #puts everything into a single string
page_numbers_v <- str_extract_all(notesVector, "\\(p?\\.?\\d+\\-?\\d+\\)", simplify = TRUE) #extracts everything that is not numbers (or number ranges) within () from string -- original REGEX "\(p?.?\d+-?\d+\)"
page_numbers_v <- as.vector(page_numbers_v) #turns it back into character vector
page_numbers_v <- str_remove_all(page_numbers_v, "\\(|\\)|p|\\.") #removes parenthesis, p's and .s
page_numbers_v <- str_remove_all(page_numbers_v, "\\-.*") #removes everything after hyphen (i.e., keeps only first page)
page_numbers_v <- as.numeric(page_numbers_v) #Turns it into a number vector

odd_total <- 0
even_total <- 0

for (i in 1:length(page_numbers_v)){
  if (page_numbers_v[i] %% 2 == 0) {
    even_total <- even_total + 1
  }
  else {
    odd_total <- odd_total + 1
  }
}

recto_per <- 100 * (odd_total / length(page_numbers_v))
verso_per <- 100 * (even_total / length(page_numbers_v))

barplot(c(recto_per, verso_per), 
        main = "Percentage of notes from Recto and Verso pages (digital books)", 
        names.arg = c("Recto", "Verso"), 
        ylab = "Percentage", ylim = c(0,100))