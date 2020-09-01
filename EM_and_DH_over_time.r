#calculating relative frequency of grouped EM and DH terms by year

library(stringr)
library(ggplot2)

#loading stopwords
stop_words <- scan("conditions/stopwords.txt", what = "character", sep = "\n")

#loading EM and DH words
EM_words_v <- scan("conditions/EM_words.txt", what = "character", sep = "\n")
DH_words_v <- scan("conditions/DH_words.txt", what = "character", sep = "\n")

#loading data
fullBiblio <- read.csv("data/GPA.csv", colClasses = "character") #load complete bibliography; colClasses reads everything as string

#turning Date added column into a date format
fullBiblio$Date.Added <- as.Date(fullBiblio$Date.Added)

#turning it into a single year date
fullBiblio$Date.Added <- lubridate::year(fullBiblio$Date.Added)

#extracting year and notes
year_notes_df <- data.frame(fullBiblio$Date.Added, fullBiblio$Notes)

#ordering by year
year_notes_df <- year_notes_df[order(year_notes_df$fullBiblio.Date.Added),]

#removing items without notes
year_notes_df <- year_notes_df[year_notes_df$fullBiblio.Notes != "",]

#removing html tags and other stuff from strings
year_notes_df$fullBiblio.Notes <- str_remove_all(year_notes_df$fullBiblio.Notes, "<.*?>") #removes html tags from string
year_notes_df$fullBiblio.Notes <- str_remove_all(year_notes_df$fullBiblio.Notes, "[0-9]") #removes numbers from from string
year_notes_df$fullBiblio.Notes <- str_squish(year_notes_df$fullBiblio.Notes) #removes repeated whitespace
year_notes_df$fullBiblio.Notes <- gsub(pattern = "[^[:print:]]", replacement = "", year_notes_df$fullBiblio.Notes) #removes non-printable characters

#turning year back into character
year_notes_df$fullBiblio.Date.Added <- as.character(year_notes_df$fullBiblio.Date.Added)

#FIND UNIQUE YEAR VALUES; POPULATE MATRIX WITH THOSE VALUES; CONCATENATE NOTES FOR THOSE YEARS
notes_by_year_m <- character(0)
notes_total_v <- character(0)
years_v <- unique(year_notes_df$fullBiblio.Date.Added)
for (i in years_v){
  notes_v <- as.character(year_notes_df$fullBiblio.Notes[which(year_notes_df$fullBiblio.Date.Added == i)])
  notes_v <- tolower(paste(notes_v, collapse = " "))
  notes_total_v <- c(notes_total_v, notes_v)
}
notes_by_year_m <- cbind(years_v, notes_total_v)
notes_by_year_l <- list()

#tokenize and create relative word frequncy lists for every year

for (year in 1:nrow(notes_by_year_m)){
  year_words_l <- strsplit(notes_by_year_m[year,2], "\\W")
  year_words_v <- unlist(year_words_l)
  year_words_v <- str_remove_all(year_words_v, "â") #removing this from string, some encoding problem doesn't figure it out *******WARNING THIS CREATES EMPTY STRINGS**********
  year_words_v <- year_words_v[which(year_words_v != "")] #removes empty indexes #removing the newly created empty string
  year_words_t <- table(year_words_v) #absolute freqency table
  year_words_not_stop_v <- year_words_v[which(!year_words_v %in% stop_words)] #remove stopwords
  year_words_rel_freq_t <- table(year_words_not_stop_v)
  year_words_rel_freq_t <- 100*(year_words_rel_freq_t/sum(year_words_t))
  year_words_rel_freq_t <- sort(year_words_rel_freq_t, decreasing = TRUE)
  #print(year_words_rel_freq_t[1:10]) #TEST
  notes_by_year_l[[notes_by_year_m[year,1]]] <- year_words_rel_freq_t[1:1000]
}

#Find most significant words by electing the top 10 for each year -- might need to redo this depending on results
most_sign_words <- character(0)
for (year in years_v){
  most_sign_words <- c(most_sign_words, names(notes_by_year_l[[year]]))
}
most_sign_words <- unique(most_sign_words) #culls repetitions

#trying to build a df with the following format: year, freq, word, in which year = x, freq = y, and word defines colour (or shape or something)
most_sign_words_year_df2 <- data.frame()
for (i in 1:length(most_sign_words)){
  #print(most_sign_words[i]) #TEST
  for (year in years_v){
    if (most_sign_words[i] %in% names(notes_by_year_l[[year]])){
      #freq_word <- c(freq_word, notes_by_year_l[[year]][[most_sign_words[i]]])
      row_v <- c(most_sign_words[i], year, notes_by_year_l[[year]][[most_sign_words[i]]])
      #print(row_v)
      most_sign_words_year_df2 <- rbind(most_sign_words_year_df2, row_v, stringsAsFactors = FALSE)
    }
    else{
      row_v <- c(most_sign_words[i], year, 0)
      most_sign_words_year_df2 <- rbind(most_sign_words_year_df2, row_v, stringsAsFactors = FALSE)
    }
  }
}
names(most_sign_words_year_df2) <- c("word", "year", "frequency") #changing column names
most_sign_words_year_df2$frequency <- as.double(most_sign_words_year_df2$frequency) #Forces frequency to be stored as a number rather than string
most_sign_words_year_df2$year <- as.double(most_sign_words_year_df2$year) #Forces year to be stored as a number rather than string for continuous line plotting

#extracting DH and EM words into seperate DFs
em_words_df <- subset(most_sign_words_year_df2, most_sign_words_year_df2$word %in% EM_words_v)
dh_words_df <- subset(most_sign_words_year_df2, most_sign_words_year_df2$word %in% DH_words_v)

#adding relative frequencies per year
group_freq_year_df <- data.frame()
for (i in 2013:2020){
  #print(i)
  row_v <- c(i, sum(subset(em_words_df, em_words_df$year == i, select = frequency)), "Early Modern") # Creating row for EM freq
  #print(row_v)
  group_freq_year_df <- rbind(group_freq_year_df, row_v, stringsAsFactors = FALSE) # Adding to DF
  row_v <- c(i, sum(subset(dh_words_df, dh_words_df$year == i, select = frequency)), "Digital Humanities") # Creating row for DH freq
  group_freq_year_df <- rbind(group_freq_year_df, row_v, stringsAsFactors = FALSE) # Adding to DF
}

#Changing column names and data types in DF
names(group_freq_year_df) <- c("Year", "Frequency", "Type")
group_freq_year_df$Year <- as.double(group_freq_year_df$Year)
group_freq_year_df$Frequency <- as.double(group_freq_year_df$Frequency)

#PLOTTING
freq_plot <- ggplot(group_freq_year_df, aes(x = Year, y = Frequency, colour = Type)) + #establishes the basics of the plot
  geom_line(size = 1) + #adds the lines, establishes their size as 1
  scale_x_continuous(breaks = seq(2013, 2020, 1)) + #Demands that all years be shown on the x axix
  #styles the legend box to have no title, be underneath the graph in a vertical arrangement, with an x separation of 5pt and text size 12pt
  theme(legend.title = element_blank(), legend.position = "bottom", legend.spacing.x = unit(5, "point"), legend.direction = "vertical", legend.key.size = unit(12, "point"))
