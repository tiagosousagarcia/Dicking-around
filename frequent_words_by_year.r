###Calculating most frequent words in notes from bibliography by year

library(stringr)
library(ggplot2)

#loading stopwords
stop_words <- scan("stopwords.txt", what = "character", sep = "\n")

#loading list of meaningful words to create the final chart
meaning_words_v <- scan("meaningful_words.txt", what = "character", sep = "\n")

#loading data
fullBiblio <- read.csv("GPA.csv", colClasses = "character") #load complete bibliography; colClasses reads everything as string

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

#merging all the notes for a single year

#METHOD 1: LOOP THROUGH THE TABLE, ADD A NEW YEAR IF NOT EXISTING, CONCATENATE ALL NOTES FOR THAT YEAR

#years_v <- character() #creates an empty vector
# 
# for (year in 1:nrow(year_notes_df)) {
#   if (!(year_notes_df$fullBiblio.Date.Added[year] %in% years_v)){
#     print(year_notes_df$fullBiblio.Date.Added[year])
#     years_v <- c(years_v, year_notes_df$fullBiblio.Date.Added[year])
#   }
# }

#METHOD 2: FIND UNIQUE YEAR VALUES; POPULATE MATRIX WITH THOSE VALUES; CONCATENATE NOTES FOR THOSE YEARS
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

#I MIGHT NEED TO RE-DO THIS -- INSTEAD OF A LIST, IT SHOULD BE A DATAFRAME WITH A ROW PER YEAR, AND A COLUMN PER NEW TERM

#plot the whole thing (FINALLY!)

#see this for more on plotting: http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#challenge_solution_:prototype:

# A different, more simple to read version of this would be to plot the relative frequencies of the n most frequent words in the corpus per year
# 1. Find n most frequent words in the corpus [this should probably be the top n words for each year]
# 2. For each word find its relative frequency per year
# 3. The result should be a data frame that looks like:
#             2013 | 2014 | 2015 | ....
#     word1|    a     b       c
#     word2|    d     e       f
#     word3|    g     h       i
#     wordn|    j     k       l
#
# 4.  Plot the changing relative frequency of each word per year

#Find most significant words by electing the top 10 for each year -- might need to redo this depending on results
most_sign_words <- character(0)
for (year in years_v){
  most_sign_words <- c(most_sign_words, names(notes_by_year_l[[year]][1:10]))
}
most_sign_words <- unique(most_sign_words) #culls repetitions

#for each word find its rel. frequency per year
#notes: 1. to access a specific element -- notes_by_year_l[["2013"]][["fanshawe"]]
#       2. if the name does not exist, returns error subscript out of bounds
#       3. to test if a word exists in a given year: "fanshawe" %in% names(notes_by_year_l[["2013"]])

# most_sign_words_year_df <- data.frame(row.names = years_v)
# #test_v <- c(1:8)
# #most_sign_words_year <- cbind(most_sign_words_year, "test" = test_v) #testing how to name columns with cbind
# 
# for (i in 1:length(most_sign_words)){
#   freq_word <- double(0)
#   #print(most_sign_words[i]) #TEST
#   for (year in years_v){
#     if (most_sign_words[i] %in% names(notes_by_year_l[[year]])){
#       freq_word <- c(freq_word, notes_by_year_l[[year]][[most_sign_words[i]]])
#     }
#     else{
#       freq_word <- c(freq_word, 0)
#     }
#   }
#   most_sign_words_year_df <- cbind(most_sign_words_year_df, freq_word)
#   #changing the column name to its respective word
#   names(most_sign_words_year_df)[names(most_sign_words_year_df) == "freq_word"] <- most_sign_words[i]
#   #print(freq_word)
# }

#Example to plot a single word
#ggplot(most_sign_words_year_df, aes(row.names.data.frame(most_sign_words_year_df), fanshawe, group=1)) + geom_line()

#seems to be quite hard to plot all the words with this df format
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

#PLOTTING

#basic line plot
#ggplot(most_sign_words_year_df2, aes(year, frequency)) + geom_line(aes(color = word))

#what needs to be done:
# 1. Do a selection of 6(ish) representative words
# 2. Add all relative frequencies for terms associated as EM or DH research, then have a line graph with those two.

#Filtering most significant words frequency per year to be plotted:
words_to_plot_df <- subset(most_sign_words_year_df2, most_sign_words_year_df2$word %in% meaning_words_v)

#plotting those words:

#OPTION 1
# plot1 <- ggplot(words_to_plot_df, aes(year, frequency)) + 
#   geom_line(aes(color = word), size = 1) +
#   scale_y_sqrt() +
#   geom_text(data = subset(words_to_plot_df, year == 2020), aes(label = word, colour = word, x = Inf, y = frequency), hjust = -.1) +
#   scale_color_discrete(guide = "none") +
#   theme(plot.margin =  unit(c(1,5,1,1), "lines"))
# 
# final_plot <- ggplotGrob(plot1)
# final_plot$layout$clip[final_plot$layout$name == "panel"] <- "off"
# grid::grid.draw(final_plot)

#OPTION 2
library(directlabels) # to be able to place the labels at the end of the lines in the graph

plot2 <- ggplot(words_to_plot_df, aes(x = year, y = frequency, colour = word)) + #establishes the basics of the plot
  geom_line(size = 1) + #adds the lines, establishes their size as 1
  scale_y_sqrt() +  #Turns the scale of the y axis into a square root scale
  coord_cartesian(ylim = c(0, 1)) + #Zooms in on the most significant bit of the plot (only one value out of bounds)
  scale_x_continuous(breaks = seq(2013, 2020, 1)) + #Demands that all years be shown on the x axix
  scale_color_discrete(guide = "none") + #eliminates the standard labels from the plot
  geom_dl(aes(label = word), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, last.qp)) #uses the direct labels library to put the labels at the end of the lines

#TESTING PUSHING TO GITHUB