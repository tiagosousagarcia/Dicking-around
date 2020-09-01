library(benford.analysis)

######## It probably needs more testing but it looks like Benford's Law does apply to word frequencies from text data;
######## HOWEVER, it also applies regardless of the source of the data; i.e., it doesn't matter whether it was written by humans or computers;
######## or whether it is a clean or dirty text;
######## or whether it is a literary or scientific text.
######## In other words, it cannot be used as a test.


#loading data
melville_text <- scan("data/melville.txt", what = "character", sep = "\n") #CLEAN MOBY DICK TEXT
austen_text <- scan("data/austen.txt", what = "character", sep = "\n") #CLEAN PRIDE AND PREJUDICE TEXT
melville_PGtext <- scan("data/melville_PG.txt", what = "character", sep = "\n") #PROJECT GUTENBERG MB TEXT
melville_OCR_text <- scan("data/melville_OCR.txt", what = "character", sep = "\n") #OCR FROM INTERNET ARCHIVE MB TEXT
random_text <- scan("data/random_text.txt", what = "character", sep = "\n") # RANDOM TEXT FROM INTERNET GENERATORS
lovelace_text <- scan("data/analytical_engine.txt", what = "character", sep = "\n") #OCR FROM BABBAGE AND LOVElACE ANALYITICAL ENGINE

#removing empty strings
melville_v <- melville_text[which(melville_text != "")] #MB CLEAN
austen_v <- austen_text[which(austen_text != "")] #AUSTEN CLEAN
melvillePG_v <- melville_PGtext[which(melville_PGtext != "")] #MB from PG
melville_OCR_v <- melville_OCR_text[which(melville_OCR_text != "")] #MB from OCR
random_v <- random_text[which(random_text != "")] #RANDOM TEXT
lovelace_v <- lovelace_text[which(lovelace_text != "")] #SCIENTIFIC TEXT

#puts everything into a single string
melville_v <- tolower(paste(melville_v, collapse = " "))
austen_v <- tolower(paste(austen_v, collapse = " "))
melvillePG_v <- tolower(paste(melvillePG_v, collapse = " "))
melville_OCR_v <- tolower(paste(melville_OCR_v, collapse = " "))
random_v <- tolower(paste(random_v, collapse = " "))
lovelace_v <- tolower(paste(lovelace_v, collapse = " "))

#tokenizes string into a list
melville_v <- strsplit(melville_v, "\\W") 
austen_v <- strsplit(austen_v, "\\W")
melvillePG_v <- strsplit(melvillePG_v, "\\W")
melville_OCR_v <- strsplit(melville_OCR_v, "\\W")
random_v <- strsplit(random_v, "\\W")
lovelace_v <- strsplit(lovelace_v, "\\W")

#turns it back into a string vector
melville_v <- unlist(melville_v)
austen_v <- unlist(austen_v)
melvillePG_v <- unlist(melvillePG_v)
melville_OCR_v <- unlist(melville_OCR_v)
random_v <- unlist(random_v)
lovelace_v <- unlist(lovelace_v)

#removes empty indexes
melville_v <- melville_v[which(melville_v != "")]
austen_v <- austen_v[which(austen_v != "")]
melvillePG_v <- melvillePG_v[which(melvillePG_v != "")]
melville_OCR_v <- melville_OCR_v[which(melville_OCR_v != "")]
random_v <- random_v[which(random_v != "")]
lovelace_v <- lovelace_v[which(lovelace_v != "")]

#turns it into a table of frequencies
melville_freq_table <- sort(table(melville_v), decreasing = TRUE) 
austen_freq_table <- sort(table(austen_v), decreasing = TRUE)
melvillePG_freq_table <- sort(table(melvillePG_v), decreasing = TRUE)
melville_OCR_freq_table <- sort(table(melville_OCR_v), decreasing = TRUE)
random_freq_table <- sort(table(random_v), decreasing = TRUE)
lovelace_freq_table <- sort(table(lovelace_v), decreasing = TRUE)

#makes it a df, for easy manipulation
melville_freq_df <- as.data.frame(melville_freq_table)
austen_freq_df <- as.data.frame(austen_freq_table)
melvillePG_freq_df <- as.data.frame(melvillePG_freq_table)
melville_OCR_freq_df <- as.data.frame(melville_OCR_freq_table)
random_freq_df <- as.data.frame(random_freq_table)
lovelace_freq_df <- as.data.frame(lovelace_freq_table)

#creates Benford object
benford_melville <- benford(melville_freq_df$Freq, number.of.digits = 1)
benford_austen <- benford(austen_freq_df$Freq, number.of.digits = 1)
benford_melvillePG <- benford(melvillePG_freq_df$Freq, number.of.digits = 1)
benford_melville_OCR <- benford(melville_OCR_freq_df$Freq, number.of.digits = 1)
benford_random <- benford(random_freq_df$Freq, number.of.digits = 1)
benford_lovelace <- benford(lovelace_freq_df$Freq, number.of.digits = 1)

#TO PLOT:
#plot(benford_object)