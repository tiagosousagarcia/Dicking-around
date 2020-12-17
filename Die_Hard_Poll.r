results_1_df <- read.csv("data/9536826245_PollReport_Mon_1.csv", sep = ",")
results_2_df <- read.csv("data/9536826245_PollReport_Mon_2.csv", sep = ",")
results_3_df <- read.csv("data/9536826245_PollReport_Tue_1.csv", sep = ",")
results_4_df <- read.csv("data/9536826245_PollReport_Tue_2.csv", sep = ",")
results_5_df <- read.csv("data/9536826245_PollReport_Tue_3.csv", sep = ",")
results_6_df <- read.csv("data/9536826245_PollReport_Tue_4.csv", sep = ",")

results_total_df <- rbind(results_1_df, results_2_df, results_3_df, results_4_df, results_5_df, results_6_df)

no_total <- sum(results_total_df$Answer == "'Come watch Die Hard, they said, It's a Christmas film, they said...' [i.e., no]")
yes_total <- nrow(results_total_df) - no_total

no_per <- no_total / nrow(results_total_df) * 100
yes_per <- yes_total / nrow(results_total_df) * 100

barplot(c(no_per, yes_per),
  main = "Is Die Hard a Christmas film?",
  names.arg = c("No", "Yes"),
  #ylab = "Percentage", 
  ylim = c(29,71),
  axes = FALSE,
  xpd = FALSE,)
