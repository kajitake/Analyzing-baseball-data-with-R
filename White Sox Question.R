install.packages("readxl")
library(readxl)
library(tidyverse)
library(dplyr)

pitch_data <- read_csv("/Users/kenji/Desktop/2024 PD Internship Questionnaire Data/pitch_data.csv")

#Q.1 How many pitches do not have a recorded spin axis?
missing_spin <- sum(is.na(pitch_data$spin_axis))

missing_spin

#Q.2 What is the ID of the pitcher who threw the highest percentage of fastballs
#(4-seam fastball + sinker) with a minimum of 30 total pitches? What is that percentage?

#filter pitchers with atleast 30 pitches
P_30 <- pitch_data %>%
  group_by(pitcher_id) %>%
  filter(n() >= 30)

# Calculating percentage of fastballs for each pitcher
fastballs <- P_30 %>%
  filter(pitch_name %in% c("4-Seam Fastball", "Sinker")) %>%
  group_by(pitcher_id) %>%
  summarize(percentage = sum(!is.na(pitch_name)) / n() * 100) %>%
  arrange(desc(percentage))

# Pitcher ID with the highest percentage of fastballs
top_fastball_pitcher <- fastballs$pitcher_id[1]
percentage <- fastballs$percentage[1]

top_fastball_pitcher
percentage

#Q.3 What is the ID of the pitcher who on average has the furthest break from pitching 
#hand to glove side on a slider or sweeper? Which pitch type is it? 
#What is the average pitching hand to glove side break on that pitcher's pitch? 
#Please group sliders and sweepers separately for pitchers who throw both.
# Filtering sliders or sweepers
sliders_sweepers <- pitch_data %>%
  filter(pitch_name %in% c("Slider", "Sweeper"))


# Calculating average break for each pitcher
average_break <- sliders_sweepers %>%
  group_by(pitcher_id, pitch_name) %>%
  summarize(avg_break = mean(abs(pfx_x), na.rm = TRUE)) %>%
  filter(!is.na(avg_break)) %>%
  group_by(pitcher_id) %>%
  summarize(max_avg_break = max(avg_break))

# Pitcher ID with the furthest break
max_break_pitcher <- average_break$pitcher_id[which.max(average_break$max_avg_break)]
max_break_value <- max(average_break$max_avg_break)

max_break_pitcher 
max_break_value


rmarkdown::render("White Sox Question.R", output_format = "pdf_document")


