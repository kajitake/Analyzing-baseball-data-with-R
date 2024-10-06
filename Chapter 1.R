install.packages("Lahman")
library(Lahman)
library(tidyverse)
library(tibble)
getwd()
spahn <- as_tibble(read.csv("https://raw.githubusercontent.com/maxtoki/baseball_R/master/data/spahn.csv",))
spahn %>% slice(1:3) %>% select(1:10)
spahn %>% slice(1:10) %>% select(Age, W, L, ERA)
(spahn %>%
  summarize(LO = min(ERA),
            QL = quantile(ERA, .25), QU = quantile(ERA, .75),
            M = median(ERA), HI = max(ERA)))
#find age at which Spahn had lowest ERA
spahn %>% filter(ERA == min(ERA)) %>% select(Age)

#mutate to create FIP
spahn %>% mutate(FIP = (13*HR + 3*BB - 2*SO)/IP) -> spahn

#find seasosn with best FIP
spahn %>%
  arrange(FIP) %>% 
  select(Year, Age, W, L, ERA, FIP) %>%
  head()

#compare pitching for BOS & MIL
spahn1 <- spahn %>% filter(Tm == "BSN" | Tm == "MLN")
#redefine Tm to only include BOS and MIL
spahn1 <- spahn1 %>% 
  mutate(Tm = factor(Tm, levels = c("BSN", "MLN")))
#compare various pitching stats for the two teams
spahn1 %>% 
  group_by(Tm) %>%
  summarize(mean_W.L = mean(W.L, na.rm = TRUE),
            mean_ERA = mean(ERA),
            mean_WHIP = mean(WHIP),
            mean_FIP = mean(FIP))

getwd()
library(readr)
data1 <- read.csv("/Desktop/R/Analyzing baseball data with R/NLbatting.csv", header = TRUE)
read_csv("data/ALbatting.csv")

#2.4.4 merging and selecting data frames
NLbatting <- read.csv("data/NLbatting.csv")
ALbatting <- read.csv("data/ALbatting.csv")
batting <- bind_rows(NLbatting, ALbatting)
