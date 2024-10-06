#5.11 Exercises
#Q.1 Run Values of Hits

data2016 %>%
  filter(EVENT_CD == 21) -> doubles
mean_doubles <- doubles %>%
  summarize(mean_run_value = mean(run_value))
mean_doubles

data2016 %>%
  filter(EVENT_CD == 22) -> triples
mean_triples <- triples %>%
  summarize(mean_run_value = mean(run_value))
mean_triples

#Q.2 Value of Different Ways of Reaching First Base
data2016 %>% filter(EVENT_CD %in% c(14, 16, 20)) -> singles2
                    
singles2 %>% 
  group_by(EVENT_CD) %>%
  summarize(N = n(), mean_run_value = mean(run_value))

#Q.3 Comparing Two Players wiht Similar OBPs
data2016 %>% filter(BAT_ID %in% c("eatoa002","marts002"),
         BAT_EVENT_FL == TRUE) %>%
  group_by(BAT_ID) %>%
  summarize(N = n(), 
            mean_run_value = mean(run_value),
            RUNS = sum(run_value))

#Q.4 Create Probability of Scoring a Run Matrix
data2016C %>%
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI >= 1)) -> C5Q4
C5Q4

#Q.5 Runner Advancement with a Single
data2016 %>%
  filter(EVENT_CD == 20) -> singles

singles %>%
  group_by(STATE, NEW.STATE) %>%
  summarize(N = n()) -> S
head(S)

S %>% mutate(BASES = substr(STATE, 1, 3),
             NEW.BASES = substr(NEW.STATE, 1, 3)) %>%
  filter(BASES == "100") %>%
  group_by(NEW.BASES) %>%
  summarize(Freq = sum(N))

S %>% mutate(BASES = substr(STATE, 1, 3),
             NEW.BASES = substr(NEW.STATE, 1, 3)) %>%
  filter(BASES == "110") %>%
  group_by(NEW.BASES) %>%
  summarize(Freq = sum(N)) %>%
  mutate(pct = Freq/sum(Freq))



#Q.6 Hitting Evaluation of Players by Run Values 
People %>% 
  filter(nameFirst == "Mike", nameLast == "Trout") %>%
  pull(retroID) -> trout.id

data2016 %>% 
  filter(BAT_ID == trout.id,
         BAT_EVENT_FL == TRUE) -> trout

trout %>%
  group_by(BASES) %>%
  summarize(N = n())

ggplot(trout, aes(BASES, run_value)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_hline(yintercept = 0, color = "blue") +
  xlab("RUNNERS")

trout %>%
  group_by(BASES) %>%
  summarize(RUNS = sum(run_value),
            PA = n()) -> Runs_Trout

Runs_Trout

Runs_Trout %>% summarize(RE24 = sum(RUNS))

