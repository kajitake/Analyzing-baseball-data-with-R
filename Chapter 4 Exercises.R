#4.7 Exercises
#Q.1 Relationship Between Winning Percentage and Run Differential Across Decades
Teams %>%
  filter(yearID >= 1961, yearID <= 2001) %>%
  mutate(Era = ifelse(yearID <= 1970, "1960-1970",
  ifelse(yearID <= 1980, "1971-1980",
  ifelse(yearID <= 1990, "1981-1990", "1991-2000"))),
  RD = R - RA, WinPct = W / (W + L)) -> Eras

one_fit <- function(years){
  lm(WinPct ~ I(R-RA),
     data = Eras)
}

the_eras <- c("1961-1970", "1971-1980", 
              "1981-1990", "1991-2000")
four_fits <- lapply(the_eras, one_fit)

names(four_fits) <- the_eras

sapply(four_fits, coef)

plot(Eras$RD, Eras$WinPct, color = four_fits)

RD_10 <- function(fit){
  predict(fit, data.frame(R = 30, RA = 20))
}

sapply(four_fits, RD_10)


#Q.2  Pythagorean Residuals for Poor and Great Teams in the 19th Century
Teams %>% filter(yearID <= 1900) %>%
  mutate(WinPct = W / (W + L)) -> D_19th

fit <- lm(WinPct ~ I(R - RA), data = D_19th)
fit
library(broom)
out <- augment(fit)
out %>% mutate(type = ifelse(WinPct > .7, "great",
        ifelse(WinPct < .3, "bad", "other"))) -> out

plot((D_19th$R - D_19th$RA), D_19th$WinPct)

ggplot(out, aes(.resid, WinPct, color = type)) +
  geom_point()

#Q.3 Exploring the Manager Effect in Baseball
Teams %>% filter(yearID >= 2010, yearID <= 2020) %>%
  mutate(RD = R - RA, WinPct = W / (W + L)) -> d

fit <- lm(WinPct ~ I(R-RA), data = d)

out <- augment(fit, data = d)

out %>% inner_join(select(Managers, playerID, yearID,
                          teamID), 
                   by = c("yearID", "teamID")) -> out

out <- inner_join(out, People, by = "playerID")

out %>% group_by(nameGiven) %>% 
  summarize(N = n(), Mean_Residual = mean(.resid)) %>% 
  arrange(desc(Mean_Residual)) -> out
head(out)
tail(out)
              

