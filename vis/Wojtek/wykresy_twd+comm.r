setwd('C:/Users/wojte/OneDrive/Pulpit/TWD_CSV')

library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

df <- read.csv('Clean_Merged_Data.csv')

df2 <- read.csv('superbowl-ads.csv')

df3 <- read.csv('halftime_musicians.csv')

df4 <- read.csv('dataset.csv')

df5 <-read.csv('super_bowls.csv')

df6 <- read.csv('tv.csv')

df7 <- read.csv('super_bowl.csv')

#Najczęstsi zwycięzcy i przegrani (best_teams przydaje się później)
df %>%
  filter(Superbowl_Winner == 1) %>%
  group_by(Superbowl_Loser_Team) %>%
  summarise(n=n()) %>%
  arrange(-n)

df %>%
  filter(Superbowl_Winner == 1) %>%
  group_by(Team) %>%
  summarise(n=n()) %>%
  arrange(-n) %>%
  top_n(6) -> best_teams


#Gdzie odbywał się najczęściej (hala)
df5 %>%
  group_by(venue) %>%
  summarise(n=n()) %>%
  arrange(-n)

#Najczęściej pojawiające się reklamy
df2 %>%
  group_by(brand) %>%
  summarise(n=n()) %>%
  arrange(-n)

#Najczęstsi MVP
df7 %>%
  group_by(MVP) %>%
  summarise(n=n()) %>%
  arrange(-n)

#Przygotowanie techniczne pod wykresy
top_brands <- c("Bud Light", "Budweiser", "Doritos", "Pepsi", "Hyundai", "Coca-Cola", "Kia", "NFL")
options(scipen = 999)

#Wykres oglądalności względem czasu (kolumnowy)
df6 %>%
  mutate(year = super_bowl + 1966, views = avg_us_viewers/1000000) %>%
  ggplot(aes(x=year, y=views)) +
  labs(title = "Wykres liniowy ogl?dalno?ci Superbowl w USA",
       subtitle = "Lata 1967-2018",
       x = "Rok",
       y = "?rednia liczba ogl?daj?cych (w mln)")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  geom_col()

#Najczęstsze reklamy (spośród 6-7 najpopularniejszych)
df2 %>%
  filter(brand %in% top_brands) %>%
  mutate(brandx = fct_infreq(ifelse(brand == 'Bud Light', 'Budweiser', brand))) %>%
  ggplot(aes(x = brandx)) +
  labs(title = "Histogram dla najpopularniejszych marek reklamowanych w ramach Superbowl",
       subtitle = "Lata 2000-2020",
       x = "Marka",
       y = "Cz?sto??") +
  geom_bar()

#Cala ponizsza część to stworzenie brzydkiego wykresu
#Pokazującego jak zmieniała się sumaryczna liczba wygranych
#na przestrzeni lat dla 6 najlepszych teamów grających w SB
df %>%
  filter(Superbowl_Winner == 1 & Team %in% best_teams$Team) %>%
  select(Team, Year) -> winners

data.frame(rep(unique(winners$Team), length(winners[[1]]))) -> teamyyy
data.frame(rep(unique(winners$Year), 1, each=6)) -> lata

teamyyy %>%
  mutate(k=rep(unique(winners$Year), 1, each=6)) -> A
winners[rep(seq_len(nrow(winners)), each = 6), ] -> B

A %>%
  mutate(wins=0) %>%
  mutate(wins=ifelse(A[,1:2]==B[,1:2], wins+1, wins)) %>%
  mutate(wins=wins[,1]) -> A
colnames(A) <- c("Team", "Year", "Win")

mujstary <- unique(winners$Year)
A %>%
  group_by(Team) %>%
  summarise(cumsum(Win))%>%
  mutate(Year=mujstary) %>%
  arrange(Year) -> Koniec

colnames(Koniec) <- c("Dru?yna", "Wins", "Year")

Koniec %>%
  ggplot(aes(x=Year, y=Wins, color=Dru?yna)) +
  geom_line(size=1.2) +
  labs(title = "Wykres liniowy najcz?stszych zwyci?zc?w Superbowl",
       subtitle = "Lata 1967-2020",
       x = "Rok",
       y = "??czna liczba wygranych") +
  geom_point()
  









