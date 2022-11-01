setwd('C:/Users/wojte/OneDrive/Pulpit/TWD_CSV')
source("themes.R")
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

df <- read.csv('Clean_Merged_Data.csv')

df2 <- read.csv('superbowl-ads.csv')

#df3 <- read.csv('halftime_musicians.csv')

df4 <- read.csv('dataset.csv')

df5 <-read.csv('super_bowls.csv')

df6 <- read.csv('tv.csv')

#df7 <- read.csv('super_bowl.csv')

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
  top_n(4) -> best_teams


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
# df7 %>%
#   group_by(MVP) %>%
#   summarise(n=n()) %>%
#   arrange(-n)

#Przygotowanie techniczne pod wykresy
top_brands <- c("Bud Light", "Budweiser", "Doritos", "Pepsi", "Hyundai", "Coca-Cola", "Kia", "NFL")
options(scipen = 999)

#Wykres oglądalności względem czasu (kolumnowy)
df6 %>%
  mutate(avg_us_viewers = ifelse(avg_us_viewers == 26750000, (26750000
+24430000), avg_us_viewers)) %>%
  filter(avg_us_viewers != 24430000) %>%
  mutate(year = super_bowl + 1966, views = avg_us_viewers/1000000) %>%
  ggplot(aes(x=year, y=views)) +
  labs(title = "Średnia oglądalność Superbowl w USA na przestrzeni lat",
       x = "Rok",
       y = "Średnia liczba oglądających (w mln)")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  scale_x_discrete(limits = c(1967, 1980, 1990, 2000,2010, 2018)) +
  geom_col(fill='pink', color = "black") +
  theme_dark_blue()

#Najczęstsze reklamy (spośród 6-7 najpopularniejszych)
df2 %>%
  filter(brand %in% top_brands) %>%
  mutate(brandx = fct_infreq(ifelse(brand == 'Bud Light', 'Budweiser', brand))) %>%
  ggplot(aes(x = brandx)) +
  labs(title = "Histogram dla najpopularniejszych marek reklamowanych w ramach Superbowl",
       subtitle = "Lata 2000-2020",
       x = "Marka",
       y = "Częstość") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 120)) +
  geom_bar(color="black", fill="steelblue") +
  theme_bw()

#Cala ponizsza część to stworzenie brzydkiego wykresu
#Pokazującego jak zmieniała się sumaryczna liczba wygranych
#na przestrzeni lat dla 6 najlepszych teamów grających w SB
df %>%
  filter(Superbowl_Winner == 1 & Team %in% best_teams$Team) %>%
  select(Team, Year) -> winners

data.frame(rep(unique(winners$Team), length(winners[[1]]))) -> teamyyy
data.frame(rep(unique(winners$Year), 1, each=4)) -> lata

teamyyy %>%
  mutate(k=rep(unique(winners$Year), 1, each=4)) -> A
winners[rep(seq_len(nrow(winners)), each = 4), ] -> B

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

colnames(Koniec) <- c("Drużyna", "Wins", "Year")

Koniec %>%
  ggplot(aes(x=Year, y=Wins)) +
  geom_line(size=1.2, color = "#f7fcb9") +
  labs(title = "Wykres liniowy najczęstszych zwycięzców Superbowl",
       x = "Rok",
       y = "Łączna liczba wygranych") +
  facet_grid(rows = vars(Drużyna)) +
  theme_dark_blue() +
  theme(panel.border = element_rect(colour = "#d3d5df", fill=NA, size=1.2))
