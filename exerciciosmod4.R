library(tidyverse)
library(readr)
dados <- read_csv("C:\\Users\\marce\\Downloads\\michelin.csv")
library(lubridate)
glimpse(dados)
library(dplyr)
library(ggplot2)
??pracma

install.packages("pracma")
library(pracma)

dados1 <- dados %>%
  mutate(latR = deg2rad(Latitude))%>%
  mutate(LongR = deg2rad(Longitude))
glimpse(dados1)
view(dados1)

atual <- dados1 %>%
  filter(Name == "Borgo Sant'Anna")
glimpse(atual)

LatA <- atual$latR
LongA <- atual$LongR

harve <- function(Lat, Long, LatB, LongB){
  d = 2 * 6371 * asin(sqrt(
    sin((LatB - Lat) / 2)^2 + cos(Lat) * cos(LatB) * sin((LongB - Long) / 2 )^2
  ))
  return(d)
}

dados2 <- dados1 %>%
  mutate(distance = harve(Lat = LatA, Long = LongA, LatB = latR, LongB = LongR))%>%
  arrange(distance)
glimpse(dados2)
view(dados2)

dados3 <- dados2 %>%
  filter(distance <= 500)%>%
  filter(Award %in% c("1 Star", "2 Stars", "3 Stars", "1 Star,Green Star", "2 Stars,Green Star", "3 Stars,Green Star"  ))
glimpse(dados3)
view(dados3)



dados10 <- dados2 %>%
  filter(distance <= 1500)%>%
  filter(Award %in% c("1 Star", "2 Stars", "3 Stars", "1 Star,Green Star", "2 Stars,Green Star", "3 Stars,Green Star"  ))
glimpse(dados10)



unique(dados2$Award)
view(dados2)

unique(dados2$Award)

dados4 <- dados2 %>%
  filter(distance <= 2000)%>%
  filter(Award %in% c("1 Star", "2 Star", "3 Star", "1 Star,Green Star", "2 Stars,Green Star", "3 Stars,Green Star"  ))%>%  filter(Price %in% c("$", "$$", "£", "££", "€", "€€", "₺", "₺₺", "¥", "¥¥", "฿฿", "₫", "₫₫", "₩", "₩₩" ))%>%
  nrow()
glimpse(dados4)
unique(dados2$Price)

dados5 <- dados2 %>%
  filter(distance <= 2000)%>%
  count(Price)
dados5

dados6 <- dados2 %>%
  filter(grepl("Contemporary", Cuisine))
glimpse(dados6)


grepl("Italian", dados6&cuisine)

dados7 <- dados2 %>%
  filter(grepl("Italian", Cuisine, ignore.case = TRUE))
glimpse(dados7)
unique(dados7$Cuisine)

dados8 <- dados %>%
  filter(grepl("(^|, )Italian(,|$)", Cuisine, ignore.case = TRUE))%>%
  nrow()
glimpse(dados8)
unique(dados8$Cuisine)

dados9 <- dados2 %>%
  filter(grepl("Country cooking", Cuisine, ignore.case = TRUE))
glimpse(dados9)
unique(dados9$Cuisine)

#=================================

library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2021, week = 48)
glimpse(tuesdata)
view(tuesdata)




ex1 <- starwars %>%
  group_by(species)%>%
  summarise(num = n())%>%
  arrange(desc(num))
ex1

ex2 <- starwars %>%
  drop_na()%>%
  group_by(gender)%>%
  summarise(media = mean(height))
glimpse(ex2)

ex3 <- starwars %>%
  group_by(species)%>%
  filter(sex == "male")%>%
  summarise(peso = mean(mass, na.rm =  TRUE))
ex3


ex4 <- starwars %>%
  group_by(species)%>%
  summarise(maior_peso = max(mass, na.rm = TRUE))%>%
  arrange(desc(maior_peso))
glimpse(ex4)


table1
tab1 <- table1 %>%
  pivot_wider(names_from = year, values_from = c(cases, population))
tab1

#===============================================================================

tuesdata <- tidytuesdayR::tt_load(2021, week = 48)
glimpse(tuesdata)
tuesdata$writers
tuesdata$directors
tuesdata$episodes
tuesdata$imdb

names(tuesdata$writers)
names(tuesdata$directors)
names(tuesdata$episodes)
names(tuesdata$imdb)

writers <- tuesdata$writers
directors <- tuesdata$directors
episodes <- tuesdata$episodes
imdb <- tuesdata$imdb

who1 <- writers %>%
  inner_join(episodes, by = "story_number") %>%
  inner_join(directors, by = "story_number")
who1
glimpse(who1)

who2 <- who1 %>%
  filter(writer == "Russell T Davies")%>%
  filter(director == "Graeme Harper")
glimpse(who2)
who2$episode_title

who11 <- who1 %>%
  filter(writer == "Chris Chibnall")%>%
  filter(director == "Jamie Magnus Stone")
glimpse(who11)
who11$episode_title

who3 <- who1 %>%
  mutate(ano = year(first_aired))%>%
  filter(writer == "Russell T Davies")%>%
  filter(ano == "2005")
glimpse(who3)
who3$episode_title


who4 <- who1 %>%
  mutate(ano = year(first_aired))%>%
  filter(writer == "Toby Whithouse")%>%
  summarise(tab = (unique(ano)))
glimpse(who4)


who5 <- who1 %>%
  filter(writer == "Toby Whithouse")%>%
  nrow()
glimpse(who5)

who6 <- who1 %>%
  filter(director == "Lawrence Gough")%>%
  summarise(mins = mean(duration))
who6





glimpse(dados2)


t1 <- dados2%>%
  filter(distance <= 3000)%>%
  filter(Award %in% c("1 Star", "2 Star", "3 Star", "1 Star,Green Star", "2 Stars,Green Star", "3 Stars,Green Star"  ))%>% 
  filter(Price %in% c("$", "$$", "$$$", "£", "££", "£££", "€", "€€", "€€€", "₺", "₺₺", "₺₺₺", "¥", "¥¥" , "¥¥¥"))
glimpse(t1)

unique(dados2$Price)

t2 <- dados2 %>%
  filter(grepl("Italian", Cuisine))
glimpse(t2)


glimpse(tuesdata)
writers <- tuesdata$writers
directors <- tuesdata$directors
episodes <- tuesdata$episodes
imdb <- tuesdata$imdb

glimpse(writers)
glimpse(directors)
glimpse(episodes)
glimpse(imdb)
names(writers)
names(directors)
names(episodes)
names(imdb)


who <- writers%>%
  inner_join(directors, by = "story_number")%>%
  inner_join(episodes, by = "story_number")
glimpse(who)


who_mut <- who%>%
  mutate(ep_sea = paste(season_number, episode_number, sep = "-"))
imdb_mut <- imdb %>%
  mutate(ep_sea = paste(season, ep_num, sep = "-"))
imdb_mut


who_final <- who_mut %>%
  inner_join(imdb_mut, by = "ep_sea")
glimpse(who_final)


w1 <- who_final %>%
  filter(director == "Daniel Nettheim")%>%
  filter(writer == "Peter Harness")
glimpse(w1)


w2 <- who_final %>%
  filter(director == "Graeme Harper")%>%
  filter(writer == "Russell T Davies")
glimpse(w2)

w3 <- who_final %>%
  mutate(ano = year(first_aired))%>%
  filter(ano == "2005")%>%
  filter(writer == "Russell T Davies")
glimpse(w3)


w4 <- who_final %>%
  mutate(ano = year(first_aired))%>%
  filter(ano == "2005")%>%
  filter(writer == "Russell T Davies")
glimpse(w4)

w5 <- who_final %>%
  mutate(ano = year(first_aired))%>%
  filter(writer == "Gareth Roberts")
glimpse(w5)

w6 <- who_final %>%
  filter(writer == "Gareth Roberts")
glimpse(w6)



w7 <- who_final %>%
  filter(director == "Nida Manzoor")%>%
  summarise(med = mean(duration))
glimpse(w7)


45 * 5 + 42 
