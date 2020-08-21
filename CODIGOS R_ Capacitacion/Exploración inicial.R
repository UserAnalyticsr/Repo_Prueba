library(tidyverse)
library(funModeling)
library(Hmisc)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
datos <- astronauts

glimpse(datos)

View(df_status(datos))

# Frecuency of missions per astronaut
frec <- datos %>% group_by(name) %>%tally() %>%  arrange(n) %>% view()

#Frecuency of astronauts per number o mission did it
frec2 <- frec %>% group_by(n) %>% tally() %>% view()


# Genre Proportion in missions
prop.table(table(datos$sex))

# Mean of trips for all astronauts
media_num_de_viajes <- mean(frec$n)

# Occupation frecuency for womans
mujeres_ocupacion <- datos %>% filter(sex=="female") %>% group_by(occupation) %>% tally() %>% view()



          ####  Exploration about the amount of trips over the years ####

years_with_more_missions <- datos %>% group_by(year_of_mission) %>% tally() %>% arrange(desc(n)) %>% view()


# IDEAS !!!!!!!!!!!!!!!!!!!!!!!!!!



          ####  Occupation of astronauts per Sex per age when mission happened  (ANIMATION ~ age)  <age in bins?> ####



         ####  Number of (astronauts per missions) per nationality per sex over time  (ANIMATION ~ year_of_missions) ####



        ####  Number of total hours in missions per astronaut and genre (IMAGE) ####





