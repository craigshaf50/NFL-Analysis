#install.packages("tidyverse")
#install.packages("nflverse")

library(tidyverse)
library(nflverse)


### Historical Data ###

# Game data
games_load<-load_schedules(2014:2022)

games<-games_load %>% 
  select(-c(5:7,12,15,16:24,28:29,31:32,35:39,42:46))





# future & current
games_2023<-load_schedules(2023) %>%
  select(-c(5:7,12,15,16:24,28:29,31:32,35:39,42:46))
