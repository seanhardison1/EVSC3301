library(tidyverse)

grades <- read.csv(here::here("data/output/grades.csv")) %>% 
  mutate(Participation = 
           dplyr::select(.,-1:-5) %>% 
           rowSums()) %>% 
  dplyr::select(1,2,Participation) %>% 
  dplyr::rename("# Student Name" = `X..Student.Name`,
                `Student ID` = Student.ID) 

grades %>% 
  write.csv(., file = here::here("data/output/intermediate_participation.csv"),
            row.names = F)
