library(tidyverse)
library(magrittr)
library(lubridate)

pe_dir <- here::here("data/PollEverywhere")
out_dir <- here::here("data/output")

# read template
template <- read.csv(here::here("data/gradebook-21Sp_Atmosphere_and_Weather-ALL.csv")) %>%
  as_tibble() %>% 
  dplyr::rename(id = 1,
                student = 2) %>% 
  separate(student, into = c("Last","First"), ", ") %>% 
  unite(col = "student", c(First, Last), sep = " ")

# read PE data
df <- tibble()
for (i in 1:length(list.files(pe_dir))){
  
  all_lines <- suppressWarnings(readLines(file.path(pe_dir,
                                list.files(pe_dir)[i])))
  
  skip <- which(all_lines == "Individual Results") + 1
  
  init <- read.csv(file.path(pe_dir,
                             list.files(pe_dir)[i]),
                   skip = skip) %>% 
    as_tibble() %>% 
    dplyr::rename(student = Registered.participant) %>% 
    dplyr::select(Created.At, student) %>% 
    mutate(Participation = 1,
           Created.At = round_date(ymd_hms(Created.At), unit = "10 minutes")) %>% 
    right_join(.,template) %>% 
    mutate(Participation = replace_na(Participation, 0),
           Created.At = mean(unique(Created.At), na.rm = T)) 
  
  assign('df', rbind(df, init))
}

# output grades for import into collab
grades <- 
  df %>% 
  group_by(lect_id = paste0("Participation ",as.Date(Created.At)), 
           `Student ID` = id,
           "# Student Name" = student) %>% 
  dplyr::summarise(Participation = sum(Participation)) %>% 
  pivot_wider(names_from = lect_id, values_from = Participation)

write.csv(grades, file = file.path(out_dir, "grades.csv"), row.names = F)
