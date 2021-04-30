library(tidyverse)
library(magrittr)
library(lubridate)
library(rvest)

# Assignment details
assign_name <- "Exam 2"
point_total <- 48
assign_dir <- here::here(file.path(here::here("data/assignments"),
                                   assign_name))

# read assignment comments for each student
final <- tibble()
missed_qs <- tibble()
for (i in 1:length(list.files(assign_dir))){
  # File path for comments
  student_path <- file.path(assign_dir,
                            list.files(assign_dir)[i],
                            "comments.txt")
  
  # Extract student name and id
  student_name_split <- str_split(list.files(assign_dir)[i], "\\(|,")[[1]] 
  student_name <- str_trim(str_glue(student_name_split[2], " ",
                                    (student_name_split[1])))
  student_id <- str_remove(student_name_split[3], "\\)")
  
  # Read comments
  comments <- 
    read_html(student_path) %>% 
    html_nodes(xpath = '//p')%>%
    html_text()
  
  int <- NULL
  for (q in 1:length(comments)){
    int <- tibble(missed_qs = str_sub(comments[q], 0,3),
                  student_name = student_name)
    assign('missed_qs', rbind(int, missed_qs))
  }

  # Extract points lost. Should be written into comments
  points_off <- vector()
  for (j in 1:length(comments)){
    points_off[j] <- as.numeric(str_extract(str_remove_all(comments[j]," "), 
                                         "-[[:digit:]]+\\.*[[:digit:]]*"))
  }
  
  output <- tibble(`Student ID` = student_id,
                   `# Student Name` = student_name,
                   Score = point_total + sum(points_off, na.rm = T))
  
  assign("final", rbind(final, output))
  
}

names(final)[3] <- assign_name
View(final)

