  #### Author Stephen Shannon
  #### Date 11/30/2024

  #### STAT 535 Final Project

  #### load libraries ----
  
library(tidyverse)
library(data.table)
  
  #### load data ----
  
local_path <- "C:/Users/sshan/Desktop/umass amherst/STA535/final_project/stat535/data/"  

jobs1 <- read.csv(paste0(local_path, "datajobs/postings.csv"))

jobs1_temp <- jobs1 %>% filter(grepl("\\$", job_summary)) %>%
    # Get jobs with salaries
  filter(grepl("\\$", job_summary) & !grepl("\\$\\s*\\d+\\s*m|\\$\\s*\\d+\\s*b", job_summary, ignore.case = TRUE)) %>%
    # flag jobs that ask for experience
  mutate(experience = ifelse(grepl("experience| year", job_summary), 1, 0)) %>%
  mutate(
      # Ignore patterns such as "raised \\d$ million or billion
    x = ifelse(
      grepl("\\$\\s*\\d+\\s*m|\\$\\s*\\d+\\s*b", job_summary, ignore.case = TRUE),
      NA,
      str_extract(job_summary, "\\$\\d{2,}(,\\d{3})*")),

    x = str_remove(x, "\\$"),

    y = str_extract(job_summary, "\\-\\s*\\$?\\d{1,3}(,\\d{3})*"),

    y = str_remove(y, "^-\\s*\\$?"),

      # there are often different years needed for different skills
      # and sometimes it will capture the amount of years of something irrelevant
    min_year = as.numeric(str_remove(
      str_extract(job_summary, "(?i)(?m)(?<!\\b(for|over|last|nearly|more than|, even|through|with a|next|be at least|must be|average of)\\s)(\\b\\(?\\d+\\+?\\)?)(?=\\s*(-|\\n)\\s*\\(?\\d+\\+?\\)?\\s*(year|years)\\b|\\s*(year|years)\\b)"),
      "[\\+\\(\\)]"
    )),
    end_year = as.numeric(str_remove(
      str_remove(
        str_extract(job_summary, "(?i)(?m)(?<!\\b(for|over|last|nearly|more than|, even|through|with a|next|be at least|must be|average of)\\s)\\-\\s*\\(?\\d+\\+?\\)?(?=\\s*(year|years)\\b)"),
        "\\-\\s*"
      ),
      "[\\+\\(\\)]"
    ))
  )




