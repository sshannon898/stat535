  #### Author Stephen Shannon
  #### Date 11/30/2024

  #### STAT 535 Final Project

  #### load libraries ----
  
library(tidyverse)
library(data.table)
  
  #### load data ----
  
local_path <- "C:/Users/sshan/Desktop/umass amherst/STA535/final_project/stat535/data/"  

jobs1 <- read.csv(paste0(local_path, "datajobs/postings.csv"))

  #### clean data ----

word_to_number <- function(text) {
  word_map <- c(
    "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
    "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9, "ten" = 10,
    "eleven" = 11, "twelve" = 12, "thirteen" = 13, "fourteen" = 14,
    "fifteen" = 15, "sixteen" = 16, "seventeen" = 17, "eighteen" = 18,
    "nineteen" = 19, "twenty" = 20
  )
  
  text <- tolower(text)
  for (word in names(word_map)) {
    text <- str_replace_all(text, paste0("\\b", word, "\\b"), as.character(word_map[[word]]))
  }
  text
}

jobs1_temp <- jobs1 %>% filter(grepl("\\$", job_summary)) %>%
    # Get jobs with salaries
  filter(grepl("\\$", job_summary, job_summary, ignore.case = TRUE)) %>%
    # flag jobs that ask for experience
  mutate(experience = ifelse(grepl("experience| year", job_summary), 1, 0)) %>%
  mutate(
      # Convert "k" to ,000 for "$150k-$220k"
    job_summary = word_to_number(job_summary),
    job_summary = str_replace_all(job_summary, "(?i)\\$\\d+[kK](?=-|\\b|\\s)", function(x) {
      str_replace(x, "[kK]", ",000")
    }),
    
    # Replace "K" or "k" in the second number of ranges
    job_summary = str_replace_all(job_summary, "(?i)\\-\\s*\\$?\\d+[kK]\\b", function(x) {
      str_replace(x, "[kK]", ",000")
    }),
    # Ignore patterns such as "raised \\d$ million or billion
    x = ifelse(
      # grepl("\\$\\s*\\d+\\s+to\\s+\\d+|\\$\\s*\\d+\\s*m|\\$\\s*\\d+\\s*b|\\$\\s*\\d{1,3}(?:,\\d{3})*(?:\\.\\d{2})?\\s*m|\\$\\s*\\d{1,3}(?:,\\d{3})*(?:\\.\\d{2})?\\s*b|\\$\\s*(\\d{1,4}|[12]\\d{4})(?:/year|\\s+a\\s+year|yr|\\s+annually|\\s+yr|\\s+year)", job_summary, ignore.case = TRUE),
      
      grepl("\\$\\s*\\d+\\s+to\\s+\\d+|\\$\\s*\\d+\\s*m|\\$\\s*\\d+\\s*b|\\$\\s*\\d{1,3}(?:,\\d{3})*(?:\\.\\d{2})?\\s*m|\\$\\s*\\d{1,3}(?:,\\d{3})*(?:\\.\\d{2})?\\s*b|\\$\\s*(\\d{1,4}|[12]\\d{4})(?:/year|\\s+a\\s+year|yr|\\s+annually|\\s+yr|\\s+year)", job_summary, ignore.case = TRUE),
      str_extract(str_extract(job_summary, "\\s*\\$?\\d{1,3}(,\\d{3})*\\s*\\-\\s*\\$?\\d{1,3}(,\\d{3})*"), "\\$\\d{1,3}(?:,\\d{3})*|\\$\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?"),
      str_extract(job_summary, "\\$\\d{2,}(,\\d{3})*|\\$\\s*\\d+\\s+to\\s+\\d+|\\$\\s*\\d+\\s+and\\s+\\d+")),
    
    # x = ifelse(is.na(x) == TRUE, str_extract(str_extract("\\s*\\$?\\d{1,3}(,\\d{3})*\\-\\s*\\$?\\d{1,3}(,\\d{3})*", job_summary),"\\$\\d{1,3}(?:,\\d{3})*"), NA),
               
    x = str_remove(x, "\\$"),

    y = str_extract(job_summary, "\\-\\s*\\$?\\d{1,3}(,\\d{3})*|\\$\\s*\\d+\\s+to\\s+\\d+"),

    y = str_remove(y, "^-\\s*\\$?"),

      # there are often different years needed for different skills
      # and sometimes it will capture the amount of years of something irrelevant
    min_year = as.numeric(str_remove(
      str_extract(job_summary, "(?i)(?m)(?<!\\b(for|over|last|nearly|more than|, even|through|with a|next|be at least|must be|average of)\\s)(\\b\\(?\\d+\\+?\\)?)(?=\\s*(-|\\n)\\s*\\(?\\d+\\+?\\)?\\s*(or more y|yrs|yr|year|years)\\b|\\s*(year|years)\\b)"),
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
  #### visualizations ----

jobs2_temp <- jobs1_temp %>%
  mutate(
    salary_low = as.numeric(str_remove_all(x, ",")),
    salary_high = as.numeric(str_remove_all(y, ",")),
    salary_low = ifelse(is.na(salary_low) == FALSE & is.na(salary_high) == FALSE & salary_low * 1000 < salary_high & salary_low < 1000, salary_low * 1000, salary_low),
    salary_low = ifelse(salary_low < 150, salary_low * 40 * 52, salary_low),
    salary_high = ifelse(salary_high < 200, salary_high * 40 * 52, salary_high),
    salary_low = ifelse(salary_low >= 200 & salary_low <= 15000, salary_low * 12, salary_low),
    salary_high = ifelse(salary_high >= 200 & salary_high <= 15000, salary_high * 12, salary_high)
  ) %>%
  arrange(salary_low)
jobs2_temp <- jobs2_temp[-c(1:15),]

jobs3_temp <- jobs2_temp %>% 
  mutate(
    role = case_when(
      grepl("\\b(ml|ai|nlp|machine learning|machine)\\b", job_title, ignore.case = TRUE) ~ "Machine Learning",
      grepl("\\b(programmer|developer|software)\\b", job_title, ignore.case = TRUE) ~ "Software",
      grepl("\\b(scientist|science)\\b", job_title, ignore.case = TRUE) ~ "Data Scientist",
      grepl("\\b(architect|engineer)\\b", job_title, ignore.case = TRUE) ~ "Data Engineer",
      grepl("\\b(analyst|analysis|analytic)\\b", job_title, ignore.case = TRUE) ~ "Data Analyst",
      TRUE ~ "Data Analyst"
    )
  ) %>%
  mutate(
    is_junior = grepl("\\b(junior|jr|associate|entry|intern)\\b", job_title, ignore.case = TRUE),
    is_senior = grepl("\\b(senior|snr|sr\\.|sr|ii|iii|iv|lead|principal|head|president|vice|advanced)\\b", job_title, ignore.case = TRUE)
  ) %>%
  relocate(job_title, role, is_junior, is_senior) %>%
  filter(!grepl("Canada|Australia|United Kingdom", job_location)) %>%
  distinct(job_summary, .keep_all = TRUE) %>%
  filter(!grepl("Crossover", company)) %>%
  mutate(level = ifelse(is_senior == TRUE, "Senior", "Standard")) %>%
  # filter(is.na(role) == FALSE) %>%
  # mutate(min_year = ifelse(experience == 0, 0, min_year)) %>%
  # mutate(excel = ifelse(grepl("Excel",job_skills, ignore.case = TRUE), 1, 0),
  #        r = ifelse(grepl(" R,| R ,RStudio", job_skills, ignore.case = TRUE),1,0),
  #        python = ifelse(grepl("python", job_skills, ignore.case = TRUE),1,0)
  #        )
  mutate(lang_flag = case_when(
    grepl("Excel|excel",job_skills, ignore.case = TRUE) ~ "MS Excel",
    grepl(" (R,| R ,|RStudio|python|scipy|numpy|pandas)",job_skills, ignore.case = TRUE) ~ "R or Python",
    TRUE ~ "MS Excel")) %>%
  mutate(min_year = ifelse(is.na(min_year) == TRUE & is_junior == TRUE, 0, min_year),
         min_year = ifelse(is.na(min_year) == TRUE &grepl("\\b(bachelor)\\b", job_summary, ignore.case = TRUE), 0, min_year),
         min_year = ifelse(is.na(min_year) == TRUE &grepl("\\b(masters|master deg|master's|)\\b", job_summary, ignore.case = TRUE), 2, min_year),
         min_year = ifelse(is.na(min_year) == TRUE &grepl("\\b(phd|ph.d|ph. d|post doc|doctoral|)\\b", job_summary, ignore.case = TRUE), 5, min_year)
  )

  




na_jobs <- jobs3_temp %>% filter(is.na(min_year) == TRUE | is.na(salary_low) == TRUE)


# " R,| R ,RStudio|python"

# Histogram for salary low and high
# min_salary <- ggplot(
#   jobs3_temp, aes(x = salary_low)) +
#   geom_histogram(bins = 30, fill = "blue", alpha = 0.5, color = "black") +
#   labs(title = "Distribution of Low Salaries", x = "Low Salary ($)", y = "Frequency") +
#   theme_bw(base_size = 16)

min_salary_role <- ggplot(
  jobs3_temp, aes(x = salary_low/1000, group = role, fill = role)) +
  geom_histogram(bins = 30, alpha = 0.75) +
  theme_bw(base_size = 16) + 
  scale_x_continuous(n.breaks = 6) + 
  labs(title = "",
       x = "Annual Minimum Salary (Thousands $)",
       y = "Frequency") +
  theme_bw(base_size = 32) +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24))






# experience (min_year) vs salary_low
exp_salary_scatter <- ggplot(
  jobs3_temp, aes(x = min_year, y = salary_low/1000, group = role, color = role)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(n.breaks = 6, limits = c(0,10)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "",
       x = "",
       y = "Annual Minimum Salary (Thousands $)") +
  theme_bw(base_size = 32) +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24))

salary_summary <- jobs3_temp %>%
  group_by(role, level) %>%
  summarize(n = n())

salary_role <- ggplot(
  data = jobs3_temp, aes(x = role, y = salary_low/1000, fill = level)) +
  geom_boxplot() +
  geom_text(data = salary_summary, aes(x = role,
                                       y = 1,
                                       group = level,
                                       color = level,
                                       label = paste0("n = ", n)),
                                       position = position_dodge(width = 1),
            show.legend = FALSE,
            size = 10) +
  scale_y_continuous(n.breaks = 6) + 
  theme_bw(base_size = 32) + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        legend.title = element_blank()) +
  labs(x = "",
       y = "Annual Minimum Salary (Thousands $)")

exp_summary <- jobs3_temp %>%
  group_by(role, level) %>%
  summarize(n = n())


exp_role <- ggplot(
  data = jobs3_temp, aes(x = role, y = min_year, fill = level)) +
  geom_boxplot() +
    geom_text(data = exp_summary, aes(x = role,
                                         y = 0.5,
                                         group = level,
                                         color = level,
                                         label = paste0("n = ", n)),
              position = position_dodge(width = 1),
              show.legend = FALSE,
              size = 10) +
      theme_bw(base_size = 32) + 
  scale_y_continuous(n.breaks = 6) + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        legend.title = element_blank()) + 
  labs(x = "",
       y = "Minimum Years of Experience")

# ggplot(data = jobs3_temp, aes(x = role, y = salary_low, fill = level)) +
#   geom_boxplot() +
#   theme_bw(base_size = 16) + 
#   theme(panel.background = element_blank(), 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) +
#   labs(x = "",
#        y = "Minimum Salary (Thousands $)")




  #### regression ----

job_reg_data <- jobs3_temp %>% select(salary_low, role, min_year, is_senior, lang_flag)
job_reg_data_temp <- job_reg_data %>% na.omit()

m1 <- lm(salary_low ~ role + min_year * is_senior + lang_flag, data = job_reg_data_temp)
m2 <- lm(salary_low ~ role + min_year + is_senior + lang_flag, data = job_reg_data_temp)
m3 <- lm(salary_low ~ role + min_year * is_senior, data = job_reg_data_temp)
m4 <- lm(salary_low ~ role + min_year, data = job_reg_data_temp)
m5 <- lm(salary_low ~ min_year, data = job_reg_data_temp )
anova(m1, m2, m3, m4, m5)

# step(m1, direction = "both")
# step


library(lme4)

mf1 <- lmer(salary_low ~ min_year*is_senior + (1|role) + lang_flag, data = job_reg_data_temp)


stargazer(m1,mf1,
          dep.var.labels = "Minimum Salary",
          intercept.top = TRUE,
          intercept.bottom = FALSE,
          covariate.labels = c("(Intercept)", "Role: Data Eng.", "Role: Data Sci.", "Role: ML/AI", "Role: Software", "Experience", "Senior", "R or Python","Experience:Senior"),
          single.row = TRUE,
          omit.stat = c("rsq", "ser", "bic", "ll", "aic"),
          digits = 2,
          add.lines = list(c("AIC", round(AIC(m1),1), round(AIC(mf1),1)),
                           c("Log-Likelihood", round(logLik(m1), 1), round(logLik(mf1),1))),
          dep.var.caption = NULL,
          p.auto = TRUE,
          se = NULL,
          header = FALSE,
          notes.align = "l"
          )





# mf2 <- lmer(salary_low ~ min_year + (1|role) + is_senior + lang_flag, data = job_reg_data_temp)
# # summary(m2)
# 
# mf3 <- lmer(salary_low ~ min_year + (1|role) + is_senior + (1|lang_flag), data = job_reg_data_temp)
# # summary(m3)
# 
# mf4 <- lmer(salary_low ~ min_year + (1|role) + is_senior, data = job_reg_data_temp)
# # summary(m4)
# 
# mf5 <- lmer(salary_low ~ min_year*is_senior + (1|role), data = job_reg_data_temp)
# summary(m5)

# mf6 <- lmer(salary_low ~ (min_year|role), data = job_reg_data_temp)
# mf7 <- lmer(salary_low ~ (min_year|role) + is_senior, data = job_reg_data_temp)
# mf8 <- lmer(salary_low ~ (min_year|role) + is_senior + lang_flag, data = job_reg_data_temp)
# mf9 <- lmer(salary_low ~ (min_year|role)*is_senior + lang_flag, data = job_reg_data_temp)
# mf10 <- lmer(salary_low ~ min_year + (min_year|role) + lang_flag, data = job_reg_data_temp)
# mf11 <- lmer(salary_low ~ min_year + (0 + min_year|role) + lang_flag, data = job_reg_data_temp)
# mf12 <- lmer(salary_low ~ min_year + (1 + min_year|role) + lang_flag, data = job_reg_data_temp)


# ranef_data <- ranef(mf10)$role
# ranef_data$role <- rownames(ranef_data)
# colnames(ranef_data) <- c("Intercept", "Slope", "role")
# 
# # Plot random intercepts vs slopes
# ggplot(ranef_data, aes(x = Intercept, y = Slope, label = role)) +
#   geom_point() +
#   geom_text(vjust = 1.2, hjust = 0.5) +
#   labs(title = "Random Intercepts and Slopes for Roles",
#        x = "Random Intercept (Baseline Salary)",
#        y = "Random Slope (Salary Increase Rate)") +
#   theme_minimal()



anova(mf1, mf2, mf3, mf4, mf5)
anova(mf6, mf7, mf8, mf9, mf10, mf12 )


  #### power study ----
  # At what point does an increase in years of experience truly lead to a significant difference in salary?
  # When does 

hybrid_data <- function(data_summary) {
  data_summary %>%
    mutate(
      sampled_means = sample(mean_salary, replace = TRUE),
      simulated_salaries = list(rnorm(n, sampled_means, sd_salary))
    ) %>%
    unnest(cols = c(simulated_salaries))
}









sample_data_function <- function(df, title, size = NULL) {
  # Optionally specify sample size
  # message(title)
  sampled_data <- df %>%
    filter(role == title) %>%
    group_by(min_year) %>%
    sample_n(ifelse(is.null(size), n(), size), replace = TRUE) %>%
    ungroup()
  return(sampled_data)
}

min_year_sample <- sample_data_function(job_reg_data_temp, title = "Data Analyst")

t_test_func <- function(df, alpha, x1, x2, title) {
  # t_test_func(sim_data, 0.05)
  # print(alpha)
  stopifnot(exprs =
              {
                alpha > 0
                alpha < 1
                is.numeric(x1)
                is.numeric(x2)
                x1 >= 0
                x2 > x1
              })
  if (
    nrow(df %>% filter(min_year == x1, role == title)) < 1|
         nrow(df %>% filter(min_year == x2, role == title)) < 1) {
    t_test = 1
  } else {
    t_test <- t.test(
      x = df %>% filter(min_year == x1, role == title) %>% select(salary_low),
      y = df %>% filter(min_year == x2, role == title) %>% select(salary_low), 
      conf.level = 1 - alpha)
  }
  
  return(t_test$p.value < alpha)
  
}


t_test_func(min_year_sample, .05, 0, 1, title = "Data Analyst")

power_estimate_function <- function(df, alpha, x1, x2, title, s, n = NULL) {
  
  test_results <- unlist(
    replicate(s,
              {
                sample_data <- sample_data_function(df, title, size = n)
                test_results <- t_test_func(sample_data, alpha, x1, x2, title)
                
              },
              simplify = FALSE
    )
  )
  return(sum(test_results)/length(test_results))

}

x <- power_estimate_function(df = job_reg_data_temp, alpha = .05, x1 = 0, x2 = 1, s = 100)

y <- c( 1, 2, 3, 4, 5, 6,7,8,9,10)


data_analyst_power <- sapply(y, function(x) {

  power_estimate_function(df = job_reg_data_temp, alpha = .05, x1 = x-1, x2 = x, title = "Data Analyst", s = 200)
})

data_eng_power <- sapply(y, function(x) {

  power_estimate_function(df = job_reg_data_temp, alpha = .05, x1 = x-1, x2 = x, title = "Data Engineer", s = 200)
})

data_scientist_power <- sapply(y, function(x) {

  power_estimate_function(df = job_reg_data_temp, alpha = .05, x1 = x-1, x2 = x, title = "Data Scientist", s = 200)
})

ml_power <- sapply(y, function(x) {

  power_estimate_function(df = job_reg_data_temp, alpha = .05, x1 = x-1, x2 = x, title = "Machine Learning", s = 200)
})

software_power <- sapply(y, function(x) {

  power_estimate_function(df = job_reg_data_temp, alpha = .05, x1 = x-1, x2 = x, title = "Software", s = 200)
})

jobs_power <- sapply(y, function(x) {
  power_vec <- power_estimate_function(df = job_reg_data_temp, alpha = .05, x1 = x-1, x2 = x, s = 100)
  return(power_vec)
})


true_diff <- job_reg_data_temp %>% filter(min_year <= 10) %>%
  group_by(min_year) %>%
  summarize(mean_salary = mean(salary_low)) %>%
  mutate(lag_mean_salary = dplyr::lag(mean_salary),
         mean_diff = lag_mean_salary - mean_salary)

df <- data.frame(
  year = seq(1:10),
  power_vec = jobs_power,
  true_diff = true_diff$mean_diff %>% na.omit()
)

df2 <- data.frame(
  `Data Analyst` = data_analyst_power,
  `Data Engineer` = data_eng_power, 
  `Data Science` = data_scientist_power,
  Software = software_power,
  ML = ml_power,
  year = seq(1:10)
) %>% pivot_longer(1:5, names_to = "Role", values_to = "Power")


power_plot <- ggplot(data = df, aes(x = year, y = power_vec)) +
  geom_line() +
  theme_bw(base_size = 32) + 
  scale_x_continuous(n.breaks = 6) + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        legend.title = element_blank()) + 
  labs(x = "Year N",
       y = "Testing Power")

per_role_plot <- ggplot(data = df2, aes(x = year, y = Power, group = Role, color = Role)) +
  geom_line() +
  theme_bw(base_size = 32) + 
  scale_x_continuous(n.breaks = 5) + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        legend.title = element_blank()) + 
  labs(x = "Year N",
       y = "Testing Power ")


