# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------

library(here)
all_data = read.csv(here("data", "raw", "all_data.csv"))

eq_trials = read.csv(here("data", "raw", "eq_trials.csv")) %>% 
  rename(question = item_num)

eq_trials$question = as.character(eq_trials$question)

eq_data = all_data %>% 
  select(prolific.id, aoo, aoa, X1:X60) %>% 
  slice(4:46) %>% 
  pivot_longer(c(X1:X60), names_to = "question", values_to = "response") %>% 
  filter(prolific.id != "v") %>% 
  mutate(question = sub('X','', question))

all_data$aoo

all_data$aoa



eq_score = left_join(eq_data, eq_trials, by = "question") %>% 
  mutate(eq_response = case_when(
    response == "Strongly agree" ~ 1,
    response == "Slightly agree" ~ 2,
    response == "Slightly disagree" ~ 4,
    response == "Slightly agree" ~ 5,
    TRUE ~ 0
  )) %>% 
  mutate(
    score = case_when(
      is_agree_score    == 1 & eq_response == 1 ~ 2, 
      is_agree_score    == 1 & eq_response == 2 ~ 1, 
      is_disagree_score == 1 & eq_response == 5 ~ 2, 
      is_disagree_score == 1 & eq_response == 4 ~ 1, 
      TRUE ~ 0)) %>% 
  group_by(prolific.id) %>%
  summarize(eq_score = sum(score))

eq_score %>% 
  write.csv(here("data", "tidy", "eq_score.csv"))

# This created and saves a condition file used for the experiment in psychopy
cond_file = data.frame(stim = list.files(path = here("data", "hunter_recordings_all", "sections"))) 

cond_file %>% 
  write.csv(here("data", "tidy", "cond_file.csv"), row.names=FALSE)
