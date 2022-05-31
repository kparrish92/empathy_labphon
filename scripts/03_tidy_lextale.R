# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "00_libs.R"))


# -----------------------------------------------------------------------------

library(here)
all_data = read.csv(here("data", "raw", "all_data.csv"))

lex_trials = read.csv(here("data", "raw", "lextale_trials.csv")) 


lex_data = all_data %>% 
  select(prolific.id, aoo, aoa, X1.1:X90) %>% 
  slice(4:46) %>% 
  pivot_longer(c(X1.1:X90), names_to = "question", values_to = "response") %>% 
  filter(prolific.id != "v") %>% 
  mutate(question = sub('X','', question)) %>% 
  mutate(stim = rep(1:90, times = 42)) 


lex_score = left_join(lex_data, lex_trials, by = "stim") %>% 
  mutate(response_button = case_when(
    response == "FAKE" ~ 0,
    response == "REAL" ~ 1)) %>%  
  mutate(
    response = response_button, 
    is_correct = correct_response, 
    is_incorrect = if_else(is_correct == 0, 1, 0), 
    is_real = case_when(
      response == 1 & is_correct == 1 ~ "real", 
      response == 1 & is_correct == 0 ~ "nonse", 
      response == 0 & is_correct == 1 ~ "nonse", 
      response == 0 & is_correct == 0 ~ "real"),  
    real_correct    = if_else(is_real == "real"  & is_correct == 1, 1, 0), 
    real_incorrect  = if_else(is_real == "real"  & is_correct == 0, 1, 0), 
    nonse_correct   = if_else(is_real == "nonse" & is_correct == 1, 1, 0), 
    nonse_incorrect = if_else(is_real == "nonse" & is_correct == 0, 1, 0)) %>% 
  group_by(prolific.id) %>% 
  summarize(totals = n(), 
            real_correct = sum(real_correct), 
            real_incorrect = sum(real_incorrect), 
            nonse_correct = sum(nonse_correct), 
            nonse_incorrect = sum(nonse_incorrect), .groups = "drop") %>% 
  mutate(
    n_real = real_correct + real_incorrect, 
    n_nonse = nonse_correct + nonse_incorrect, 
    n = n_real + n_nonse, 
    lextale_avg = score_lextale(
      n_real = n_real, 
      n_nonse = n_nonse,
      n_real_correct = real_correct, 
      n_nonse_correct = nonse_correct), 
    lextale_tra = score_lextale(
      n_real_correct = real_correct,
      n_nonse_incorrect = nonse_incorrect
    )
  ) %>% 
  filter(prolific.id != "6132e1c8bab771d0d0f6f3e9")


score_correct = left_join(lex_data, lex_trials, by = "stim") %>% 
  filter(prolific.id == "6132e1c8bab771d0d0f6f3e9") %>% 
  filter(stim != 1) %>% 
  mutate(response_button = case_when(
    response == "FAKE" ~ 0,
    response == "REAL" ~ 1)) %>%  
  mutate(
    response = response_button, 
    is_correct = correct_response, 
    is_incorrect = if_else(is_correct == 0, 1, 0), 
    is_real = case_when(
      response == 1 & is_correct == 1 ~ "real", 
      response == 1 & is_correct == 0 ~ "nonse", 
      response == 0 & is_correct == 1 ~ "nonse", 
      response == 0 & is_correct == 0 ~ "real"),  
    real_correct    = if_else(is_real == "real"  & is_correct == 1, 1, 0), 
    real_incorrect  = if_else(is_real == "real"  & is_correct == 0, 1, 0), 
    nonse_correct   = if_else(is_real == "nonse" & is_correct == 1, 1, 0), 
    nonse_incorrect = if_else(is_real == "nonse" & is_correct == 0, 1, 0)) %>% 
  group_by(prolific.id) %>% 
  summarize(totals = n(), 
            real_correct = sum(real_correct), 
            real_incorrect = sum(real_incorrect), 
            nonse_correct = sum(nonse_correct), 
            nonse_incorrect = sum(nonse_incorrect), .groups = "drop") %>% 
  mutate(
    n_real = real_correct + real_incorrect, 
    n_nonse = nonse_correct + nonse_incorrect, 
    n = n_real + n_nonse, 
    lextale_avg = score_lextale(
      n_real = n_real, 
      n_nonse = n_nonse,
      n_real_correct = real_correct, 
      n_nonse_correct = nonse_correct), 
    lextale_tra = score_lextale(
      n_real_correct = real_correct,
      n_nonse_incorrect = nonse_incorrect
    )
  )


lex_final = rbind(score_correct, lex_score)


lex_final %>% 
  write.csv(here("data", "tidy", "lex_final.csv"))
