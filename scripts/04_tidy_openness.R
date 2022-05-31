# Tidy openness
# scoring sheet: https://ipip.ori.org/new_ipip-50-item-scale.htm 

# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "00_libs.R"))


big5_score = read.csv(here("data", "raw", "big5_trials.csv"))

all_data = read.csv(here("data", "raw", "all_data.csv")) %>% 
  select(prolific.id, X1.2: X50.2) %>% 
  slice(4:46) %>%   
  pivot_longer(c(X1.2: X50.2), names_to = "question", values_to = "response") %>% 
  filter(prolific.id != "v") %>% 
  mutate(question = sub('X','', question)) %>% 
  mutate(item_no = rep(1:50, times = 42)) 

big5_df = left_join(all_data, big5_score, by = "item_no")


open_df = big5_df %>% 
  filter(inv_num == 1) %>% 
  mutate(score = case_when(
    response == "Strongly disagree" & is_positive == 1 ~ 1,
    response == "Slightly disagree" & is_positive == 1 ~ 2,
    response == "Neutral" & is_positive == 1 ~ 3,
    response == "Slightly agree" & is_positive == 1 ~ 4,
    response == "Strongly agree" & is_positive == 1 ~ 5,
    response == "Strongly disagree" & is_positive == 0 ~ 5,
    response == "Slightly disagree" & is_positive == 0 ~ 4,
    response == "Neutral" & is_positive == 0 ~ 3,
    response == "Slightly agree" & is_positive == 0 ~ 2,
    response == "Strongly agree" & is_positive == 0 ~ 1,
 )) %>% 
  group_by(prolific.id) %>% 
  summarize(open_score = sum(score))


open_df %>% 
  write.csv(here("data", "tidy", "open_df.csv"))


