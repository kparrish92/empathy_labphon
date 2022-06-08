## Descriptive tables 

source(here::here("scripts", "05_load_data.R"))

age_df = read.csv(here("data", "raw", "all_data.csv")) %>% 
  select(prolific.id, aoo, aoa) %>% 
  slice(4:46) %>% 
  filter(prolific.id != "v")


nrow(analysis_df_m %>% filter(!is.na(mean_acc)))

## Desc L2 learners      
desc_df_l2 = analysis_df_m %>% 
  filter(!is.na(mean_acc)) %>% 
  summarize("Mean Empathy" = paste0(round(mean(eq_score), digits = 2), 
                                   " (", 
                                   round(sd(eq_score), 
                                         digits = 2), 
                                   ")"),
            "Mean Extroversion" = paste0(round(mean(open_score), digits = 2), 
                                    " (", 
                                    round(sd(open_score), 
                                          digits = 2), 
                                    ")"),
            "Mean Lextale" = paste0(round(mean(lextale_avg), digits = 2), 
                                    " (", 
                                    round(sd(lextale_avg), 
                                          digits = 2), 
                                    ")"),
            "Mean Accentedness" = paste0(round(mean(mean_acc), digits = 2), 
                                    " (", 
                                    round(sd(mean_acc), 
                                          digits = 2), 
                                    ")"),
            "Mean Comprehensibility" = paste0(round(mean(mean_comp), digits = 2), 
                                    " (", 
                                    round(sd(mean_comp), 
                                          digits = 2), 
                                    ")")) %>% 
  pivot_longer(cols = 1:5, names_to = "Measure", 
               values_to = "Value")


use_df = ratings_df %>% 
  filter(!is.na(daily_eng_use))

mean(use_df$daily_eng_use)
     
use_df$teaching

# Desc raters 

means_df_raters = ratings_df %>% 
  summarize("Age" = paste0(round(mean(age), digits = 2), 
                           " (", 
                           round(sd(age), 
                                 digits = 2), 
                           ")")) %>% 
  mutate("N" = paste0(length(unique(ratings_df$participant)), 
                             " (Male = ", nrow(ratings_df %>% 
                                          group_by(participant, Sex) %>% 
                                          summarize(n = n()) %>% 
                                          filter(Sex == "Male")),")")) %>% 
    mutate("Percentage Daily English Use" = paste0(round(mean(use_df$daily_eng_use)*10, digits = 2), 
                                                " (", 
                                                round(sd(use_df$daily_eng_use)*10, 
                                                      digits = 2), 
                                                ")")) %>% 
    mutate("Self-rated English proficiency" = paste0(round(mean(use_df$eng_proficiency), digits = 2), 
                                                   " (", 
                                                   round(sd(use_df$eng_proficiency), 
                                                         digits = 2), 
                                                   ")")) %>% 
  pivot_longer(cols = 1:4, names_to = "Measure", 
               values_to = "Value") %>% 
  mutate("Range" = c("18-58", "N/A", "10-99.5", "1-9"))



  
means_df_raters %>% 
  write.csv(here("data", "tidy", "rater_desc.csv"))

desc_df_l2 %>% 
  write.csv(here("data", "tidy", "l2_desc.csv"))


  