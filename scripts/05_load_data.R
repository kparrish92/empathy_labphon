# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "00_libs.R"))

eq_score = read.csv(here("data", "tidy", "eq_score.csv"))

lex_final = read.csv(here("data", "tidy", "lex_final.csv"))

open_df =  read.csv(here("data", "tidy", "open_df.csv"))

eq_lex_df = left_join(eq_score, lex_final, by = "prolific.id")


p1 = read.csv(here("data", "raw", "labvanced", "phase2.csv"))

p2 = read.csv(here("data", "raw", "labvanced", "phase1.csv"))


labv = rbind(p1,p2) %>% 
  rename(prolific.id = prolific_id) %>% 
  select(prolific.id, rec_session_id)


eq_lex_df = left_join(eq_score, lex_final, by = "prolific.id")

eq_lex_lab_df = left_join(eq_lex_df, labv, by = "prolific.id")

final_df = left_join(eq_lex_lab_df, open_df, by = "prolific.id") %>% 
  rename("id" = rec_session_id)


ratings_means = read.csv(here("data", "tidy", "ratings_df.csv"))

ratings_df = read.csv(here("data", "tidy", "ratings_tidy.csv"))

use_df = ratings_df %>% 
  filter(!is.na(daily_eng_use))

analysis_df = left_join(ratings_df, final_df, by = "id")

analysis_df_m = left_join(final_df, ratings_means, by = "id")