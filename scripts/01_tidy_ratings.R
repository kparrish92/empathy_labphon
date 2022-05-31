source(here::here("scripts", "00_libs.R"))


desc_df_r = read.csv(here("data", "raw", "rater_info.csv")) %>% 
  filter(status == "APPROVED") %>% 
  select(participant_id, age, Country.of.Birth, Current.Country.of.Residence,
         First.Language, Nationality, Sex) %>% 
  rename(participant = participant_id)

glimpse(raw_data)

raw_data <- dir_ls(here("data", "ratings"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c")) %>% 
  select(eng_proficiency.response,
         eng_proficiency_2.response,
         `Cuántos años tenía cuando ud. empezó a aprender el inglés?`,
         `Ha enseñado el español cómo seguna lengua?`, 
         accent_rating.response, 
         accent_rating_2.response, 
         stim, 
         participant) %>% 
  rename("eng_proficiency" = eng_proficiency_2.response) %>%
  rename("daily_eng_use" = eng_proficiency.response) %>%
  rename("L2_aoo" = `Cuántos años tenía cuando ud. empezó a aprender el inglés?`) %>% 
  rename("teaching" = `Ha enseñado el español cómo seguna lengua?`) %>% 
  rename("accentedness" = accent_rating.response) %>% 
  rename("comprehensibility" = accent_rating_2.response) %>% 
  separate(stim, into = c('id', "trash")) %>% 
  filter(!is.na(participant))

check_df = raw_data %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n == 78) %>% 
  filter(participant %in% desc_df_r$participant)

raw_data$accentedness = as.numeric(raw_data$accentedness)
raw_data$comprehensibility = as.numeric(raw_data$comprehensibility)

ratings_df = raw_data %>% 
  group_by(id) %>% 
  summarize(mean_acc = mean(accentedness), mean_comp = mean(comprehensibility))

ratings_tidy = raw_data %>% 
  filter(participant %in% check_df$participant) %>% 
  left_join(., desc_df_r, by = "participant")


ratings_tidy %>% 
  write.csv(here("data", "tidy", "ratings_tidy.csv"))

ratings_df %>% 
  write.csv(here("data", "tidy", "ratings_df.csv"))






