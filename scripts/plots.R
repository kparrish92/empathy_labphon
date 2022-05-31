

source(here::here("scripts", "03_load_data.R"))

eq_lex_df = left_join(eq_score, lex_final, by = "prolific.id")

eq_lex_lab_df = left_join(eq_lex_df, labv, by = "prolific.id")

final_df %>% 
  ggplot(aes(x = open_score, y = eq_score)) + geom_point() + 
  geom_smooth(method = "lm")

ex = final_df %>% 
  distinct(prolific.id, .keep_all = TRUE)

mean(ex$lextale_avg)
sd(ex$lextale_avg)

mean(ex$eq_score)
sd(ex$eq_score)


ratings_k = c(8,7,3,3,2,4,8,5,4,6,
              2,5,9,9,7,8,9,4,9,3,
              5,4,7,5,7,1,8,1,1,
              3,2,1,7,0,8,5,5,3,6,
              2,7) 
              

order_arr = ex %>% 
  select(rec_session_id) %>% 
  arrange(rec_session_id) %>% 
  mutate(no = c(1:41))

order_arr$ratings_k = ratings_k

order_arr$no = as.character(order_arr$no)
            
ratings_df = left_join(ex, order_arr, by = "rec_session_id") %>% 
  filter(ratings_k > 0)



library(lme4)
library(brms)

mod0 = glm(ratings_k ~ 1, data = ratings_df)
mod1 = glm(ratings_k ~ open_score, data = ratings_df)


summary(mod1)

anova(mod0, mod1)

ratings_df %>% 
  ggplot(aes(x = eq_score, y = ratings_k)) + geom_point() + 
  geom_smooth(method = "lm") + geom_vline(xintercept = 
                                            mean(ratings_df$eq_score),
                                          linetype = "dashed") + 
  geom_hline(yintercept = mean(ratings_df$ratings_k), 
             linetype = "dashed")


ratings_df %>% 
  ggplot(aes(x = open_score, y = ratings_k)) + geom_point() + 
  geom_smooth(method = "lm") + geom_vline(xintercept = 
                                            mean(ratings_df$eq_score),
                                          linetype = "dashed") + 
  geom_hline(yintercept = mean(ratings_df$ratings_k), 
             linetype = "dashed")



ratings_df %>% 
  ggplot(aes(x = lextale_avg, y = ratings_k)) + geom_point() + 
  geom_smooth(method = "lm")



ratings_k = data.frame(rating = c(8,7,3,3,2,4,8,5,4,6,
              2,5,9,9,7,8,9,4,9,3,
              5,4,7,5,7,1,8,1,1,
              3,2,1,7,0,8,5,5,3,6,
              2,7), no = c(1:41)) 
  


df_f <- character()
for(thisRun in 1:nrow(ratings_k))
{
  df <- rnorm(n = 80, mean = ratings_k$rating[thisRun], sd = .2)
  df$rater = thisRun
  df_f <- rbind(df_f, df) 
}


x = df_f %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(no = 1:41) %>% 
  select(no, V1:V80) %>% 
  pivot_longer(c(V1:V80), names_to = "rater", values_to = "response")  

x$no = as.character(x$no)

y = left_join(x, order_arr, by = "no")

y$response = as.numeric(y$response)

y %>% 
  group_by(no) %>% 
  summarize(mean_r = mean(response))

w = ratings_df %>% 
  select(eq_score, open_score, lextale_avg, rec_session_id)

f_df = left_join(y, w, by = "rec_session_id") %>% 
  filter(rec_session_id != "485052")

mod_n = lmer(response ~ 1 + (1 | no), data = f_df)
mod_eq = lmer(response ~ open_score + (1 | no), data = f_df)
mod_lex = lmer(response ~ eq_score + lextale_avg + (1 | no), data = f_df)

summary(mod_eq)

anova(mod_n, mod_eq, mod_lex)


mod_eq = brm(response ~ open_score + eq_score + (1 | no), data = f_df)

summary(mod_eq)

conditional_effects(mod_eq)

mcmc_plot(mod_eq)

fixef(mod_eq)


# 484441 
# 
