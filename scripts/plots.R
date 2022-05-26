

eq_lex_df = left_join(eq_score, lex_final, by = "prolific.id")

eq_lex_lab_df = left_join(eq_lex_df, labv, by = "prolific.id")

eq_lex_df %>% 
  ggplot(aes(x = eq_score, y = lextale_avg)) + geom_point() + 
  geom_smooth(method = "lm")

ex = eq_lex_lab_df %>% 
  distinct(prolific.id, .keep_all = TRUE)