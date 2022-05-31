# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "05_load_data.R"))

# -----------------------------------------------------------------------------

all_data = read.csv(here("data", "raw", "all_data.csv")) %>% 
  select(prolific.id, aoo, aoa) %>% 
  slice(4:46) %>% 
  filter(prolific.id != "v")

all_data$aoo = as.numeric(all_data$aoo)

analysis_df_m %>% 
  ggplot(aes(x = mean_acc, y = eq_score)) + 
  geom_point(alpha = .25) + geom_smooth(method = "lm")

analysis_df_m %>% 
  filter(!prolific.id %in% early_bil) %>% 
  ggplot(aes(x = mean_acc, y = eq_score)) + 
  geom_point(alpha = .25) + geom_smooth(method = "lm")

#mod0 = brm(accentedness ~ 1 + (1 | prolific.id), data = analysis_df)
mod1 = brm(accentedness ~ eq_score + open_score + lextale_avg + 
             (1 | prolific.id) + (1 | participant),
           warmup = 1000, iter = 2000, chains = 4, 
           cores = parallel::detectCores(), 
           control = list(adapt_delta = 0.99, max_treedepth = 15),
           data = analysis_df)

# comp
mod2 = brm(comprehensibility ~ eq_score + open_score + lextale_avg + 
             (1 | prolific.id) + (1 | participant),
           warmup = 1000, iter = 2000, chains = 4, 
           cores = parallel::detectCores(), 
           control = list(adapt_delta = 0.99, max_treedepth = 15),
           data = analysis_df)


prior_summary(mod1)

fixef(mod1)

fixef(mod2)

conditional_effects(mod1)

mcmc_plot(mod1)
mcmc_plot(modfull_1)

x = rope(mod1)

plot(x)

rope(mod1)

posterior <- as.array(mod2)

mc_plot = mcmc_areas(
  posterior, 
  pars = c("b_eq_score", "b_open_score", "b_lextale_avg"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)




fixef_df = fixef(mod2) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(parameter = rowname) %>% 
  mutate(parameter = paste0("b_",parameter), sep = "") %>% 
  filter(parameter != "b_Intercept")


mc_plot + xlim(-.5, .5) + geom_vline(xintercept = .23, linetype = "dashed", 
                                     color = "red") + 
  geom_vline(xintercept = -.23, linetype = "dashed",
             color = "red") + 
  geom_text(data = mutate_if(fixef_df, is.numeric, round, 2),
              aes(label = paste(Estimate, "[",
                                Q2.5, "-", Q97.5,
                                "]"), x = Inf), 
            hjust = "inward",
            family = "Times", size = 2) +
  labs(
    title = "Comprehensibility Ratings as a function of Empathy, Openness and Proficiency",
    subtitle = "Posterior Distributions with means and 80% intervals"
  ) + 
  ylab("Predictor") + xlab("Effect in Accent Rating Units") + 
  scale_y_discrete(labels=c("b_lextale_avg" = "LexTALE Score",
                            "b_open_score" = "Openness",
                            "b_eq_score" = "Empathy")) + 
  theme(text=element_text(size=10, family="Times")) +
  ggsave(here("poster", "figs", "comp_plot.png"), dpi = 300)
                   

