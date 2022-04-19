

desc_df = pct_tidy %>% 
  group_by(L1, language_chosen, phoneme, stim_language) %>% 
  summarize(n = n())

desc_df %>% 
  filter(L1 == "English") %>% 
  ggplot(aes(x = language_chosen, y = n, 
             fill = language_chosen)) +  
  geom_col(color = "black") +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  facet_grid(stim_language~phoneme) + 
  ggtitle("Raw categorization data English L1")

desc_df %>% 
  filter(L1 == "Spanish") %>% 
  ggplot(aes(x = language_chosen, y = n, 
             fill = language_chosen)) +  
  geom_col(color = "black") +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  facet_grid(stim_language~phoneme) + 
  ggtitle("Raw categorization data Spanish L1")
  
  
log_df = fixef(log_1_e) %>% 
  data.frame() %>% 
  dplyr::select(Estimate, Q2.5, Q97.5) %>%
  t() %>% 
  data.frame() %>% 
  mutate(prob_i_fr = plogis(Intercept),
         prob_o_fr = plogis(Intercept + phonemeo),
         prob_schwa_fr = plogis(Intercept + phonemeschwa),
         prob_y_fr = plogis(Intercept + phonemey),
         prob_i_ger = plogis(Intercept + stim_languageGerman),
         prob_o_ger = plogis(Intercept + phonemeo + 
                               phonemeo.stim_languageGerman),
         prob_schwa_ger = plogis(Intercept + phonemeschwa +
                                   phonemeschwa.stim_languageGerman),
         prob_y_ger = plogis(Intercept + phonemey + 
                               phonemey.stim_languageGerman))
  

log_df_s = fixef(log_1_s) %>% 
  data.frame() %>% 
  dplyr::select(Estimate, Q2.5, Q97.5) %>%
  t() %>% 
  data.frame() %>% 
  mutate(prob_i_fr = plogis(Intercept),
         prob_o_fr = plogis(Intercept + phonemeo),
         prob_schwa_fr = plogis(Intercept + phonemeschwa),
         prob_y_fr = plogis(Intercept + phonemey),
         prob_i_ger = plogis(Intercept + stim_languageGerman),
         prob_o_ger = plogis(Intercept + phonemeo + 
                               phonemeo.stim_languageGerman),
         prob_schwa_ger = plogis(Intercept + phonemeschwa +
                                   phonemeschwa.stim_languageGerman),
         prob_y_ger = plogis(Intercept + phonemey + 
                               phonemey.stim_languageGerman))



lx_e = log_df %>% 
  select(prob_i_fr,prob_o_fr,prob_schwa_fr,prob_y_fr,
         prob_i_ger,prob_o_ger,prob_schwa_ger,prob_y_ger) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column("level") %>% 
  separate(level, into = c("prob", "phoneme", "stim_language")) %>% 
  mutate(L1 = "English")

lx_s = log_df_s %>% 
  select(prob_i_fr,prob_o_fr,prob_schwa_fr,prob_y_fr,
         prob_i_ger,prob_o_ger,prob_schwa_ger,prob_y_ger) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column("level") %>% 
  separate(level, into = c("prob", "phoneme", "stim_language")) %>% 
  mutate(L1 = "Spanish")

lx = rbind(lx_e, lx_s)


lx %>% 
  ggplot(aes(y = Estimate, x = phoneme, 
             fill = stim_language)) +
geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), shape = 21, 
                position = position_dodge(width = .5)) +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  ylim(0,1) + theme(panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(size = 0.05, linetype = 'solid',
                                                    colour = "blue"),
                     legend.position = "bottom") + 
  ggtitle("Probabilities of choosing a Spanish word") +
  facet_wrap(~L1)


