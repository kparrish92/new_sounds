
source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "03_load_data.R"))


df = read.csv(here("data", "tidy", 
                    "int_only_plot_data.csv")) %>% 
  mutate(group = "Spanish L1")
              
df_eng = read.csv(here("data", "tidy", 
                     xf    "int_only_plot_data_eng.csv")) %>% 
  mutate(group = "English L1")

comb_df = rbind(df, df_eng)

df %>%  
  ggplot(aes(x = Estimate, y = reorder(level, +Estimate), 
             fill = language)) +
  geom_col(color = "black") +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  geom_linerange(aes(xmin = Q2.5, xmax = Q97.5),
                 color = pl[2], size = 1) +
  scale_x_continuous(NULL, labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(subtitle = "Spanish L1") +  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") + 
  ggtitle("Overall Probability of choosing each word") + xlim(0,.5) +
  geom_vline(xintercept = 1/7, 
             linetype = "dashed", alpha = .7) + 
  ylab("Word") + xlab("Probability") + 
  ggsave(here("data", "plots", "spl1_base_prob_bar.png"), dpi=300)

df %>%  
  ggplot(aes(y = Estimate, x = reorder(level, -Estimate), 
             fill = language)) + 
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), shape = 21) +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  scale_y_continuous(NULL, labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05)), 
                     limits = c(0, NA))  +
  labs(subtitle = "Spanish L1") +  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") + 
  ggtitle("Overall Probability of choosing each word") + 
  ylim(0,.5) +
  geom_hline(yintercept = 1/7, 
             linetype = "dashed", alpha = .7) + 
  ylab("Probability") + xlab("Word") +
  ggsave(here("data", "plots", "spl1_base_prob_point.png"), dpi=300)


df_eng %>%  
  ggplot(aes(y = Estimate, x = reorder(level, -Estimate), 
             fill = language)) + 
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), shape = 21) +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  scale_y_continuous(NULL, labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05)), 
                     limits = c(0, NA))  +
  labs(subtitle = "English L1") +  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") + 
  ggtitle("Overall Probability of choosing each word") + 
  ylim(0,.5) +
  geom_hline(yintercept = 1/7, 
             linetype = "dashed", alpha = .7) + 
  ylab("Probability") + xlab("Word") +
  ggsave(here("data", "plots", "engl1_base_prob_point.png"), dpi=300)



comb_df %>%  
  ggplot(aes(y = Estimate, x = reorder(level, -Estimate), 
             fill = language)) + 
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), shape = 21) +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  scale_y_continuous(NULL, labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05)), 
                     limits = c(0, NA)) +  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") + 
  ggtitle("Overall Probability of choosing each word") + 
  ylim(0,.5) +
  geom_hline(yintercept = 1/7, 
             linetype = "dashed", alpha = .7) + 
  ylab("Probability") + xlab("Word") + facet_wrap(~group) +
  ggsave(here("data", "plots", "facet_base_prob_point.png"), dpi=300)


desc_df = pct_tidy %>% 
  group_by(L1, language_chosen, phoneme, stim_language) %>% 
  summarize(n = n())

pe = desc_df %>% 
  filter(L1 == "English") %>% 
  ggplot(aes(x = language_chosen, y = n, 
             fill = language_chosen)) +  
  geom_col(color = "black") +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  facet_grid(stim_language~phoneme) + 
  ggtitle("English L1 Raw categorization data")

ps = desc_df %>% 
  filter(L1 == "Spanish") %>% 
  ggplot(aes(x = language_chosen, y = n, 
             fill = language_chosen)) +  
  geom_col(color = "black") +
  scale_fill_manual(values=c("#0dbaff", "#fa6652")) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  facet_grid(stim_language~phoneme) + 
  ggtitle("Spanish L1 Raw categorization data")

plot = ggpubr::ggarrange(pe, ps)

plot %>% 
  ggsave(here("data", "plots", "raw_data.png"))

log_1_e = read_rds(here("data", 
                        "models", 
                        "logistic_regression_eng.rds"))

log_1_s = read_rds(here("data", 
                        "models", 
                        "logistic_regression_span.rds"))


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
  facet_wrap(~L1) + ggsave(here("data", "plots", "logistic_phoneme.png"))


