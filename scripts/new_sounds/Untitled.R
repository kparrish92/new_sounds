
# Spanish raw data 
span_desc_plot = rbind(desc_df_span, desc_df_span_g)  %>% 
  ggplot(aes(x = choice, y = percentage, 
                          fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text.x = element_text(color="black",
                                   size=7, angle=25)) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  ylim(0, 1) +
  facet_grid(stim~phoneme) +
  theme(legend.text=element_text(size=8)) +
  ggsave(here("MDPI_template", "figs", "span_desc_plot.png"),
         dpi = 1200)

## Descriptive tables of spanish 

### French
span_fr = desc_df_span %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()
### German 

span_ger = desc_df_span_g %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

# English raw data 
eng_desc_plot = rbind(desc_df_eng, desc_df_eng_g)  %>% 
  ggplot(aes(x = choice, y = percentage, 
             fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text.x = element_text(color="black",
                                   size=7, angle=25)) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  ylim(0, 1) +
  facet_grid(stim~phoneme) +
  theme(legend.text=element_text(size=8)) +
  ggsave(here("MDPI_template", "figs", "eng_desc_plot.png"),
         dpi = 1200)


# Descriptive tables of English 

## French 

eng_fr = desc_df_eng %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

## German 

eng_ger = desc_df_eng_g %>% 
  mutate(percentage = round(percentage, digits = 2)) %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

