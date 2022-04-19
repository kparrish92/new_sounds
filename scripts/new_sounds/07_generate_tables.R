
view(spanish_l1_pct)


desc_df_eng %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

print(c)

desc_df_span %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()