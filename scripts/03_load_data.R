# A script to load data - it is typically run by other scripts 


pct_tidy = read.csv(here::here("data", "tidy", "pct_tidy.csv"))

pct_tidy_mono = read.csv(here("data", "tidy", "tidy_mono_groups.csv"))

pct_eng_mono = pct_tidy_mono %>% 
  filter(L1 == "English_mono")

length(unique(pct_eng_mono$participant))

pct_span_mono = pct_tidy_mono %>% 
  filter(L1 == "Spanish_mono")

length(unique(pct_span_mono$participant))


cond_df_prob = read.csv(here("data", "tidy", "cond_prob_df.csv"))
