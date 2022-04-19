# load data 
source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "03_load_data.R"))

span_blp = read.csv(here("data", "tidy", "span_l1_blp.csv"))
eng_blp = read.csv(here("data", "tidy", "eng_l1_blp.csv"))

removed = read.csv(here("data", "tidy", "removed.csv"))

english_l1_pct = pct_tidy %>% 
  filter(L1 == "English") %>% 
  filter(participant %in% eng_blp$prolific_id)

english_l1_pct_g = pct_tidy %>% 
  filter(L1 == "English") %>% 
  filter(stim_language == "German") %>% 
  filter(participant %in% eng_blp$prolific_id)

spanish_l1_pct = pct_tidy %>% 
#  filter(L1 == "Spanish") %>% 
  filter(stim_language == "french") %>% 
  filter(participant %in% span_blp$prolific_id)

spanish_l1_pct_g = pct_tidy %>% 
  filter(L1 == "Spanish") %>% 
  filter(stim_language == "German") %>% 
  filter(participant %in% span_blp$prolific_id)

