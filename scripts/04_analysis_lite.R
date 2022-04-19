source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "03_load_data.R"))

# Set priors
prior = c(prior(normal(0, 8), class = Intercept, dpar = mufin),
          prior(normal(0, 8), class = Intercept, dpar = mufool),
          prior(normal(0, 8), class = Intercept, dpar = mufought),
          prior(normal(0, 8), class = Intercept, dpar = mufun),
          prior(normal(0, 8), class = Intercept, dpar = muson),
          prior(normal(0, 8), class = Intercept, dpar = musu))


# Spanish bilingual model
intercept_only <- 
  brm(formula = choice ~ 1 +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "Spanish"),
      file = here("data", "models", "intercept_only.rds"))

# English bilingual model
intercept_only_eng <- 
  brm(formula = choice ~ 1 +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "English"),
      file = here("data", "models", "intercept_only_eng.rds"))

# Create fitted objects pre-tidying 
f = fitted(intercept_only)
eng = fitted(intercept_only_eng)

# tidy object for plotting Spanish
df = f[1, , ] %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column("level") %>% 
  mutate(level = str_replace_all(string = level,
                                 pattern = "[\\)\\(\\)]",
                                 replacement = "")) %>% 
  mutate(level = str_remove(level, "PY = ")) %>% 
  mutate(language = case_when(
    level == "feel"  ~ "English",
    level == "fin"  ~ "Spanish",
    level == "fool"  ~ "English",
    level == "fought"  ~ "English",
    level == "fun"  ~ "English",
    level == "son"  ~ "Spanish",
    level == "su"  ~ "Spanish"))


# tidy object for plotting English
df_eng = eng[1, , ] %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column("level") %>% 
  mutate(level = str_replace_all(string = level,
                                 pattern = "[\\)\\(\\)]",
                                 replacement = "")) %>% 
  mutate(level = str_remove(level, "PY = ")) %>% 
  mutate(language = case_when(
    level == "feel"  ~ "English",
    level == "fin"  ~ "Spanish",
    level == "fool"  ~ "English",
    level == "fought"  ~ "English",
    level == "fun"  ~ "English",
    level == "son"  ~ "Spanish",
    level == "su"  ~ "Spanish"))


#### tidy intercept only model 

df %>% 
  write.csv(here("data", "tidy", "int_only_plot_data.csv"))

df_eng %>% 
  write.csv(here("data", "tidy", "int_only_plot_data_eng.csv"))

#########

# 2719
int_mod_sp <- 
  brm(formula = choice ~ phoneme*stim_language +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "Spanish"),
      file = here("data", "models", "sp_l1_int.rds"))

all_mod_sp <- 
  brm(formula = choice ~ phoneme*stim_language*frame +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "Spanish"),
      file = here("data", "models", "sp_l1_full_int.rds"))

three_int_mod_sp <- 
  brm(formula = choice ~ phoneme*stim_language*frame +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "Spanish"),
      file = here("data", "models", "sp_l1_int.rds"))

full_mod_sp <- 
  brm(formula = choice ~ phoneme +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "Spanish"),
      file = here("data", "models", "sp_l1_full.rds"))



############# logsitic regression models 

log_1_e <- 
  brm(formula = language_chosen ~ phoneme*stim_language +
        (1 | participant),, 
      warmup = 1000, iter = 4000, chains = 4, 
      family = bernoulli(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "English"),
      file = here("data", "models", "logistic_regression_eng.rds"))

log_1_s <- 
  brm(formula = language_chosen ~ phoneme*stim_language +
        (1 | participant),, 
      warmup = 1000, iter = 4000, chains = 4, 
      family = bernoulli(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy %>% filter(L1 == "Spanish"),
      file = here("data", "models", "logistic_regression_span.rds"))


summary(log_1_e)