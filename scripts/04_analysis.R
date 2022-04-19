# Source libs ------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "03_load_data.R"))

# ------------------------------------------------------

l2_response_priors <- c(
  prior(normal(0, 8), class = b, coef = phonemeo, 
        dpar = mufin),
  prior(normal(0, 8), class = b, coef = phonemeschwa, 
        dpar = mufin),
  prior(normal(0, 8), class = b, coef = phonemey, 
        dpar = mufin))


unique(pct_tidy$choice)

brm (choice ~ phoneme, 
     data=pct_tidy,
     family="categorical",
     prior=l2_response_priors)

get_prior(choice ~ phoneme + (1 | participant), 
     data=pct_tidy,
     family="categorical")

b2 <- brm (choice ~ L1*phoneme*stim_language* 
             frame + (1 | participant), 
           data=pct_tidy,
           family="categorical",
           prior=l2_response_priors)

b2 <- brm (choice ~ L1*phoneme*stim_language* 
             frame + (1 | participant), 
           data=pct_tidy,
           family="categorical",
           prior=c(set_prior ("normal (0, 8)"))) 


summary(b2)
            

desc = pct_tidy %>% 
  group_by(phoneme, choice, L1, frame, stim_language) %>% 
  summarize(n = n())


desc %>% 
  ggplot(aes(x = choice, y = n, fill = L1)) +  
  geom_bar(stat = "identity") 

desc %>% 
  ggplot(aes(x = choice, y = n, fill = L1)) +  
  geom_col() + facet_wrap(~phoneme)
    
desc %>% 
  filter(L1 == "Spanish") %>% 
  ggplot(aes(x = choice, y = n, fill = frame)) +  
  geom_col() + facet_wrap(~phoneme)

a = desc %>% 
  filter(L1 == "English") %>% 
  ggplot(aes(x = choice, y = n, fill = stim_language)) +  
  geom_col() + facet_wrap(~phoneme)

b = desc %>% 
  filter(L1 == "Spanish") %>% 
  ggplot(aes(x = choice, y = n, fill = stim_language)) +  
  geom_col() + facet_wrap(~phoneme)

ggpubr::ggarrange(a, b)



 


desc %>% 
  filter(phoneme == "o")

desc %>% 
  filter(phoneme == "y")

desc %>% 
  filter(phoneme == "schwa")


null = glmer(language_choice_num ~ 1 + (1 | participant), 
      family = "binomial",
      data = pct_tidy)


group = glmer(language_choice_num ~ L1 + (1 | participant), 
             family = "binomial",
             data = pct_tidy)

phoneme = glmer(language_choice_num ~ L1 + phoneme + (1 | participant), 
              family = "binomial",
              data = pct_tidy)

stim = glmer(language_choice_num ~ L1 + phoneme + stim_language + (1 | participant), 
              family = "binomial",
              data = pct_tidy)

frame = glmer(language_choice_num ~ L1 + phoneme + stim_language + frame +
               (1 | participant), 
             family = "binomial",
             data = pct_tidy)

int = glmer(language_choice_num ~ L1*phoneme*stim_language*frame + 
              (1 | participant), 
              family = "binomial",
              data = pct_tidy)


anova(null, group, phoneme, stim, frame, int)

summary(int)

df = data.frame(fixef(int))

df = data.frame(fixef(int))