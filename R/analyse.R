library(tidyverse)
library(lme4)
import::from(broom.mixed, tidy)
import::from(emmeans, emmeans, contrast, as.glht)


# Load the simulated DF
source("./R/simulate.R")
head(df)

#===============================================================================
# Plot the data
#===============================================================================

# Omit the dimension "task"
df |> 
  ggplot(aes(x = version, y =time)) +
  geom_violin(fill = "grey") +
  geom_point(shape = 1) +
  labs(x = NULL, y = "time") +
  facet_grid(phase ~ prog)

# At the first glance, it looks like Post had an effect on time in the two cases CO and ST


#===============================================================================
# Analyse simple models
#===============================================================================

# In a first step, I try to look at some very simple models

# The overall question I want to answer: Is time influenced by the three IV prog, version and phase?

# Convert to factors for modelling
# For phase, set Pre as the baseline (1. level)
df <- df %>% mutate(
  group = factor(group),
  prog = factor(prog),
  task = factor(task),
  phase = factor(phase, levels = c("Pre","Post")),
  version = factor(version)
)
str(df)


###
# Some very simple models wiht 1 IV
###

m_vers = lm(time ~ version, data = df)
summary(m_vers)
tibble(model_name = "m_vers", model = list(m_vers)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
# The version alone is not deemed significant (F-test not significant)
# This makes sense, since we expect the version to depend on what kind of prog and also in which phase it was solved
# The hypothesis is: If the prog is of a type where design patterns help the developer and the developer had the course on design patterns already (phase), THEN
# the version could have a positive impact on time (lower time)

m_prog = lm(time ~ prog, data = df)
summary(m_prog)
tibble(model_name = "m_prog", model = list(m_prog)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
# prog alone has an impact, which is not surprising. We expect the four problem sets to be rather different in terms of time needed to solve the tasks.
# The two programs CO and ST are significantly lower than the baseline BO. We already see in the plot, that those two programs are solved faster on average than the other two.

m_phase = lm(time ~ phase, data = df)
summary(m_phase)
tibble(model_name = "m_phase", model = list(m_phase)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
# The phase alone does not have a clear effect, which is what we expect since it might differ from prog to prog (F-test not significant)
# e.g. the course on design patterns is not helpful for all 4 kinds of problems and also depends on if you have the Pattern version Pat or the alt version


###
# Expand to 2 IVs
###

# What interests me: version and prog. Does the version have an impact on time if we account for the different prog?

m_vers_prog = lm(time ~ version + prog, data=df)
summary(m_vers_prog)
tibble(model_name = "m_vers_prog", model = list(m_vers_prog)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
# The main effect "versionPat" does not change from the simple model m_vers when we include prog
# prog still contains two significant effects for CO and ST

# Also include the interaction
m_vers_prog_inter = lm(time ~ version * prog, data=df)
summary(m_vers_prog_inter)
tibble(model_name = "m_vers_prog_inter", model = list(m_vers_prog_inter)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
# Here, it would be interesting to see if the version Pat is significant for some programs, which is not the case
# Therefore, I try to include the third IV as well, since the phase could also influence how version affects time given a program

anova(m_vers_prog_inter)
# All F-statistics on the models are highly significant, but the anova test tells me that the interaction does not explain variability in time



###
# Expand to 3 IVs
###

m_vers_prog_phase = lm(time ~ version + prog + phase, data=df)
summary(m_vers_prog_phase)
tibble(model_name = "m_vers_prog_phase", model = list(m_vers_prog_phase)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
# Without interaction, we see the same significant effects, phase does not change from m_phase

# With interactions
m_vers_prog_phase_inter = lm(time ~ version * prog * phase, data=df)
summary(m_vers_prog_phase_inter)
tibble(model_name = "m_vers_prog_phase_inter", model = list(m_vers_prog_phase_inter)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)

# The problem with this approach is that we have way too many coefficients now to interpret

anova(m_vers_prog_phase_inter)
# Also the anova test tells us that the variability is explained by the prog variable alone and the interaction does not help explain the variability



#===============================================================================
# Mixed effects models
#===============================================================================

# Including all the interaction terms of all 3 IVs can quickly get messy and is very hard to interpret


df_bo = df %>% filter(prog == "BO")

m1 <- lm(time ~ version , data = df_bo)
summary(m1)