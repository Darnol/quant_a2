library(tidyverse)
library(lme4)
library(lmerTest)
import::from(multcomp, glht, mcp, adjusted)
import::from(broom.mixed, tidy)
import::from(emmeans, emmeans, contrast, as.glht)
import::from(afex, lmer_alt)


# Load the simulated DF
df <- read.csv("data/data1.csv")

# Convert to factors for modelling
# For phase, set Pre as the baseline (1. level)
df <- df %>% mutate(
  subject_id = factor(subject_id),
  group = factor(group),
  prog = factor(prog),
  task = factor(task),
  phase = factor(phase, levels = c("Pre","Post")),
  version = factor(version)
)
str(df)




#===============================================================================
# Plot the data
#===============================================================================

# Omit the dimension "task" for the plot
df %>% 
  ggplot(aes(x = version, y =time)) +
  geom_violin(fill = "grey") +
  geom_point(shape = 1) +
  labs(x = NULL, y = "time") +
  facet_grid(phase ~ prog)

# At the first glance, it looks like Post had an effect on time in the two cases CO and ST.
# This is in line with what the paper found

# Plot version only
df %>% 
  ggplot(aes(x = version, y = time)) +
  geom_violin(fill = "grey", alpha = 0.5) +
  geom_point(stat = "summary", fun = mean, color = "red", size = 6) +
  geom_point()
# The version alone does not yield any visually describable effect

# Plot prog only
df %>% 
  ggplot(aes(x = prog, y = time)) +
  geom_violin(fill = "grey", alpha = 0.5) +
  geom_point(stat = "summary", fun = mean, color = "red", size = 6) +
  geom_point()
# Here we see that CO and ST are rather different in their distribution

# Plot phase only
df %>% 
  ggplot(aes(x = phase, y = time)) +
  geom_violin(fill = "grey", alpha = 0.5) +
  geom_point(stat = "summary", fun = mean, color = "red", size = 6) +
  geom_point()
# This is also not notable distinct for the two levels



# The overall question I want to answer: Is time influenced by the variable version? And if so, how does it depend on the other two IVs prog and phase?



#===============================================================================
# Analyse simple and increasingly complex models
#===============================================================================

# In a first step, I try to look at some very simple models with one IV only and build my way up

###
# Some very simple models with 1 IV
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
# Expand to 2 IVs, using prog and version
###

# What I want to explore: version and prog. Does the version have an impact on time if we account for the different prog?
# Summary: When I add combine version and prog, it is prog which explains all the variance. version is still not significant

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
# From the plots, I saw that Pat seems to play a role in CO and ST, unfortunately this model cannot capture this

anova(m_vers_prog_inter)
# All F-statistics on the models are highly significant, but the anova test tells me that the interaction does not explain variability in time
# The explained variance comes from the prog variable alone, which is what we saw in the two 1 IV models before



###
# Expand to 3 IVs
###

# What I want to explore: What if I add all three variables and account for interactions?
# Summary: The models get overly complex and very hard to interpret, especially if we consider the interaction effects

m_vers_prog_phase = lm(time ~ version + prog + phase, data=df)
summary(m_vers_prog_phase)
tibble(model_name = "m_vers_prog_phase", model = list(m_vers_prog_phase)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
anova(m_vers_prog_phase)
# Without interaction, we see the same significant effects, phase does not change from m_phase

# What if we consider interactions between version and phase, but not prog
m_prog_vers_x_phase = lm(time ~ prog + version * phase, data = df)
summary(m_prog_vers_x_phase)
tidy(m_prog_vers_x_phase, conf.int=T)
anova(m_prog_vers_x_phase)

# With interactions
m_vers_prog_phase_inter = lm(time ~ version * prog * phase, data=df)
summary(m_vers_prog_phase_inter)
# tibble(model_name = "m_vers_prog_phase_inter", model = list(m_vers_prog_phase_inter)) %>% mutate(tidy_lm = map(model, ~tidy(.x, conf.int=T))) %>% unnest(tidy_lm)
tidy(m_vers_prog_phase_inter, conf.int = T)
anova(m_vers_prog_phase_inter)
# Also the anova test tells us that the variability is explained by the prog variable alone and the interaction does not help explain the variability


# The problem with this approach is that we have way too many coefficients now to interpret

# At this point, I figured I should try a mixed model to account for the individual subjects effects
mm_rand_intercept <- lme4::lmer(time ~ version * prog * phase + (1|subject_id), data = df)
summary(mm_rand_intercept)
coef(mm_rand_intercept)
tidy(mm_rand_intercept, conf.int=T)

# Try random slopes for the main effects -> SINGULARITY ISSUES
mm_rand_intercept <- lme4::lmer(time ~ version * prog * phase + (1 + prog | subject_id), data = df) # ERROR singular

# Try #15 from slides
mm_rand_intercept <- lmer_alt(time ~ version * prog * phase + (1 + prog || subject_id), data = df) # ERROR singluar

# Unfortunately, #13 and #16 are not applicable ...





#===============================================================================
# Different approach: Analyze each program separately instead of keeping this variable
#===============================================================================

df_bo <- df %>% filter(prog == "BO")
df_co <- df %>% filter(prog == "CO")
df_gr <- df %>% filter(prog == "GR")
df_st <- df %>% filter(prog == "ST")

plot_prog <- function(prog_name, data) {
  data %>% 
    ggplot(aes(x=version, y=time)) +
    geom_violin(fill = "grey", alpha = 0.5) +
    geom_point(shape = 1) +
    geom_point(stat = "summary", fun = mean, color = "red", size = 6) +
    labs(x = NULL, y = "time") +
    facet_grid( ~ phase) +
    ggtitle(prog_name)
}



###
# BO
# Summary: version does not influence time in the prog = BO case. The models do not fit well

plot_prog("BO", df_bo)

df_to_analyze <- df_bo

# time ~ version -- Not a good fit, fails the F-test
m_vers <- lm(time ~ version, data = df_to_analyze)
summary(m_vers)

# time ~ version * phase -- Still not a good fit, this confirms what we saw visually, for prog BO, the variables version and prog do not have an impact
m_vers_phase <- lm(time ~ version * phase, data = df_to_analyze)
summary(m_vers_phase)


###
# GR
# Summary: version does not influence time in the prog = GR case. The models do not fit well

plot_prog("CO", df_co)

df_to_analyze <- df_gr

# time ~ version -- Not a good fit, fails the F-test
m_vers <- lm(time ~ version, data = df_to_analyze)
summary(m_vers)
tidy(m_vers, conf.int = T)

# time ~ version * phase -- Still not a good fit, this confirms what we saw visually, for prog BO, the variables version and prog do not have an impact
m_vers_phase <- lm(time ~ version * phase, data = df_to_analyze)
summary(m_vers_phase)
tidy(m_vers_phase, conf.int = T)
anova(m_vers_phase)


###
# CO

plot_prog("CO", df_co)

df_to_analyze <- df_co

# time ~ version -- This simple model does already provide a reasonable fit
m_vers <- lm(time ~ version, data = df_to_analyze)
summary(m_vers)
tidy(m_vers, conf.int = T)

# time ~ version * phase -- The fit is still good
m_vers_phase <- lm(time ~ version * phase, data = df_to_analyze)
summary(m_vers_phase)
tidy(m_vers_phase, conf.int = T)
anova(m_vers_phase)


# Here I tried to fit a mixed effects model -> Singularity issue I cannot solve
mm <- lme4::lmer(time ~ version * phase + (1 + version + phase|subject_id), data = df_to_analyze)
print(mm)

# Post-hoc main effects on the interaction model without random effects
emm_interaction = emmeans(m_vers_phase, ~ version * phase)
contr_cond = contrast(emm_interaction, "pairwise", simple = "each", combine = TRUE, adjust = "holm")
contr_cond
plot(contr_cond)

# -> What I'm interested in the contrasts Pre.Alt-Pat and Post.Alt-Pat
# Pre.Alt-Pat -> The p-value suggests that this effect is not significant
# Post.Alt-Pat -> Here we have a significant value which corroborates our hypothesis: In the Post phase, the Alt group had higher times than the Pat group




###
# ST

plot_prog("ST", df_st)

df_to_analyze <- df_st

# The fit of the simple model does not yet provide a very good fit with an F-test p-value of 0.17
m_vers <- lm(time ~ version, data = df_to_analyze)
summary(m_vers)
tidy(m_vers, conf.int = T)

# The fit is good now given by the F-test
m_vers_phase <- lm(time ~ version * phase, data = df_to_analyze)
summary(m_vers_phase)
tidy(m_vers_phase, conf.int = T)
anova(m_vers_phase)
# -> We see that the interaction effect seems to explain the variance in time

# I run into singularity problems again and try to keep on going with the non mixed-effect model
mm <- lme4::lmer(time ~ version * phase + (1 + version + phase|subject_id), data = df_to_analyze)
print(mm)

# Post-hoc main effects on the interaction model without random effects
emm_interaction = emmeans(m_vers_phase, ~ version * phase)
contr_cond = contrast(emm_interaction, "pairwise", simple = "each", combine = TRUE, adjust = "holm")
contr_cond
plot(contr_cond)

# -> Again, we look at the two contrasts Pre.Alt-Pat and Post.Alt-Pat
# Pre.Alt-Pat -> The p-value suggests that this effect is not significant
# Post.Alt-Pat -> Here we have a significant value which corroborates our hypothesis: In the Post phase, the Alt group had higher times than the Pat group
