library(tidyverse)
library(faux)

N = 10
set.seed(100)

# From Table 5 I extracted the mean and SD as IQR/1.35.
# It is important to match the correct tuple to the correct prog_task_version_phase, therefore I match by name manually
df_mean_sd = data.frame(
  prog = rep(c("BO","CO","GR","ST"), each=8),
  task = rep(c("Task1","Task2"), each=2, times=8),
  phase = rep(c("Pre","Post"), times=16),
  version = rep(c("Alt","Pat"), each = 4, times =4),
  mean = c(
    129, 86, 13, 14, 108, 99, 26, 24,
    63, 69, 15, 19, 65, 33, 33, 20,
    91, 105, 37, 41, 78, 127, 28, 24,
    14, 26, 31, 50, 22, 14, 36, 15
  ),
  sd = c(
    (175 - 88)/1.35, (130 - 45)/1.35, (20 - 4)/1.35, (17 - 8)/1.35,
    (135 - 86)/1.35, (160 - 55)/1.35, (39 - 13)/1.35, (46 - 3)/1.35,
    (85 - 35)/1.35, (90 - 48)/1.35, (20 - 9)/1.35, (27 - 9)/1.35,
    (84 - 45)/1.35, (43 - 22)/1.35, (45 - 18)/1.35, (29 - 11)/1.35,
    (136 - 51)/1.35, (155 - 57)/1.35, (51 - 16)/1.35, (43 - 22)/1.35,
    (89 - 59)/1.35, (145 - 104)/1.35, (42 - 14)/1.35, (38 - 9)/1.35,
    (21 - 7)/1.35, (35 - 13)/1.35, (48 - 15)/1.35, (77 - 23)/1.35,
    (27 - 3)/1.35, (26 - 6)/1.35, (56 - 14)/1.35, (22 - 7)/1.35
  )
)
# Add the column cond
df_mean_sd <- df_mean_sd %>% unite("cond", prog, task, phase, version, sep = ".", remove = F)

# Now create the matching of which cond belongs to which group
df_groups <- data.frame(
  "group" = rep(c("A","B","C","D"), each = 8),
  "cond" = c(
    
    # A
    "ST.Task1.Pre.Pat",
    "ST.Task2.Pre.Pat",
    "GR.Task1.Pre.Alt",
    "GR.Task2.Pre.Alt",
    "CO.Task1.Post.Alt",
    "CO.Task2.Post.Alt",
    "BO.Task1.Post.Pat",
    "BO.Task2.Post.Pat",
    
    # B
    "GR.Task1.Pre.Pat",
    "GR.Task2.Pre.Pat",
    "ST.Task1.Pre.Alt",
    "ST.Task2.Pre.Alt",
    "BO.Task1.Post.Alt",
    "BO.Task2.Post.Alt",
    "CO.Task1.Post.Pat",
    "CO.Task2.Post.Pat",
    
    # C
    "CO.Task1.Pre.Alt",
    "CO.Task2.Pre.Alt",
    "BO.Task1.Pre.Pat",
    "BO.Task2.Pre.Pat",
    "ST.Task2.Post.Pat",
    "ST.Task1.Post.Pat",
    "GR.Task1.Post.Alt",
    "GR.Task2.Post.Alt",
    
    # D
    "BO.Task1.Pre.Alt",
    "BO.Task2.Pre.Alt",
    "CO.Task1.Pre.Pat",
    "CO.Task2.Pre.Pat",
    "GR.Task1.Post.Pat",
    "GR.Task2.Post.Pat",
    "ST.Task2.Post.Alt",
    "ST.Task1.Post.Alt"
  )
)
# Add seperate columns
df_groups <- df_groups %>% separate(cond, into = c("prog", "task", "phase", "version"), sep = "\\.", remove = F)

# Because I wanted to make sure that the 32 mean and sd entries match the correct group, I decided to match on the cond column
# This way I'm sure that the means and sd match the correct group
df_cond <- merge(df_mean_sd, df_groups)

df_subjects <- data.frame(
  id = 1:(4*N),
  group = rep(c("A", "B", "C", "D"), each = N)
)


df <- faux::sim_design(
  between = list(
    cond = df_cond$cond
  ),
  mu = df_cond$mean,
  sd = df_cond$sd,
  n = N,
  dv = "time",
  long=T,
  empirical=F
)

# Split cond
df <- df %>% separate(cond, into = c("prog", "task", "phase", "version"), sep = "\\.", remove = T)

# Drop the false ID column
df <- df %>% select(-id)

# Add the group
df <- merge(df, df_groups)

# Add the N user IDs, each group has 8 conditions. So each user should have 8 rows
df <- df %>% arrange(group, cond) %>% mutate(subject_id = rep(c(1:N), 32))

# Rearrange columns
df <- df %>% relocate(subject_id, group, prog, task, phase, version, time)

# Drop cond
df <- df %>% select(-cond)

# Manually inspect some means and sd to make sure nothing got mixed up
df %>% filter(prog == "GR") %>%
  group_by(prog, task, phase, version) %>%
  summarise(mean(time), sd(time)) %>%
  arrange(prog, version, task, desc(phase))

write.csv(df, "data/data1.csv", row.names=F)


# ###
# # MANUAL APPROACH
# # Pro: Easier to understand than faux imo for this specific design
# # Contra: The rnorm will not guarantee an empirical mean and sd, because we only draw 10 samples each
# 
# # At this point, I tried to make faux work with what I wanted (10 subjects per group, each subject is exposed to 8 unique conditions, each of the 32 conditions
# # has its own mean and sd). But I failed, so I figured I can also produce the simulated data by hand
# sim_data = list()
# ct <- 1
# for (s_id in 1:nrow(df_subjects)) {
#   s_group = df_subjects %>% filter(id == s_id) %>% pull(group)
#   s_conds = df_cond %>% filter(group == s_group) %>% pull(cond)
#   
#   for (s_cond in s_conds) {
#     s_mean_sd <- df_mean_sd %>% filter(cond == s_cond)
#     
#     # Draw from normal distribution
#     s_time = rnorm(n = 1, s_mean_sd$mean, s_mean_sd$sd)
#     
#     # Add row
#     sim_data[[ct]] <- data.frame(
#       id = s_id,
#       group = s_group,
#       prog = s_mean_sd$prog,
#       task = s_mean_sd$task,
#       phase = s_mean_sd$phase,
#       version = s_mean_sd$version,
#       time = s_time
#     )
#     ct <- ct + 1
#   }
# }
# 
# df <- do.call(rbind, sim_data)
# 
# dim(df)
# head(df)
# 
# # Manually inspect some means and sd to make sure nothing got mixed up
# df %>% filter(prog == "BO") %>% group_by(prog, task, phase, version) %>% summarise(mean(time), sd(time))
# df %>% filter(prog == "GR") %>%
#   group_by(prog, task, phase, version) %>%
#   summarise(mean(time), sd(time)) %>% 
#   arrange(prog, version, task, desc(phase))
# 





