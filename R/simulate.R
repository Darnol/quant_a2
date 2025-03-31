library(tidyverse)
library(faux)

# From Table 5 I extracted the mean and SD as IQR/1.35.
# It is important to match the correct tuple to the correct prog_task_version_phase, therefore I match by name manually
df_cond = data.frame(
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

# Because I wanted to make sure that the 32 mean and sd entries match the correct group, I decided to create a cond vector and pass that one into sim_design.
# This way I'm sure that the means and sd match the correct group

# Create a single vector with the different conditions
cond_vector <- df_cond %>% unite(cond, prog, task, phase, version, sep = ".") %>% pull(cond)

df <- faux::sim_design(
  between = list(
    "prog_task_phase_version" = cond_vector
  ),
  mu =  df_cond$mean,
  sd = df_cond$sd,
  n = 10,
  dv = "time",
  long = T,
  empirical = T
)

# Now add the group according to Figure 1
matching_groups <- data.frame(
  "group" = rep(c("A","B","C","D"), each = 8),
  "prog_task_phase_version" = c(
    
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

df <- merge(matching_groups, df)

# Separate the artificial column again
df <- df %>% separate(prog_task_phase_version, into = c("prog", "task", "phase", "version"), sep = "\\.")

# Drop the id column, not relevant for further analysis
df <- df %>% select(-id)

dim(df) # Should be 320 x 6

# Reorder columns
df <- df %>% relocate(group, prog, task, phase, version, time)

# Manually inspect some means and sd to make sure nothing got mixed up
print(n=100, df %>% filter(prog == "BO") %>% group_by(prog, task, phase, version) %>% summarise(mean(time), sd(time)))
print(n=100, df %>% filter(prog == "GR") %>% group_by(prog, task, phase, version) %>% summarise(mean(time), sd(time)))
