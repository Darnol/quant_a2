library(tidyverse)
library(faux)

prog = c("ST","GR","BO","CO")
task = c("Task1","Task2")
phase = c("Pre","Post")
version = c("Alt","Pat")
tmp <- interaction(prog,task,phase,version,sep=".")
prog_task_phase_version <- levels(tmp)

# From Table 5 I extracted the mean and SD as IQR/1.35
means_sds <- data.frame(
  prog = c(rep("BO", 8), rep("CO", 8), rep("GR", 8), rep("ST", 8)),
  task = rep(c(1, 1, 2, 2), times = 4),
  phase = rep(c("Pre", "Post"), each = 2, times = 4),
  version = rep(c("Alt", "Pat"), each = 4, times = 4),
  mean = c(
    129, 86, 13, 14,    # BO Alt
    108, 99, 26, 24,    # BO Pat
    63, 69, 15, 19,     # CO Alt
    65, 33, 33, 20,     # CO Pat
    91, 105, 37, 41,    # GR Alt
    78, 127, 28, 24,    # GR Pat
    14, 26, 31, 50,     # ST Alt
    22, 14, 36, 15      # ST Pat
  ),
  sd = c(
    (175-88)/1.35, (130-45)/1.35, (20-4)/1.35, (17-8)/1.35,
    (135-86)/1.35, (160-55)/1.35, (39-13)/1.35, (46-3)/1.35,
    (85-35)/1.35, (90-48)/1.35, (20-9)/1.35, (27-9)/1.35,
    (84-45)/1.35, (43-22)/1.35, (45-18)/1.35, (29-11)/1.35,
    (136-51)/1.35, (155-57)/1.35, (51-16)/1.35, (43-22)/1.35,
    (89-59)/1.35, (145-104)/1.35, (42-14)/1.35, (38-9)/1.35,
    (21-7)/1.35, (35-13)/1.35, (48-15)/1.35, (77-23)/1.35,
    (27-3)/1.35, (26-6)/1.35, (56-14)/1.35, (22-7)/1.35
  )
)

df <- faux::sim_design(
  between = list(
    "prog_task_phase_version" = prog_task_phase_version
  ),
  mu =  means_sds$mean,
  sd = means_sds$sd,
  n = 10,
  dv = "time",
  long = T
)

df <- df %>% separate(prog_task_phase_version, into = c("prog", "task", "phase", "version"), sep = "\\.")

dim(df)

df %>% filter(prog == "BO")