library(tidyverse)
library(faux)

name_dv = "time"


prog = c("ST","GR","BO","CO")
phase = c("Pre","Post")
version = c("Alt","Pat")
tmp <- interaction(prog,phase,version,sep=".")
prog_phase_version <- levels(tmp)

# From Table 5 the "best guess" SD and means
collapsed_stats <- data.frame(
  prog = c(rep("BO", 4), rep("CO", 4), rep("GR", 4), rep("ST", 4)),
  phase = rep(c("Pre", "Post"), times = 4 * 2),
  version = rep(c("Alt", "Pat"), each = 2, times = 4),
  mean = c(
    71.0, 50.0, 67.0, 61.5,     # BO
    39.0, 44.0, 49.0, 26.5,     # CO
    64.0, 73.0, 53.0, 75.5,     # GR
    22.5, 38.0, 29.0, 14.5      # ST
  ),
  sd = c(
    38.15, 34.85, 27.80, 54.85,  # BO
    22.55, 22.20, 24.45, 14.45,  # CO
    44.45, 44.10, 21.45, 25.95,  # GR
    17.40, 28.15, 24.45, 12.95   # ST
  )
)

print(collapsed_stats)

df <- faux::sim_design(
  between = list(
    "prog_phase_version" = prog_phase_version
  ),
  mu =  c(1),
  sd = c(1),
  n = 10,
  dv = "time",
  long = T
)

df <- df %>% separate(prog_phase_version, into = c("prog", "phase", "version"), sep = "\\.")

dim(df)

df %>% filter(prog == "BO")