library(tidyverse)
library(flextable)
library(patchwork)

theme_set(theme_bw())

jcolor <- c("#5a2158","#8d8c8cff", "#EF0029",  "#ee5497ff", "#f56871ff", "#ee8757", "#febe57", "#b9f971ff")


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 6,
  background.color = "#F8F5F5")