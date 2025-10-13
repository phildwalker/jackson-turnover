library(tidyverse)
library(flextable)
library(patchwork)

theme_set(theme_bw())

jcolor <- c("#5a2158", "#EF0029", "#983560", "#cb575e", "#ee8757", "#febe57", "#f9f871")


set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 6,
  background.color = "#F8F5F5")