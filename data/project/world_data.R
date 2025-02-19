library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)
library(tsibble)
library(readxl)
library(cowplot)




raw.data_1950_2000 <- read_excel("Dropbox/앱/data_visual/LAST/project/05k5-1.xlsx")
raw.data_1950_2000
raw.data_1950_2000 %>% dim()

raw.data.using01 = raw.data_1950_2000[8:90,2:6]
raw.data.using01 = raw.data.using01[,-2]

colnames(raw.data.using01) <- c("연도", "총인구", "남자", "여자")

raw.data.using = 
  raw.data.using01 %>% slice(-c(1, 2))

raw.data.using






raw.data_2000_2015 <- read_excel("Dropbox/앱/data_visual/LAST/project/05k5-10-1.xlsx")


raw.data.using02 = raw.data_2000_2015[7:10,5:50]

raw.data.using02 = raw.data.using02[3:4,]
raw.data.using02
