

library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)
library(tsibble)
library(readxl)
library(cowplot)



getwd()
setwd("/Users/PDG/Dropbox/앱/data_visual/LAST/project")


# 연령별 출산율
#출처: 통계청, 「인구동향조사」
# [단위 : 해당 연령 여자 인구 1천명당 명]

library(readxl)


age_birthrate <- read_excel("H000201_20240523133739046_excel.xlsx")

data2 = age_birthrate[2:9,]
colnames(data2) <- as.character(unlist(data2[1,]))
data2 <- data2[-1, ]
data_melt2 <- data2 %>%
  reshape2::melt(id.vars = NA, 
                 variable.name = "연도",
                 value.name = "출산율")
colnames(data_melt2) <- c("연령대","연도", "출산율")
data_melt2


data_melt2$연도 <- as.numeric(as.character(data_melt2$연도))
data_melt2$출산율= as.numeric(data_melt2$출산율)
data_melt2


my_palette <- c("gray", "orange", "green4", "red", "blue", "gray0", "gray80")

p2 = 
  ggplot(data_melt2, aes(x = 연도, y = 출산율, colour = 연령대)) +  # fill -> color로 변경
  geom_line() +
  geom_point() +  # 데이터 포인트 표시
  geom_label(aes(label = sprintf("%.1f", 출산율)), size = 4, vjust = -0.5, label.padding = unit(0.15, "lines"), fill = "white", family = "나눔고딕") +  # 데이터 포인트 값 표시
  theme_minimal() +
  theme(
    legend.position = 'right',
    legend.key.size = unit(0.6, "cm"),
    legend.spacing.y = unit(0.9, "cm"),
    text = element_text(family = "나눔고딕", size = 10), 
    plot.title = element_text(size = 18),  # 연도 글씨 크기
    legend.text = element_text(size = 11, color = "black", family = "나눔고딕"),
    axis.title = element_text(size = 14),  # xlab, ylab 글씨 크기
    plot.caption = element_text(size = 8, hjust = 1.1) 
  ) +
  scale_color_manual(values = my_palette) + 
  transition_reveal(연도) +
  labs(legend.position = 'bottom',
    title = '대한민국 연령별출산율 변화: {round(frame_along)}년', 
    x = "연도", 
    y = "출산율", 
    caption = "  
                \n 출처: 통계청, 「인구동향조사」
                \n 단위 : 해당 연령 여자 인구 1천명당 명"
  )



# 저장 
animate(p2, duration = 15, fps = 10, width = 800, height = 400, renderer = gifski_renderer("p2.gif"))








