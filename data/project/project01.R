


library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)
library(tsibble)
library(readxl)
library(cowplot)



getwd()
setwd("/Users/PDG/Dropbox/앱/data_visual/LAST/project")
# 출처: 통계청, 「인구동향조사」
# *자료 : 통계청, 「인구동향조사」 각 연도
# 주석: 1) 합계출산율은 가임기 여성(15-49세) 1명이 가임기간(15-49세) 동안 낳을 것으로 예상되는 평균 출생아수임.
sum_birthrate <- read_excel("/Users/PDG/Dropbox/앱/data_visual/LAST/project/506101_20240523131040585_excel.xlsx")

# 전처리 
data1 = sum_birthrate[2:3,]
colnames(data1) <- as.character(unlist(data1[1,]))
data1 <- data1[-1, ]

data_melt <- data1 %>%
  reshape2::melt(id.vars = NA, 
                 variable.name = "연도",
                 value.name = "출산율")
data_melt = data_melt[-1] 
data_melt$연도 <- as.numeric(as.character(data_melt$연도))
data_melt$출산율= as.numeric(data_melt$출산율)

# plot 01 
p1 = 
  ggplot(data_melt, aes(x = 연도, y = 출산율)) +
  geom_line() +
  geom_point() +  # 데이터 포인트 표시
  geom_label(aes(label = sprintf("%.2f", 출산율)), size = 5, vjust = -0.5, label.padding = unit(0.15, "lines"), fill = "white") +  # 데이터 포인트 값 표시
  theme_minimal() +
  theme(
    legend.position = 'right', 
    text = element_text(family = "나눔고딕", size = 12), 
    plot.title = element_text(size = 20),  # 연도 글씨 크기
    axis.title = element_text(size = 16)  # xlab, ylab 글씨 크기
  ) +
  transition_reveal(연도) +
  labs(
    title = '대한민국 출산율 변화: {round(frame_along)}년', 
    x = "연도", 
    y = "출산율", 
    caption = "\n출처: 통계청, 「인구동향조사」 
    \n 자료 : 통계청, 「인구동향조사」 각 연도"
  )


# 저장 
animate(p1, duration = 15, fps = 10, width = 800, height = 400, renderer = gifski_renderer("p1.gif"))








