# 필요한 패키지 로드
library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)
library(tsibble)

# 데이터 로드 및 전처리
sum_birthrate <- read_excel("data/raw/birth_rate/506101_20240523131040585_excel.xlsx")
data1 = sum_birthrate[2:3,]
colnames(data1) <- as.character(unlist(data1[1,]))
data1 <- data1[-1, ]

# 데이터 변환
data_melt <- data1 %>%
  reshape2::melt(id.vars = NA, 
                 variable.name = "연도",
                 value.name = "출산율")
data_melt = data_melt[-1] 
data_melt$연도 <- as.numeric(as.character(data_melt$연도))
data_melt$출산율 = as.numeric(data_melt$출산율)

# 시각화
p1 = ggplot(data_melt, aes(x = 연도, y = 출산율)) +
  geom_line() +
  geom_point() +
  geom_label(aes(label = sprintf("%.2f", 출산율)), 
             size = 5, vjust = -0.5) +
  theme_minimal() +
  theme(text = element_text(family = "나눔고딕")) +
  transition_reveal(연도) +
  labs(title = '대한민국 출산율 변화: {round(frame_along)}년')

# 애니메이션 저장
animate(p1, duration = 15, fps = 10, 
        width = 800, height = 400, 
        renderer = gifski_renderer("output/figures/p1.gif"))