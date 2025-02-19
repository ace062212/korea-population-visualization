# 필요한 패키지 로드
library(ggplot2)
library(dplyr)
library(gganimate)

# 데이터 로드 및 전처리
age_birthrate <- read_excel("data/raw/birth_rate/H000201_20240523133739046_excel.xlsx")
data2 = age_birthrate[2:9,]
colnames(data2) <- as.character(unlist(data2[1,]))
data2 <- data2[-1, ]

# 데이터 변환
data_melt2 <- data2 %>%
  reshape2::melt(id.vars = NA, 
                 variable.name = "연도",
                 value.name = "출산율")
colnames(data_melt2) <- c("연령대","연도", "출산율")

# 시각화
p2 = ggplot(data_melt2, aes(x = 연도, y = 출산율, colour = 연령대)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(text = element_text(family = "나눔고딕")) +
  transition_reveal(연도) +
  labs(title = '연령별 출산율 변화: {round(frame_along)}년')

# 애니메이션 저장
animate(p2, duration = 15, fps = 10, 
        width = 800, height = 400, 
        renderer = gifski_renderer("output/figures/p2.gif"))