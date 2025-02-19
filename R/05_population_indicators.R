# 필요한 패키지 로드
library(ggplot2)
library(dplyr)

# 데이터 전처리
data_use <- data_use %>%
  select("연도", "중위연령(세)", "노령화지수", "총부양비")

# 시각화
ggplot(data_long, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(text = element_text(family = "나눔고딕")) +
  labs(title = "주요 인구지표 변화")