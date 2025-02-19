# 필요한 패키지 로드
library(ggplot2)
library(dplyr)

# 데이터 전처리
raw.data.using = raw.data_1950_2000[8:90,2:6]
colnames(raw.data.using) <- c("연도", "총인구", "남자", "여자")

# 시각화
ggplot(raw.data.using, aes(x = 연도)) +
  geom_line(aes(y = 총인구)) +
  theme_minimal() +
  theme(text = element_text(family = "나눔고딕")) +
  labs(title = "세계 인구와의 비교")