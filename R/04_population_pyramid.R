# 필요한 패키지 로드
library(ggplot2)
library(dplyr)

# 데이터 전처리
data.use <- bind_rows(data.boy01, data.girl01)
data.use$인구수[is.na(data.use$인구수)] <- 0
data.use <- data.use %>%
  mutate(인구수 = ifelse(성별 == "남자", -인구수, 인구수))

# 시각화
p4 = ggplot(data.use, aes(x = 연령별, y = 인구수, fill = 성별)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "나눔고딕")) +
  transition_time(연도) +
  labs(title = '인구 피라미드: {round(frame_time)}년')

# 애니메이션 저장
animate(p4, duration = 15, fps = 10, 
        width = 800, height = 400, 
        renderer = gifski_renderer("output/figures/p4.gif"))