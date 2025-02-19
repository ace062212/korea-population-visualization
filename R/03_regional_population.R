# 필요한 패키지 로드
library(sf)
library(ggplot2)
library(dplyr)

# 지도 데이터 로드
KOR_SIDO <- st_read("data/raw/SIDO_MAP_2022.json")

# 인구 데이터 전처리
data_melt3$인구 <- as.numeric(gsub(",", "", as.character(data_melt3$인구)))
data_melt3_join = left_join(data_melt3, geo_data)

# 시각화
p3 = KOR_SIDO %>% 
  ggplot(aes(fill = 인구)) + 
  geom_sf(color = NA) + 
  transition_time(연도) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_void() +
  theme(text = element_text(family = "나눔고딕")) +
  labs(title = "지역별 인구 분포: {round(frame_time)}년")

# 애니메이션 저장
animate(p3, duration = 15, fps = 10, 
        width = 800, height = 800, 
        renderer = gifski_renderer("output/figures/p3.gif"))