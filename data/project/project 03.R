
#install.packages("readxl")
library(readxl)
#install.packages("dplyr")
library(dplyr)
#install.packages("reshape2")
library(reshape2)
kor_pop <- read_excel("지역별 인구 및 인구밀도_20240523144105.xlsx")

data3 = kor_pop[28:48,]

colnames(data3) <- as.character(unlist(data3[1,]))
data3 <- data3[-1, ]

data_melt3 <- data3 %>%
  reshape2::melt(id.vars = NA, 
                 variable.name = "연도",
                 value.name = "인구")

colnames(data_melt3) <- c("지역","연도", "인구")
data_melt3 <- data_melt3 %>%
  filter(!is.na(지역)) %>% 
  filter(!지역 == "계") %>% 
  filter(!지역 == "수도권")

# NA 및 "-" 값을 0으로 대체
data_melt3$인구[data_melt3$인구 == "-" | is.na(data_melt3$인구)] <- "0"

# 인구를 숫자로 변환
data_melt3$인구 <- as.numeric(gsub(",", "", as.character(data_melt3$인구)))
data_melt3_join$연도 <- as.numeric(as.character(data_melt3_join$연도))

# install.packages("ggplot2")
library(ggplot2)
ggplot(data_melt3, aes(x = 연도, y = 지역, fill = 인구)) +
  geom_tile(color = "white", linewidth = 0.25) +
  theme(text = element_text(family = "나눔고딕"))



library(geojsonsf)

library(sf)

# 절대 경로를 사용하여 파일 경로 수정
file_path <- 'SIDO_MAP_2022.json'

# 파일 읽기
KOR_SIDO <- st_read(file_path)
KOR_SIDO <- KOR_SIDO %>%
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "서울특별시", "서울", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "부산광역시", "부산", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "대구광역시", "대구", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "인천광역시", "인천", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "광주광역시", "광주", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "대전광역시", "대전", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "울산광역시", "울산", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "세종특별자치시", "세종", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "경기도", "경기", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "강원도", "강원", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "충청북도", "충북", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "충청남도", "충남", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "전라북도", "전북", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "전라남도", "전남", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "경상북도", "경북", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "경상남도", "경남", CTP_KOR_NM)) %>% 
  mutate(CTP_KOR_NM  = ifelse(CTP_KOR_NM   == "제주특별자치도", "제주", CTP_KOR_NM)) 
  
KOR_SIDO <- KOR_SIDO %>%
  rename(지역 = CTP_KOR_NM)




library(dplyr)
library(stringr)
library(tidyr)
library(gganimate)

# 병합
KOR_SIDO <- KOR_SIDO %>% merge(data_melt3, by = "지역", all.x=T)
# '연도' 열을 숫자형으로 변환
KOR_SIDO$연도 <- as.numeric(as.character(KOR_SIDO$연도))
KOR_SIDO <- KOR_SIDO %>%
  mutate(연도 = as.double(연도))
KOR_SIDO
# '연도' 열을 기준으로 데이터프레임 정렬


blue_palette <- scale_fill_gradient(low = "white", high = "red")


## ggplot
library(gganimate)
KOR_SIDO$인구_LogScale <- log(KOR_SIDO$인구)
p3 = 
KOR_SIDO %>% ggplot(aes(fill = 인구)) + 
  geom_sf(color = NA) + 
  transition_time(연도) +
  labs(title = "대한민국 년도 별 인구수: {round(frame_time)}년",
       caption = "\n출처: 통계청「장래인구추계 시도편 : 2020-2050」국토교통부「지적통계」
       \n통계공표시기: 작성기준년도 5년
       \n 단위 : 천/명") + 
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.caption.position = "plot",
        text = element_text(family = "나눔고딕"),
        axis.text = element_blank(),
        axis.title = element_blank())  +
  blue_palette


# 저장 
animate(p3, duration = 15, fps = 10, width = 800, height = 800, renderer = gifski_renderer("p3.gif"))
