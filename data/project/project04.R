
library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)
library(tsibble)
library(readxl)
library(cowplot)
raw.data <- read_excel("성_및_연령별_추계인구_1세별__5세별____전국_20240609171516.xlsx")
raw.data %>% dim()

raw.data = raw.data[,2:116]
print(raw.data)
names(raw.data)


#########################################################################################

# boy 

data.boy = raw.data[24:46,]
data.boy %>% head()
data.boy <- data.boy %>%
  mutate(성별 = replace_na(성별, "남자"))
# 데이터 변형
data.boy01 <- data.boy %>%
  reshape2::melt(id.vars = c("성별", "연령별"), 
       variable.name = "연도",
       value.name = "인구수")
# 연도 열을 숫자로 변환
data.boy01$연도 <- as.integer(as.character(data.boy01$연도))
data.boy01 <- data.boy01 %>%
  filter(연령별 != "계")
data.boy01

#########################################################################################

# girl 
data.girl = raw.data[47:69,]
data.girl
data.girl <- data.girl %>%
  mutate(성별 = replace_na(성별, "여자"))
# 데이터 변형
data.girl01 <- data.girl %>%
  reshape2::melt(id.vars = c("성별", "연령별"), 
                 variable.name = "연도",
                 value.name = "인구수")
# 연도 열을 숫자로 변환
data.girl01$연도 <- as.integer(as.character(data.girl01$연도))
data.girl01 <- data.girl01 %>%
  filter(연령별 != "계")
data.girl01

#########################################################################################

# 남,녀 결합 
data.use <- bind_rows(data.boy01, data.girl01)
data.use %>% head()
data.use %>% tail()

#########################################################################################


# 피라미드 그래프 
# 결측치 대체 : "0"
data.use$인구수[is.na(data.use$인구수)] <- 0
data.use <- data.use %>%
  na.omit()


data.use <- data.use %>%
  mutate(인구수 = as.numeric(인구수))


data.use <- data.use %>%
  mutate(연령별 = factor(연령별, levels = c("0 - 4세", "5 - 9세", "10 - 14세", "15 - 19세", 
                                      "20 - 24세", "25 - 29세", "30 - 34세", "35 - 39세", 
                                      "40 - 44세", "45 - 49세", "50 - 54세", "55 - 59세",
                                      "60 - 64세", "65 - 69세", "70 - 74세", "75 - 79세",
                                      "80 - 84세", "85 - 89세", "90 - 94세", "95 - 99세", 
                                      "100세 이상"))) %>%
           arrange(연도, 연령별, 성별)
         


# 남성 인구수를 음수로 변환
data.use <- data.use %>%
  mutate(인구수 = ifelse(성별 == "남자", -인구수, 인구수))

data.use.1970 = data.use %>% filter(연도 == "1970")

abs(data.use$인구수)


p4 = 
ggplot(data.use, aes(x = 연령별, y = 인구수, fill = 성별)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # x축과 y축을 뒤집어 피라미드 형태로 만듦  # y축 레이블을 절대값으로 표시
  labs(title = "남녀 인구 피라미드",
       x = "연령대",
       y = "인구수",
       fill = "성별") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1L)) + 
  theme_minimal() +
  theme(text = element_text(family = "나눔고딕"), 
        plot.caption.position = "plot") + 
  transition_time(연도) +
  labs(title = '대한민국 남녀 인구 피라미드: {round(frame_time)} 년' ,
       caption = "\n출처: 통계청 사회통계국 인구추계팀
    \n 「장래인구추계」 통계청")

animate(p4, duration = 15, fps = 10, width = 800, height = 400, renderer = gifski_renderer("p4.gif"))


#########################################################################################



 
data.use.raito <- data.use %>%
  mutate(인구수 = ifelse(성별 == "남자", -인구수, 인구수))
data.use.raito %>% head()



pop_ratio <- data.use.raito %>%
  group_by(연도) %>%
  summarise(남자_인구수 = sum(인구수[성별 == "남자"]),
            여자_인구수 = sum(인구수[성별 == "여자"])) %>%
  mutate(성별_비율 = 여자_인구수 / 남자_인구수)

head(pop_ratio)




