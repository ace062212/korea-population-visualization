

library(dplyr)
library(readxl)
library(tidyr)
library(reshape2)
library(ggplot2)
options(scipen = 999)

kor_pop_2172_ml <- read.csv("Dropbox/앱/data_visual/LAST/project/combined_df_full_01 복사본.csv")
kor_pop_2172 <- read.csv("Dropbox/앱/data_visual/LAST/project/final_forecast 복사본.csv")

##############################################################################################################################################
# 전처리 
kor_pop_2072 <- read_excel("Dropbox/앱/data_visual/LAST/project/주요_인구지표_성비_인구성장률_인구구조_부양비_등____전국_20240610162105 복사본.xlsx")
kor_pop_2072 = kor_pop_2072[,-1]
data_melt <- kor_pop_2072 %>%
  reshape2::melt(id.vars = NULL, 
                 variable.name = "연도",
                 value.name = "출산율")
data_melt <- kor_pop_2072 %>%
  reshape2::melt(id.vars = "인구구조,부양비별", variable.name = "연도",value.name ="값")
data_use <- spread(data_melt, key = "인구구조,부양비별", value = "값")
data_use
##############################################################################################################################################
#####################
# 지표 필터링 
data_use %>% names()
data_use <- data_use %>%
  mutate(across(everything(), as.numeric))
data_use %>% summary()

##############################################################################################################################################
#####################
# 인구지표 (비율)
data_use %>% names()
filtered_data <- data_use %>% 
  select("연도", "- 구성비(%): 0-14세", "- 구성비(%): 15-64세", 
         "- 구성비(%): 65세 이상","성비(여자1백명당)"  )

data_long <- filtered_data %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")

ggplot(data_long, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  annotate("text", x = 2024, y = Inf, label = "2024", vjust = 20, hjust = 1.2, color = "red", size = 6) +  # 기준선 위에 텍스트 추가
  labs(title = "인구지표 (비율)", x = "연도", y = "값(비율)") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(data_long$연도), max(data_long$연도), by = 10)) +
  theme(text = element_text(family = "나눔고딕"))

##############################################################################################################################################
#####################
# 인구지표 (비용)
data_use %>% summary()
data_use %>% names()
filtered_data <- data_use %>% 
  select("연도", "노년부양비", "유소년부양비" , 
         "총부양비")

data_long <- filtered_data %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")

ggplot(data_long, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  annotate("text", x = 2024, y = Inf, label = "2024", vjust = 20, hjust = 1.2, color = "red", size = 6) +  # 기준선 위에 텍스트 추가
  labs(title = "인구지표 (비용)", x = "연도", y = "값(비용)") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(data_long$연도), max(data_long$연도), by = 10)) +
  theme(text = element_text(family = "나눔고딕"))


##############################################################################################################################################
#####################
# 인구지표 (비용)
data_use %>% summary()
data_use %>% names()
filtered_data <- data_use %>% 
  select("연도", "남자(명)", "여자(명)", 
         "인구(명): 0-14세","인구(명): 15-64세","인구(명): 65세 이상","총인구(명)"  )

data_long <- filtered_data %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")

ggplot(data_long, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  annotate("text", x = 2024, y = Inf, label = "2024", vjust = 20, hjust = 1.2, color = "red", size = 6) +  # 기준선 위에 텍스트 추가
  labs(title = "인구지표 (명)", x = "연도", y = "값(명)") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(data_long$연도), max(data_long$연도), by = 10)) +
  theme(text = element_text(family = "나눔고딕"))

##############################################################################################################################################
#####################
# 인구지표 (연령)
data_use %>% summary()
data_use %>% names()
filtered_data <- data_use %>% 
  select("연도", "중위연령(세)", "중위연령(세)-남자" , 
         "중위연령(세)-여자", "평균연령(세)",  "평균연령(세)-남자","평균연령(세)-여자" )

data_long <- filtered_data %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")

ggplot(data_long, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  annotate("text", x = 2024, y = Inf, label = "2024", vjust = 20, hjust = 1.2, color = "red", size = 6) +  # 기준선 위에 텍스트 추가
  labs(title = "인구지표 (연령)", x = "연도", y = "값(연령)") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(data_long$연도), max(data_long$연도), by = 10)) +
  theme(text = element_text(family = "나눔고딕"))

##############################################################################################################################################
#####################
# 인구지표 (인구성장률)
data_use %>% summary()
data_use %>% names()
filtered_data <- data_use %>% 
  select("연도","인구성장률" )

data_long <- filtered_data %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")

ggplot(data_long, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  annotate("text", x = 2024, y = Inf, label = "2024", vjust = 10, hjust = 1.2, color = "red", size = 6) +  # 기준선 위에 텍스트 추가
  labs(title = "인구지표 (인구성장률)", x = "연도", y = "값(인구성장률)") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(data_long$연도), max(data_long$연도), by = 10)) +
  theme(text = element_text(family = "나눔고딕"))

##############################################################################################################################################
#####################
# 인구지표 (인구성장률)
data_use %>% summary()
data_use %>% names()
filtered_data <- data_use %>% 
  select("연도","노령화지수"   )

data_long <- filtered_data %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")

ggplot(data_long, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  geom_vline(xintercept = 2024, linetype = "dashed", color = "red") +
  annotate("text", x = 2024, y = Inf, label = "2024", vjust = 10, hjust = 1.2, color = "red", size = 6) +  # 기준선 위에 텍스트 추가
  labs(title = "인구지표 (노령화지수)", x = "연도", y = "값(노령화지수)") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(data_long$연도), max(data_long$연도), by = 10)) +
  theme(text = element_text(family = "나눔고딕"))


##############################################################################################################################################
# 인구 지표중 100 이하의 값을 가지고 평균 값이 10이 넘지않는 지표 
# 필터링과 시각화
data_use %>% names()
data_use %>% str()
data_use$연도 <- as.numeric(as.character(data_use$연도))

# 데이터프레임을 긴 형식으로 변환
data_long <- data_use %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")
data_long$값 <- as.numeric(as.character(data_long$값))


filtered_data <- data_long %>% filter(값 <= 100)


# 각 지표별 평균값 계산
mean_values <- filtered_data %>%
  group_by(지표) %>%
  summarize(평균값 = mean(값, na.rm = TRUE))

# 평균값이 10 이하인 지표만 필터링
filtered_mean_values_10 <- mean_values %>% filter(평균값 <= 10)

# 필터링된 지표에 해당하는 데이터만 추출
final_filtered_data <- filtered_data %>% filter(지표 %in% filtered_mean_values_10$지표)


ggplot(final_filtered_data, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  labs(title = "연도별 지표 (평균값이 10 이하인 경우)", x = "연도", y = "값 (로그 스케일)") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(final_filtered_data$연도), max(final_filtered_data$연도), by = 10)) +
  annotation_logticks(sides = "l") +  # 왼쪽(y축)에 로그 눈금 표시 +
  theme(text = element_text(family = "나눔고딕"))
##############################################################################################################################################
#####################
# 인구 지표중 100 이하의 값을 가지고 평균 값이 10이 넘지않는 지표 
# 필터링과 시각화
data_use %>% names()
data_use %>% str()
data_use$연도 <- as.numeric(as.character(data_use$연도))

# 데이터프레임을 긴 형식으로 변환
data_long <- data_use %>%
  pivot_longer(cols = -연도, names_to = "지표", values_to = "값")
data_long$값 <- as.numeric(as.character(data_long$값))


filtered_data <- data_long %>% filter(값 <= 1000 | 값 > 100)


# 각 지표별 평균값 계산
mean_values <- filtered_data %>%
  group_by(지표) %>%
  summarize(평균값 = mean(값, na.rm = TRUE))

# 평균값이 10 이하인 지표만 필터링
filtered_mean_values_10 <- mean_values %>% filter(평균값 <= 100 | 평균값 > 10)

# 필터링된 지표에 해당하는 데이터만 추출
final_filtered_data <- filtered_data %>% filter(지표 %in% filtered_mean_values_10$지표)

ggplot(final_filtered_data, aes(x = 연도, y = 값, color = 지표)) +
  geom_line() +  # 선 그래프
  labs(title = "연도별 지표 (평균값이 10 이하인 경우)", x = "연도", y = "값 (로그 스케일)") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(min(final_filtered_data$연도), max(final_filtered_data$연도), by = 10)) +
  annotation_logticks(sides = "l") +  # 왼쪽(y축)에 로그 눈금 표시 +
  theme(text = element_text(family = "나눔고딕"))


## 메모리 한계 코드 폐기 



