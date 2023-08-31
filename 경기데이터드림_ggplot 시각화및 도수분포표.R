library(dplyr)
library(ggplot2)
df_test <- read.csv("ggpark2.csv",fileEncoding = "euc-kr") # pandas에서 내보냈던 데이터셋
df_test

# ggplot 테마 생성
mytheme <-theme(plot.title=element_text(face="bold.italic",
                                        size=14,
                                        color="brown")) + # 그래프의 테마 변경
  theme(axis.title=element_text(face="bold.italic",
                                size=10,
                                color="tomato")) +
  theme(axis.text=element_text(face="bold",
                               size=9,
                               color="royalblue"),
        panel.background = element_rect(fill="snow",color="darkblue"),
        panel.grid.major.y = element_line(color="gray",linetype="solid"),
        panel.grid.minor.y = element_line(color="gray",linetype="dashed"),
        legend.position = "inside")


# 시군별로 주차장수 종류별 주차장 수와 그의 합계 total 변수 생성
df_4<- df_test %>%
  group_by(시군별) %>%
  select(X2021_노상.유료.,X2021_노상.무료.,X2021_노외.공영.,X2021_노외.민영.) %>%
  mutate(total = X2021_노상.유료.+ X2021_노상.무료.+X2021_노외.공영.+X2021_노외.민영.)

df_4

df_5 <- df_4 %>%
  group_by(시군별) %>%
  mutate(pay = X2021_노상.유료.+ X2021_노외.민영.) %>%
  mutate(free = X2021_노상.무료.+ X2021_노외.공영.) %>%
  mutate(city=as.factor(시군별)) %>%
  arrange(city)

df_5

df_6 <- df_test %>%
  group_by(남부_북부) %>%
  mutate(pay = X2021_노상.유료.+ X2021_노외.민영.) %>%
  mutate(free = X2021_노상.무료.+ X2021_노외.공영.) %>%
  select(pay,free,시군별)

df_6

value <- rev(as.numeric(rownames(df_5['city']))) # 가평군이 맨위에 출력되게
value

# 경기도 시군별 무료주차장 수 막대 그래프
p1 <- df_5 %>%
  ggplot(aes(x=reorder(city,value),y=free)) +
  geom_col(fill="hotpink") +
  geom_text(aes(label = free), hjust = 0, nudge_y=1) +
  coord_flip() +
  labs(x = "경기도 시군별", y = "무료주차장수수")+
  mytheme


# 경기도 시군별 유료 주차장 수 막대 그래프
p2 <- df_5 %>%
  ggplot(aes(x=reorder(city,value),y=pay)) +
  geom_col(fill="chocolate") +
  coord_flip() +
  geom_text(aes(label = pay), hjust = 0, nudge_y=1) +
  labs(x = "경기도 시군별", y = "유료주차장수") +
  mytheme


# 경기도 무료 주차장 남부/북부 수의 분포
p3 <- df_6 %>%
  ggplot(aes(x=남부_북부,y=free)) +
  geom_boxplot(fill="aliceblue") +
  labs(x = "경기도 남부/북부 ", y = "무료주차장수") +
  mytheme

# 경기도 유료 주차장 남부/북부 수의 분포
p4 <- df_6 %>%
  ggplot(aes(x=남부_북부,y=pay)) +
  geom_boxplot(fill="coral4") +
  labs(x = "경기도 남부/북부 ", y = "유료주차장 수") +
  mytheme

# 4분할 화면으로 출력
library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2, ncol=2)

