
df_test <- read.csv("ggpark2.csv",fileEncoding = "euc-kr") # pandas에서 내보냈던 데이터셋
df_test

# dplyr로 남부_북부 그룹화
df_test %>%
  group_by(남부_북부) %>%
  summarise(갯수=n(),
            평균_노상.유료=mean(X2021_노상.유료.),
            평균_노상.무료=mean(X2021_노상.무료.),
            평균_노외.공영=mean(X2021_노외.공영.),
            평균_노외.민영=mean(X2021_노외.민영.),
  )


# 시군별로 주차장수 종류별 주차장 수와 그의 합계 total 변수 생성
df_4<- df_test %>%
  group_by(시군별) %>%
  select(X2021_노상.유료.,X2021_노상.무료.,X2021_노외.공영.,X2021_노외.민영.) %>%
  mutate(total = X2021_노상.유료.+ X2021_노상.무료.+X2021_노외.공영.+X2021_노외.민영.)

df_4

# 경기도 전체 시군별 주차장 수 막대 그래프
p1 <- df_4 %>%
  ggplot(aes(x=시군별,y=total)) +
  geom_col(fill="hotpink") +
  geom_text(aes(label = total), hjust = 0, nudge_y=1) +
  coord_flip() +
  labs(x = "전체 주차장 수", y = "경기도 시군", title = "전체주차장 수")+
  mytheme

# 경기도 전체 주차장 남부/북부 수의 분포
## 전체 합계 변수 추가
df_5 <- df_5 %>%
  mutate(total = X2021_노상.유료.+ X2021_노상.무료.+X2021_노외.공영.+X2021_노외.민영.)

p2 <- df_5 %>%
  ggplot(aes(x=loc,y=total)) +
  geom_boxplot(fill="aliceblue") +
  labs(x = "경기도 남부/북부 ", y = "전체 주차장 수", title = "전체주차장 분포") +
  mytheme

# 경기도 무료 주차장 수 막대 그래프
## 무료 합계 변수 추가
df_5 <- df_5 %>%
  mutate(Free=X2021_노상.무료.+X2021_노외.공영.)

p3 <- df_5 %>%
  ggplot(aes(x=시군별,y=Free)) +
  geom_col(fill="chocolate") +
  coord_flip() +
  geom_text(aes(label = Free), hjust = 0, nudge_y=1) +
  labs(x = "무료 주차장 수 ", y = "무료 주차장 수", title = "무료료주차장 수") +
  mytheme

# 경기도 무료 주차장 남부/북부 수의 분포
p4 <- df_5 %>%
  ggplot(aes(x=loc,y=Free)) +
  geom_boxplot(fill="coral4") +
  labs(x = "경기도 남부/북부 ", y = "무료 주차장 수", title = "무료료주차장 분포") +
  mytheme

# 4분할 화면으로 출력
library(gridExtra)
grid.arrange(p1,p3,p2,p4,nrow=2, ncol=2)


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
