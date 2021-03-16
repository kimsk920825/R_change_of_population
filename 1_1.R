## 서울시 생활인구 변화 분석

## 데이터 출처 : "행정동별 서울생활인구(내국인)" -> 집계 및 변환
##               서울열린데이터광장(http://data.seoul.go.kr/dataList/OA-14991/S/1/datasetView.do)

## 데이터 설명 : 2019, 2020년 12월 한달 간 서울시 25개 자치구, 24개 시간대, 7개 연령대별 생활인구수

## 분석 목표   : 2019년 대비 2020년의 생활인구 변화의 특성 파악
install.packages("extrafont")
library(extrafont)
font_import()
library(dplyr)
library(ggplot2)
theme_set(theme_grey(base_family='NanumGothic'))

data_2019 = read.csv('data/SEOUL_PEOPLE_GU_2019.csv', fileEncoding = 'UTF-8', colClasses=c('시간대'='character'))
data_2020 = read.csv('data/SEOUL_PEOPLE_GU_2020.csv', fileEncoding = 'UTF-8', colClasses=c('시간대'='character'))
data_2019 %>% head()
data_2020 %>% head()

#1. 전처리
## 2개로 나눠진 데이터를 하나의 데이터로 합치자. 
SP_GU = rbind(data_2019, data_2020)
SP_GU %>% head(10)
SP_GU %>% tail(10)

SP_GU %>% str() #기준일의 character가 시간으로 되있는것을 알 수 있다. 

SP_GU = SP_GU %>% 
  mutate(기준일 = as.Date(기준일))
SP_GU %>% str()

SP_GU = SP_GU %>% mutate(연도 = format(기준일,'%Y'))
SP_GU = SP_GU %>% mutate(요일 = format(기준일, '%u_%a'))
SP_GU %>% head()

#2. 요약 및 시각화
## 크리스마스 생활 인구수 비교 
SP_GU %>% 
  filter(기준일 %in% c(as.Date('2019-12-24'), as.Date('2020-12-24'))) %>% 
  group_by(기준일) %>% 
  summarise(TOTAL = sum(생활인구수))
#해석:인구수가 많은 이유는 한사람이 여러번 중복될 수 있기때문이다. 결과적으로 2019년도의 크리스마스 유동인구가 더 많다.
## 저녁시간대의 크리스마스 이동 인구를 계산해보자 
SP_GU %>% 
  filter(기준일 %in% c(as.Date('2019-12-24'), as.Date('2020-12-24'))) %>% 
  filter(시간대 %in% c('18','19','20','21')) %>% 
  group_by(기준일) %>% 
  summarise(TOTAL = sum(생활인구수))
#해석: 저녁시간의 생활인구 또한 2019년도가 앞선다.
##자치구별 생활인구수 합계 계산 / 막대그래프 시각화 
agg1 = SP_GU %>% 
  group_by(자치구) %>% 
  summarise(TOTAL = sum(생활인구수))
agg1 %>% 
  arrange(desc(TOTAL))
#해석: 2019, 2020총 합해서 강남구에 생활인구가 가장 많았고 그 다음이 송파구인걸로 나온다. 생활인구가 크게 나온건 개인별, 시간별이 중복될 뿐만 아니라 2019,2020년도를 합해서 계산했기 떄문이다. 
agg1 %>% 
  ggplot(aes(자치구, TOTAL))+
  geom_col()
agg1 %>% 
  ggplot(aes(reorder(자치구,-TOTAL),TOTAL)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle=90))
## 연도/자치구별 생활인구수 합계 계산/ 나란히 그린 막대그래프 시각화 
agg2 = SP_GU %>% 
  group_by(연도, 자치구) %>% 
  summarise(TOTAL = sum(생활인구수))

agg2 %>%  ggplot(aes(reorder(자치구,-TOTAL),TOTAL,fill=연도)) + geom_col(position='dodge') +
  theme(axis.text.x = element_text(angle=90))
#해석: 연도와 상관없이 강남구 송파구 서초구가 가장 생활인구가 많고, 2019년도가 근소하게 생활 인구가 2020년도보다 많다. 

## 2020년의 각 자치구별 생활인구의 연령대 비중 계산해보자. 나이 구간별로 생활한 장소가 다를 수 있지 않을까? 
agg3 = SP_GU %>% 
  filter(연도 =='2020') %>% 
  group_by(자치구, 연령대) %>% 
  summarise(TOTAL = sum(생활인구수)) %>% 
  mutate(PROP = TOTAL/sum(TOTAL))
agg3

agg3 %>% 
  ggplot(aes(연령대, 자치구, fill=PROP))+
  geom_tile()+ scale_fill_distiller(palette = 'Blues', direction =1)
#해석:2020년도에는 10대는 양천구에, 20대는 관악구, 30대는 중구, 40대는 중구, 서초구, 강남구, 50대부터 70대는 점점 생활 빈도가 낮아지는걸 볼 수 있다. 

## 연도/자치구/요일별 일평균 생활인구수 계산해보기 
### To keep in mind: 하루에 24개 시간대와 7개 연려대의 관측치가 있음. 
agg4 = SP_GU %>% 
  group_by(연도, 자치구, 요일, 기준일) %>% 
  summarise(TOTAL = sum(생활인구수))
agg4
#해석:음.. 특별한 insight를 얻지 못했다.. 
##연도/자치구/요일별 일평균 생활인구수의 열지도 시각화
### 요일(x)/ 자치구(y)별 일평균 생활인구수 열지도를 facet_wrap()을 활용해서 연도별로 분할해볼 예정. 
### To keep in mind: 요약된 데이터가 있으면 그 요약된 데이터에 쓰였던 모든 변수를 사용해서 시각화를 해야한다. 
agg5 = agg4 %>% 
  summarise(MEAN = mean(TOTAL))
agg5

agg5 %>% 
  ggplot(aes(요일, 자치구, fill=MEAN)) +
  geom_tile()+
  facet_wrap(vars(연도))+
  scale_fill_distiller(palette = 'YlGnBu', direction =1)

#해석: 2019년도와 2020년도 두 해 모두 월요일부터 일요일까지 송파구와 강남구는 북적거린다. 하지만 강남구의 2019년의 색갈이 2020년의 색깔보다 더 진하다. 

## tidyr의 spread()를 활용한 형태 변환 및 변화율 계산 
library(tidyr)
agg5
agg5 %>% 
  spread(연도, MEAN)

agg6 = agg5 %>% 
  spread(연도, MEAN) %>% 
  mutate(RATIO = `2020`/`2019`)
agg6

agg6 %>% 
  ggplot(aes(요일, 자치구, fill=RATIO)) + 
  geom_tile() + 
  scale_fill_distiller(palette = 'Reds')
#해석: 2019년도와 2020년도 생활 인구 변화가 가장 두드러지게 보이는 지역은 중구와 종로구이다. 특히, 주말이 될 수록 생활 인구가 줄어드는것을 볼 수 있다. 

##연도/ 연령대/ 요일별 일평균 생활인구수를 계산하고
##2019년 대비 2020년의 변화율을 열지도로 시각화 
agg7 = SP_GU %>% 
  group_by(연도, 연령대, 요일, 기준일) %>% 
  summarise(TOTAL = sum(생활인구수))
agg8 = agg7 %>% 
  summarise(MEAN = mean(TOTAL))
agg8
agg9 = agg8 %>% 
  spread(연도, MEAN) %>% 
  mutate(RATIO = `2020`/`2019`)
agg9
agg9 %>% 
  ggplot(aes(요일, 연령대,fill=RATIO)) + 
  geom_tile()+
  scale_fill_distiller(palette = "Reds")
#해석: 2019년에 비해 2020년에 생활 반경이 줄어든 나이대는 20대, 60대, 70대인걸 볼 수 있다. 코로나가 터져도 30대, 40대, 50대는 직장을 다녀야 하기 때문에 다른 나이때보다 생활 반경에 영향을 덜 받지 않았나 유추해볼 수 있을것같다. 
# 또한 전반적으로 서울 전 연령대에 붉은색이 띄는 것을 보아 1이상이 넘는 즉 2019년보다 2020년에 서울에서의 인구 활동이 줄어들었다는 것을 볼 수 있고 그 뜻은 경기권이나 타 지역에서 서울로 유입되는 사람들이 줄어들었다라고 해석할 수도 있겠다.

##위 분석을 18~시22시 4시간만 선택해서 반복 

### 조건: 시간대 %in% c('18','19','20','21')
agg10 = SP_GU %>% 
  filter(시간대 %in% c('18','19','20','21')) %>% 
  group_by(연도, 연령대, 요일, 기준일) %>% 
  summarise(TOTAL = sum(생활인구수)) %>% 
  summarise(MEAN = mean(TOTAL)) %>% 
  spread(연도, MEAN) %>% 
  mutate(RATIO = `2020`/`2019`)
agg10 %>% 
  ggplot(aes(요일, 연령대,fill=RATIO)) + 
  geom_tile()+
  scale_fill_distiller(palette = "Reds")

