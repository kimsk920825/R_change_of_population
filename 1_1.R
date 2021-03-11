## 서울시 생활인구 변화 분석

## 데이터 출처 : "행정동별 서울생활인구(내국인)" -> 집계 및 변환
##               서울열린데이터광장(http://data.seoul.go.kr/dataList/OA-14991/S/1/datasetView.do)

## 데이터 설명 : 2019, 2020년 12월 한달 간 서울시 25개 자치구, 24개 시간대, 7개 연령대별 생활인구수

## 분석 목표   : 2019년 대비 2020년의 생활인구 변화의 특성 파악

library(dplyr)
library(ggplot2)

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
