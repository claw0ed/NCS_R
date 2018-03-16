# 데이터 시각화

# 데이터 시각화 p15
# 인간의 시각적 능력을...

# 시각화 기능 p15
# 설명 - 데이터의...

# 탐색 - 데이터에 숨겨져 있는 관계와...

# 표현 - 데이터를 활용한 예술적...

accid_2015 <- read.csv(
  'c:/Java/seoul_car_acci_v1.txt', sep=',')
head(accid_2015) # 자치구별 2015년 교통사고 현황

accid_2016 <- read.csv(
  'c:/Java/car_accid_2016.txt', sep='\t')
head(accid_2016) # 자치구별 2016년 교통사고 현황

# accid_2005_2016 <- read.csv(
#   'c:/Java/car_accid_2005_2016.txt', sep='\t', fileEncoding='utf-8')
accid_2005_2016 <- read.csv(
  'c:/Java/car_accid_2005_2016.txt', sep='\t')
head(accid_2005_2016) # 자치구별 2005~2016년 교통사고 현황

# R 시각화 도구
# graphics : R 기본 시각화 도구, 별도설치x,
# 쉽고 사용하기 편함
# ggplot2 : R 고급 시각화 도구, 별도 설치o,
# 배우기 다소 어려움
# lattice : R 시각화 도구, 별도 설치o,
# 다중 그래프 작성시 편리, 직관성 떨어짐

# install.packages('lattice')
# library(lattice)

install.packages('ggplot2')

library(ggplot2)

# 그래프 작성 보조 도구 - dplyr
install.packages('dplyr')
library(dplyr)

# 산점도
head(iris)
gg1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width))
# 그래프 작성 초기화 (데이터집합, x축, y축)
gg1 <- gg1 + geom_point()
# 점 그래프 : 산점도, 산포도
print(gg1)

# Species 별로 색상, 크기를 지정해서 산포도 그림
# colour에 자동으로 색상 부여시 Factor 타입변수 필요!
# color=Species : 자동 색깔지정
gg2 <- ggplot(iris, aes(Sepal.Length, Sepal.Width))
gg2 <- gg2 + geom_point()
gg2 <- gg2 + geom_point(aes(color=Species,
                        size=Petal.Width))
print(gg2)

# 서울 중구 2015 교통사고 현황을 산점도geom_point로 작성
# ------------------------------------------------------
str(accid_2015)
junggu2015 <- subset(accid_2015, 자치구명=='중구')

p1 <- ggplot(data=junggu2015, aes(x=월, y=발생건수))
p1 + geom_point(aes(color=월,size=월))
# ------------------------------------------------------
str(accid_2015)
junggu2015 <- subset(accid_2015, 자치구명=='중구')

p1 <- ggplot(data=junggu2015, aes(x=월, y=발생건수))
# p1 <- p1 + geom_point()
p1 <- p1 + geom_point(aes(colour=월,size=월))
p1 <- p1 + xlim(1, 13) + ylim(0, 150) # x, y 상하한값
p1 <- p1 + ggtitle('2015년 중구 월별 교통사고 발생 현황')
print(p1)
# ------------------------------------------------------

# 또 다른 유형 산점도
x <- 1:50
# y = sapply(x, function(x) { x/(x+1) } )
y = sapply(x, function(x) x/(x+1) ) # 괄호 생략
df <- data.frame(x, y)
head(df)

gg3 <- ggplot(df, aes(x, y))
gg3 <- gg3 + geom_point()
print(gg3)

# 다이아몬드 데이터집합
# 캐럿carat당 가격price을 산포도로 그림
# 캐럿당 가격을 산포도로 그림, 색상은 color 변수 사용
head(diamonds)

gg4 <- ggplot(diamonds, aes(x=carat, y=price))
# 그래프 작성 초기화 (데이터집합, x축, y축)
gg4 <- gg4 + geom_point()
# 점 그래프 : 산점도, 산포도
gg4 <- gg4 + geom_point(aes(colour=color, size=price))

print(gg4)

# 선 그래프 : geom_line
head(economics)

# 시기 date별 실업unemploy율 현황
gg5 <- ggplot(economics)
gg5 <- gg5 + geom_line(aes(x= date, y=unemploy))
print(gg5)

# 선 그래프에 색상/크기/선종류 지정
gg6 <- ggplot(economics, aes(x= date, y=unemploy))
gg6 <- gg6 + geom_line(colour='red', size=1, linetype=3)
print(gg6)

# 여러 개의 선 그래프에 작성
gg7 <- ggplot(economics)
gg7 <- gg7 + geom_line(aes(x=date, y=unemploy))
gg7 <- gg7 + geom_line(aes(x=date, y=pce), colour='red')
print(gg7)

# 오렌지 생산량 현황
head(Orange)

# 선그래프 + 점그래프
gg8 <- ggplot(Orange, aes(age, circumference))
gg8 <- gg8 + geom_line(aes(colour=Tree))
gg8 <- gg8 + geom_point()
print(gg8)

# 서울 중구 2015 교통사고 현황(월/발생건수) 을
# 선 그래프 geom_line 로 작성
# ------------------------------------------------------
str(accid_2015)
junggu2015 <- subset(accid_2015, 자치구명=='중구')

p1 <- ggplot(data=junggu2015, aes(x=월, y=발생건수))
# p1 <- p1 + geom_point()
p1 <- p1 + geom_point(aes(colour=월,size=월))
p1 <- p1 + xlim(1, 13) + ylim(0, 150) # x, y 상하한값
p1 <- p1 + ggtitle('2015년 중구 월별 교통사고 발생 현황')
print(p1)
# ------------------------------------------------------
p2 <- ggplot(junggu2015, aes(월, 발생건수))
p2 <- p2 + geom_line()
p2 <- p2 + geom_point()
p2 <- p2 + xlim(1, 13) + ylim(0, 150) # x, y 상하한값
p2 <- p2 + ggtitle('2015년 중구 월별 교통사고 발생현황')
print(p2)
# ------------------------------------------------------
p2b <- ggplot(junggu2015, aes(월, 발생건수))
p2b <- p2b + geom_line()
p2b <- p2b + geom_point()
# p2b <- p2b + xlim(1, 13) + ylim(0, 150) # x, y 상하한값
p2b <- p2b + ggtitle('2015년 중구 월별 교통사고 발생현황')
p2b <- p2b + scale_x_continuous(breaks=1:12)
# p2b <- p2b + theme(panel.background=element_blank())
p2b <- p2b + theme(panel.background=
                     element_rect(fill='white', color='grey'),
                   plot.title = element_text(hjust=0.5))
print(p2b)


# 막대 그래프 : geom_bar
head(mtcars)
str(mtcars) # factor 요소 확인 : num

# 차량별 실린더수 현황 히스토그램
gg9 <- ggplot(mtcars, aes(cyl))
# gg9 <- gg9 + geom_bar()
gg9 <- gg9 + geom_bar()
print(gg9)


fcyl <- factor(mtcars$cyl)
# cyl 은 num  타입 - factor 형으로 변환
gg9b <- ggplot(mtcars, aes(fcyl))
gg9b <- gg9b + geom_bar(
  aes(fill=fcyl), width=0.5)
print(gg9b)


# 누적 막대 그래프
fgear <- factor(mtcars$gear)
gg9c <- ggplot(mtcars, aes(fcyl))
gg9c <- gg9c + geom_bar(
  aes(fill=fcyl), width=0.5)
gg9c <- gg9c + geom_bar(
  aes(fill=fgear), width=0.5)
print(gg9c)


# 수평 누적 막대 그래프
gg9d <- ggplot(mtcars, aes(fcyl))
gg9d <- gg9d + geom_bar(aes(fill=fcyl), width=0.5)
gg9d <- gg9d + geom_bar(aes(fill=fgear), width=0.5)
gg9d <- gg9d + coord_flip()
print(gg9d)

# 일반적인 막대그래프
# 실린더 수에 따른 연비 그래프
# stat='identity' 는 y축에 mpg값을 그대로 사용하라는 의미
gg10 <- ggplot(mtcars, aes(fcyl, mpg))
gg10 <- gg10 + geom_bar(aes(fill=fcyl), stat='identity')
print(gg10)

# 서울 중구 2015 교통사고 현황(월/발생건수) 을
# 선 그래프 geom_bar 로 작성
p4 <- ggplot(junggu2015, aes(월, 발생건수))
# p4 <- p4 + geom_bar(fill=junggu2015$월, stat='identity') # 안됨
# p4 <- p4 + geom_bar(fill='red', stat='identity')
p4 <- p4 + geom_bar(aes(fill=월), stat='identity')
print(p4)

head(junggu2015)

# 서울시 2015년 12월 각 자치구 별
# 교통사고 현황(자치구명/발생건수)을 출력
# 막대그래프 geom_bar 로 작성
accid_2015_12 <- subset(accid_2015, 연도==2015 & 월==12)
accid_2015_12

p5 <- ggplot(accid_2015_12, aes(자치구명, 발생건수))
# p5 <- p5 + geom_bar(aes(fill=rainbow(25)), stat='identity')
# 자치구명 대신 색깔로 나옴
# p5 <- p5 + geom_bar(fill='red', stat='identity')
p5 <- p5 + geom_bar(aes(fill=자치구명), stat='identity') # 자치구명 개수대로 색을 넣음
p5 <- p5 + theme(axis.text.x=element_text(angle=90, hjust=1)) # hjust=1 중간
print(p5)


# 서울시 2015년 12월 각 자치구 별
# 교통사고 현황(자치구명/부상자수)을 출력
accid_2015_12 <- subset(accid_2015, 연도==2015 & 월==12)
accid_2015_12

p6 <- ggplot(accid_2015_12, aes(자치구명, 부상자수))
p6 <- p6 + geom_bar(stat='identity', fill=rainbow(25))
p6 <- p6 + theme(axis.text.x=element_text(angle=90, hjust=1))
print(p6)

# 서울시 2015년 12월 각 자치구 별
# 교통사고 현황(자치구명/사망자수)을 출력
accid_2015_12 <- subset(accid_2015, 연도==2015 & 월==12)
accid_2015_12

p7 <- ggplot(accid_2015_12, aes(자치구명, 사망자수))
p7 <- p7 + geom_bar(aes(fill=자치구명), stat='identity')
p7 <- p7 + theme(axis.text.x=element_text(angle=90, hjust=1))
print(p7)

p7 <- ggplot(accid_2015_12, aes(자치구명, 사망자수))
p7 <- p7 + geom_bar(stat='identity', fill=rainbow(25))
p7 <- p7 + theme(axis.text.x=element_text(angle=90, hjust=1))
print(p7)

# 원 그래프 : geom_bar + coord_polar()
# 차량별 실린더수 현황 히스토그램
# ---------------------------------------
gg9 <- ggplot(mtcars, aes(cyl))
gg9 <- gg9 + geom_bar()
print(gg9)
# ---------------------------------------
gg9 <- gg9 + coord_flip() # 수평막대
print(gg9)

gg9b <- gg9b + coord_polar() # 원그래프
print(gg9b)

gg10 <- ggplot(mtcars, aes(factor(1), fill=fcyl))
gg10 <- gg10 + geom_bar(width=1)
gg10 <- gg10 + coord_polar(theta = 'y')
print(gg10)

# 으으음...
df <- data.frame(
  group = c('Male', 'Female', 'Child'),
  age = c(25, 27, 5) )
head(df)

# 비추
bc <- ggplot(df, aes('', age, fill=group))
bc <- bc + geom_bar(stat='identity')
bc <- bc + coord_polar(theta='y') # 임의의 각도 사용
print(bc)

# heat map : heatmap()
# 데이터가 행렬 객체를 사용함
# ------------------------------------------------------------
# 서울시 2015년 12월 각 자치구 별
# 교통사고 현황(자치구명/발생건수)을 출력
# 막대그래프 geom_bar 로 작성
accid_2015_12 <- subset(accid_2015, 연도==2015 & 월==12)
accid_2015_12

p5 <- ggplot(accid_2015_12, aes(자치구명, 발생건수))
# p5 <- p5 + geom_bar(aes(fill=rainbow(25)), stat='identity')
# 자치구명 대신 색깔로 나옴
# p5 <- p5 + geom_bar(fill='red', stat='identity')
p5 <- p5 + geom_bar(aes(fill=자치구명), stat='identity') # 자치구명 개수대로 색을 넣음
p5 <- p5 + theme(axis.text.x=element_text(angle=90, hjust=1)) # hjust=1 중간
print(p5)
# ------------------------------------------------------------
accid_2015_12 # 문자와 숫자가 같이 있음
row.names(accid_2015_12) <- accid_2015_12$자치구명
# accid_2015_12의 각 행의 이름을 지정
mtx_acc_2015_12 <- accid_2015_12[, c(4:6)]
# accid_2015_12에서 발생건수, 사망자수, 부상자수 추출
mtx_acc_2015_12 <- data.matrix(mtx_acc_2015_12)
# 발생건수, 부상자수 데이터를 행렬로 변환
mtx_acc_2015_12

heatmap(mtx_acc_2015_12, col=cm.colors(128),
        Rowv=NA, Colv=NA, scale='column',
        cexCol=1, margin=c(5, 5))

# mtcars
head(mtcars)
mmtcars <- as.matrix(mtcars)
head(mmtcars)

heatmap(mmtcars) # 기본 열그래프 작성
heatmap(mmtcars, scale='column') # 정규화 작업후 다시그림
# 유사성을 나타내기 위해 군집화 실행
heatmap(mmtcars, scale='column',
        Colv=NA, Rowv=NA) # 쓸때없는 막대 제거(군집화 제거)
heatmap(mmtcars, scale='column',
        Colv=NA, Rowv=NA, col=rainbow(256)) # 색상지정1
heatmap(mmtcars, scale='column',
        Colv=NA, Rowv=NA, col=terrain.colors(256)) # 색상지정2

# 버블차트
# 2015년 12월 서울 각 자치구 별
# 교통사고 현황(발생건수/사망자수)을 출력
gg11 <- ggplot(accid_2015_12, aes(발생건수, 사망자수))
# gg11 <- gg11 + geom_point(aes(size=사망자수),
#                           shape=16, color='blue', alpha=0.45)
gg11 <- gg11 + geom_point(aes(size=사망자수),
                          shape=16, alpha=0.45)
# gg11 <- gg11 + scale_fill_brewer(palette='Set1')
# gg11 <- gg11 + scale_fill_brewer(palette='Paired')
gg11 <- gg11 + scale_fill_brewer(palette='Blues') # 팔레트 적용 안됨
print(gg11)

# scale_fill_brewer 에서 지원하는 색상 팔레트 조회
RColorBrewer::display.brewer.all()

# Cars93 데이터 이용 - 도시, 고속도로별 연비
library(MASS)
head(Cars93)

ggplot(Cars93, aes(Weight, MPG.highway)) +
#  geom_point(size=6, shape=21) # 기본
#  geom_point(aes(size=MPG.highway), shape=21) # 사이즈 지정
#  geom_point(aes(size=MPG.highway), shape=21,
#             colour='red') # 색상지정
  geom_point(aes(size=MPG.highway), shape=21,
             fill='red') # 내부채우기

# ggplot(Cars93, aes(Weight, MPG.highway, fill=Price)) +
#   # 연속형 변수값에 따라 색깔 변화
ggplot(Cars93, aes(Weight, MPG.highway, fill=Cylinders)) +
  # 연속형 변수Cylinders에 따른 색깔 변화
  geom_point(size=5, shape=21, colour='red') +
  # scale_fill_brewer(palette='Oranges')
  # # 색깔 지정을 미리 정의된 팔레드 이용
  # scale_fill_brewer(palette='Set1')
  scale_fill_brewer(palette='Paired')

# 공간지도 - 지도에 통계관련 자료를 표시
head(accid_2015)

install.packages('ggmap')
install.packages('maps')
install.packages('mapproj')

library(ggmap)
library(maps)
library(mapproj)

# seoulmp <- get_googlemap('seoul')
# seoulmp <- get_googlemap('seoul', maptype='satellite')
seoulmp <- get_googlemap('seoul', maptype='roadmap', zoom=12)
# seoulmp <- get_googlemap('seoul', maptype='hybrid')
# seoulmp <- get_googlemap('seoul', maptype='terrain')
seoul <- ggmap(seoulmp)
print(seoul)

# ------------------------------------------------------
accid_2015 <- read.csv(
  'c:/Java/seoul_car_acci_v1.txt', sep=',')
head(accid_2015) # 자치구별 2015년 교통사고 현황

accid_2015_12 <- subset(accid_2015, 연도==2015 & 월==12)
# ------------------------------------------------------
head(accid_2015_12)
seoul_lat_lon <- read.csv('c:/Java/seoul_lat_lon.txt')
head(seoul_lat_lon)

# 교통사고 정보와 구별 위치정보를 합침
accid_2015_12 <- merge(accid_2015_12, seoul_lat_lon,
                       by.x='자치구명', by.y='area')

head(accid_2015_12)

# 맵에 자치구 위치를 점으로 표시
seoulmp <- get_googlemap('seoul', maptype='roadmap', zoom=12)
gmp <- ggmap(seoulmp)
# gmp <- gmp + geom_point(data=accid_2015_12, aes(lon, lat))
gmp <- gmp + geom_point(data=accid_2015_12, aes(lon, lat, size=발생건수),
                        shape=17, color='blue', alpha=.5) # x를 마음에 안들어함
gmp <- gmp + geom_text(data=accid_2015_12, aes(label=자치구명), size=3, hjust=1.2, fontface='bold')
# gmp <- gmp + geom_path(data=accid_2015_12, aes(x=lon, y=lat), color='red', alpha=.5, lwd=1)
print(gmp)

