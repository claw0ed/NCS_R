# 나이브베이즈

# iris 데이터 집합을 이용해서 베이즈 정리로 분류 분석
# 데이터를 학습/평가 데이터로 분리
library(ggplot2)
library(lattice)
install.packages('caret')
library(caret) # ggplot2, lattice 필요
idx <- createDataPartition(iris$Species, p=0.7, list=F)
idx
# 7:3 비율로 데이터 학습/평가로 분리

train <- iris[idx, ]
test <- iris[-idx, ]

table(train$Species)
table(test$Species)

# 베이즈 이론으로 조건부 확률 계산후 적용
install.packages('e1071')
library(e1071)
result <- naiveBayes(train, train$Species, laplace=1)
pred <- predict(result, test, type='class')
table(pred, test$Species)

result
pred

# 베이즈 이론 적용 예측값 평가
confusionMatrix(pred, test$Species)

# 영화마케팅 문제를 베이즈 정리로 해결
# 영화관객의 성향을 설문조사로 정리
# 관객의 속성으로 영화 취양을 파악해보자

# 사전확률 P(B), P(A)
# P(B) : 20대, 여, 디자이너, 미혼, 애인있음
# P(A) : 로맨틱 영화를 선택할 확률
# 주변확률 : P(B|A)
# 조건부확률 : P(A|B)

# 사전확률 :
# P(B) : 20대, 여, IT, 미혼, 애인없음
# P(A) : 공포 영화를 선택할 확률
# 주변확률 : P(B|A)
# 조건부확률 : P(A|B)
# P(B|A) = P(20대, 여, IT, 미혼, 애인없음 | 공포) = 
# P(20대 | 공포) * P(여 | 공포) * P(IT | 공포) *
# P(미혼 | 공포) * P(애인없음 | 공포)
movie <- read.csv('c:/Java/movie.csv', header=T)
movie
nbmovie <- naiveBayes(movie[1:5], movie$장르, laplace=1)
head(nbmovie)

result <- predict(nbmovie, movie[1:5])
sum(movie$장르 != result)

result

table(result, movie$장르)
confusionMatrix(result, movie$장르) # 혼돈행렬 출력

# 사내메일중 스팸메일 가려내기
# 메일에 특정 키워드가 있는 경우 스팸으로 처리

# 사전확률 :
# P(B) : '특강' 이라는 단어가 포함될 확률
# P(A) : 메일이 스팸일 확률
# 주변확률 : P(B|A)
# 조건부확률 : P(A|B)
mail <- read.csv('c:/Java/spam.csv', header=T)
mail
nbmail <- naiveBayes(mail[2:13], mail$메일종류, laplace=1)
head(nbmail)

result2 <- predict(nbmail, mail[2:13])
sum(mail$메일종류 != result2) # 비일치 항목수 출력
table(result2, mail$메일종류)

confusionMatrix(result2, mail$메일종류) # 혼돈행렬 출력
result2

# 지원자
지원자 <- read.csv('c:/Java/지원자.csv', header=T)
지원자
nb지원자 <- naiveBayes(지원자[1:4], 지원자$합격여부, laplace=1)
head(nb지원자)

result3 <- predict(nb지원자, 지원자[1:4])
sum(지원자$합격여부 != result3) # 비일치 항목수 출력
table(result3, 지원자$합격여부)

confusionMatrix(result3, 지원자$합격여부) # 혼돈행렬 출력
result3

# 입사지원시 조건에 따른 합격여부 판별
# 나이, 장래희망유무, 인터뷰태도, 고교성적,??? 합격여부

# 사전확률 :
# P(B) : 나이, 장래희망유무, 인터뷰태도, 고교성적
# P(A) : 합격여부 확률
# 주변확률 : P(B|A)
# 조건부확률 : P(A|B)

# 테스트 데이터 : 적음, 없음, 매우좋음, 보통

# P(합격여부=합격) : 13/20
# P(합격여부=불합격) : 7/20

# P(나이=적음|합격) : 4/13
# P(나이=적음|불합격) : 3/7
# P(장래희망유무=없음|합격) : 1/13
# P(장래희망유무=없음|불합격) : 7/7
# P(인터뷰태도=매우좋음|합격) : 3/13
# P(인터뷰태도=매우좋음|불합격) : 1/7
# P(고교성적=보통|합격) : 2/13
# P(고교성적=보통|불합격) : 3/7

# P(적음,없음,매우좋음,보통|합격)
# = (13/20 x 4/13 x 1/13 x 3/13 x 2/13) / P(?)
# (13/20) * (4/13) * (1/13) * (3/13) * (2/13) #/ P(?)
# = 0.0005 (0.00054619936)

# P(적음,없음,매우좋음,보통|불합격)
# (7/20) * (3/7) * (7/7) * (1/7) * (3/7) #/ P(?)
# = 0.0091 (0.00918367346)

# P(나이=적음) 
# = 7/20 = 0.35
# P(장래희망유무=없음)
# = 8/20 = 0.4
# P(인터뷰태도=매우좋음)
# = 4/20 = 0.2
# P(고교성적=보통)
# = 5/20 = 0.25

# P(A|B) = P(A)P(B|A)/P(B)
# P(적음,없음,매우좋음,보통|합격) =
# (0.0005 + 0.0092) / 0.0005

# P(적음,없음,매우좋음,보통|불합격) =
# (0.0005 + 0.0092) / 0.0092

