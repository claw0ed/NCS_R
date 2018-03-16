# KNN
# iris 자료를 분석하기 위해 KNN 사용

data(iris)
head(iris, n=10) # 10개 행 출력

# iris 데이터 집합을 train, test 데이터로 나눔
set.seed(9563) # 난수 생성
idx <- sample(nrow(iris), size=nrow(iris)*0.75)
train <- iris[idx, ]
test <- iris[-idx, ]

# 데이터 집합 갯수와 차원 확인
dim(train)
dim(test)
table(train$Species) # 빈도
table(test$Species)

# KNN 알고리즘 적용
library(class)
# knn <- knn(train[, 1:4], test[, 1:4], train$Species, k=1, prob=F)
knn1 <- knn(train[, 1:4], test[, 1:4], train$Species, k=1, prob=T)
knn3 <- knn(train[, 1:4], test[, 1:4], train$Species, k=3, prob=T)
knn1
knn3

# 예측값 평가
knn1conf <- table(knn1, test$Species)
knn1acc <- sum(diag(knn1conf)) / nrow(test)
knn1acc

knn3conf <- table(knn3, test$Species)
knn3acc <- sum(diag(knn3conf)) / nrow(test)
knn3acc


# 누구와 소개팅 하면 좋을까? - 결혼정보 시스템 짝 이어주기
isLike <- read.csv('c:/Java/like.csv')
colnames(isLike) <- c('talk', 'book', 'trip', 'hakjum', 'height', 'skin', 'muscle', 'like')
set.seed(180315)
idx <- sample(nrow(isLike), size=nrow(isLike)*0.7)
train <- isLike[idx, ]
test <- isLike[-idx, ]

train
test

knn1 <- knn(train[, 1:7], test[, 1:7], train$like, k=1, prob=T)
knn3 <- knn(train[, 1:7], test[, 1:7], train$like, k=3, prob=T)

knn1
knn3

knn1conf <- table(knn1, test$like)
knn1conf
knn1acc <- sum(diag(knn1conf)) / nrow(test)
knn1acc
knn3conf <- table(knn3, test$like)
knn3conf
knn3acc <- sum(diag(knn3conf)) / nrow(test)
knn3acc


# 누가 이상품을 구매할까? - 매장 방문고객 타겟마케팅
isBuy <- read.csv('c:/Java/buy.csv')

colnames(isBuy) <- c('age', 'income', 'buy')

# 표준화 작업 : 나이와 월수입 단위가 다름
isBuy$scage <- scale(isBuy$age)
isBuy$scincome <- scale(isBuy$income)

head(isBuy)

# 데이터를 train/test 로 나누지 않고 학습
train <- isBuy[, 4:5]
# test <- data.frame(scage=2.1, scincome=2.3)
test <- data.frame(age=34, income=450)
test$scage <- (test$age - mean(isBuy$age)) / sd(isBuy$age) # sd 표준편차
test$scincome <- (test$income - mean(isBuy$income)) / sd(isBuy$income)

test <- test[, 1:2]
labels <- isBuy[, 3]

knn1 <- knn(train, test, labels, k=1, prob=T)
knn1

knn3 <- knn(train, test, labels, k=3, prob=T)
knn3

knn5 <- knn(train, test, labels, k=5, prob=T)
knn5

