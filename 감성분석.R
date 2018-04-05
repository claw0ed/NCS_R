# 감성 분석

# 특정 대상에 대하여 소비자가 느끼는 감성(긍정, 부정)과
# 그에 대한 이유를 분석

# 감성 분석은 사전이 얼마나 많은 양의 정제된 감성어를
# 포함하고 있는지에 따라 분석의 품질이 달라질 수 있음

# 감성 분석 사전을 구축하기 위해서는 다양한 머신러닝
# 기법을 통해 구축할 수 있으나, 정확성을 높이기 위해서는
# 사람이 직·간접적으로 참여하는 과정이 필요

install.packages('twitteR')
install.packages('ROAuth')
install.packages('plyr')
install.packages('stringr')

# 패키지 불러오기
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)

# 트위터API를 이용해서 애플, 삼성에 대한 트윗을 수집
load("C:/Java/apple.RData")
load("C:/Java/samsung.RData")

# 총 트윗 수 출력
#length(apple.tweets)
length(samsung.tweets)

# 트윗에 자료형 파악
#class(apple.tweets) # list 로 출력
class(samsung.tweets)

# 1-5행까지의 트윗 출력
#apple.tweets[1:5]
samsung.tweets[1:5]

# 첫번째 트윗의 상세정보 출력
#tweet<-apple.tweets[[1]]
tweet<-samsung.tweets[[1]]
tweet$getScreenName() # 작성자
tweet$getText()       # 트윗(본문)

# 모든 트윗에서 본문글만 출력하는 함수 작성
#apple.text<-lapply(apple.tweets,function(t){t$getText()}) # lapply, l은 리스
samsung.text<-lapply(samsung.tweets,function(t){t$getText()}) # lapply, l은 리스

# lapply 실행 결과 중에서 3행까지 출력
#head(apple.text,3)
head(samsung.text,3)

# 감성분석을 위한 긍정/부정 사전을 불러옴
pos.word=scan("c:/Java/positive-words.txt",what="character",comment.char=";")
neg.word=scan("c:/Java/negative-words.txt",what="character",comment.char=";")

# 기존 긍정/부정 사전에 새로운 단어 추가
pos.words<-c(pos.word,"upgrade")
neg.words<-c(neg.word,"wait","waiting")

# 트윗 내용을 바탕으로 감성기반 분석후 점수 출력
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # 관련 패키지 추가 및 등록
  require(plyr)
  require(stringr)
  
  # 감성기반 분석후 점수 산출하는 함수
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    # 의미없는 단어(문장부호, 기호, 숫자등) 제거
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence) # 리트윗 횟수
    
    # 소문자로 변환 후 문장을 단어로 분리
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    
    # 분리된 단어와 긍정/부정 단어 간 비교
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    # 점수계산 = 긍정일치수 - 부정일치수
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  # 각 단어와 점수를 dataframe 형태로 저장
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

# apple 에 대한 트윗을 기반으로 감성분석 후 점수 출력
#apple.scores=score.sentiment(apple.text,pos.words,neg.words,.progress='text')
samsung.scores=score.sentiment(samsung.text,pos.words,neg.words,.progress='text')

# 감성분석 결과를 그래프로 출력
# 0보다 클수록 긍정의 의미가 높음
#hist(apple.scores$score)
hist(samsung.scores$score)
