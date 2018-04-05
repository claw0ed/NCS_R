# 형태소분석
# https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md

install.packages('devtools')
library(devtools)

install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP') # 한글 형태소 분석 지원
install.packages('dplyr')

library(memoise)
library(KoNLP)
library(dplyr)
library(stringr)

Sys.setenv(JAVA_HOME='C:/Java/jdk1.8.0_152')

# 형태소 분석을 위한 참고사전 필요
# 분석할 문장에 포함된 단어들이
# 사전에 알맞는 형태(품사)로 정의되어야 정확한 분석 가능
# KoNLP에는 세종 사전이 포함되어 있음
# useNIADic()
useSejongDic()

install_github('haven-jeon/NIADic/NIADic', build_vignettes=T)

library(NIADic)
sejong <- get_dic('sejong') # 세종사전 적재
head(sejong)

# 형태소 분리 : 명사 추출
sentence <- "아버지 가방에 들어가신다"
extractNoun(sentence)

# SimplePos09 : 품사의 가장 상위 분류인 9개의 품사로 분류
# SimplePos22 : 22개의 픔사로 분류
# MorphAnalyzer : 가장 상세하게 품사를 분류
SimplePos09(sentence)
SimplePos22(sentence)
MorphAnalyzer(sentence)

sentence <- "분석할 문장에 포함된 단어들이 사전에 알맞는 형태(품사)로 정의되어야 정확한 분석이 가능"
extractNoun(sentence)

# KAIST 품사 태그 집합
# N : 체언 (명사, 수사)
# P : 용언 (동사, 형용사)
# M : 수식언 (관형사, 부사)

# 트윗글 읽어오기
tweets <- readLines('c:/Java/speech.txt') # , encoding='UTF-8'
head(tweets, 3) # class(tweets)

# 명사 추출 : extractNoun
docs <- str_replace_all(tweets, "\\W", " ") # 공백처리

# 불용어 처리 (공백, 특수문자등등)
docs <- str_replace_all(tweets, "\\[", "")
docs <- str_replace_all(tweets, "\\[ㄱ-ㅎ]", "") # ㅋㅋ
docs <- str_replace_all(tweets, "\\[0-9]", "")
docs <- str_replace_all(tweets, "\\[a-zA-Z]", "")
docs <- str_replace_all(tweets, "\\[ㅜ|ㅠ]", "")
docs <- str_replace_all(tweets, "\\[~!@#$%^&*()_-+=?<>]", "")

nouns <- extractNoun(docs) # 명사추출
nouns # 결과는 리스트 형태

# 추출된 명사를 기반으로 워드카운트
wordcount <- table(unlist(nouns))
# 추출된 명사 리스트를 벡터로 변환, 빈도표로 생성

df_word <- as.data.frame(wordcount, stringsAsFactor=F)
# 테이블로 생성된 단어들을 데이터 프레임으로 생성
df_word

df_word <- rename(df_word, word=Var1, freq=Freq)
# 데이터 프레임의 각 열이름 수정

df_word$word <- as.character(df_word$word)
df_word <- filter(df_word, nchar(word) >= 2)
# 2글자 이상의 단어만 걸러냄

# dplyr 패키지를 이용해서 작업
# df_word 의 내용을 정렬한 뒤 상위 20개만 추려냄
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)

top20

# 워드클라우드
install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer) # 색상표 초기화 (화려하게 출력)

pal <- brewer.pal(8, "Dark2") # Dark2 섹상 중 8개 색 추출

wordcloud(word = df_word$word, # 단어
          freq = df_word$freq,  # 빈도
          min.freq = 2,        # 최소 단어 빈도
          max.words = 250,     # 표현 단어수
          random.order = F,    # 고빈도 중앙배치여부
          rot.per = .1,        # 회전 단어 비율
          scale = c(4, 0.3),   # 빈도별 확대 비율
          colors = pal)        # 표현 색상

