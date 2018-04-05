# 데이터 마이닝(data mining)
# 구조적인 데이터(정형 데이터)를 대상으로
# 유용하고 가치 있는 패턴을 추출

# 텍스트 마이닝(text mining)
# 자연어로 구성된 비구조적인 데이터(비정형 데이터)를
# 대상으로 개체명(인명, 지역명 등), 패턴 혹은
# 단어-문장 관계 정보를 추출

# 형태소
# 의미가 있는 최소의 단위로서 더 이상 분리가
# 불가능한 가장 작은 의미 요소

# 형태소 분석
# 주어진 단어 또는 어절을 구성하는 각 형태소를
# 분리한 후 분리된 형태소의 기본형 및 품사 정보를 추출

# 텍스트 마이닝 패키지 설치
install.packages('tm')
library(tm)

# 텍스트 파일 읽기
inputText <- readLines('c:/Java/output2.txt')
inputText


# 텍스트 마이닝 작업을 위해 코퍼스(corpus) 형으로 변환
corpus <- Corpus( VectorSource( inputText ) )

# 문서 전처리 과정 수행
# 대소문자 변환, 숫자, 문장부호, 공백 제거 수행
docs <- tm_map(corpus, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation) # 문장기호
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, c('New Jersey')) # 특정단어

print(docs)
summary(docs) # 코퍼스 자료 요약정보 출력
inspect(docs) # 코퍼스에 저장된 본문 출력
inspect(docs[1:3]) # 코퍼스에 저장된 문장 출력

strwrap(docs[1]) # 코퍼스에 저장된 문장 출력

# 전처리된 문서들을 행렬로 변환
dtm <- TermDocumentMatrix(docs) # 단어/문서 행렬 생성
dtm <- as.matrix(dtm)

dtm

freq <- sort(rowSums(dtm), decreasing=T) # 출현빈도에 따라 정렬
freq[1:10]

barplot(freq[1:10]) # 출현빈도에 따라 그래프 출력

# 불용어 제거
# 입력 문서를 이루는 단어 성분 중에는 문서의
# 정보(의미)를 표현하지 못하는 단어, 즉 문서와
# 관련성이 없는 것으로 간주하는 단어

# 불용어가 아닌 단어들이 '가용어'라 부름
# 특히 가용어 중에서도 문서의 중심이 되는 주제어:키워드
# 일반적으로 문서 내에서 발생 빈도가 높은 단어들을 선정

docs <- tm_map(docs, removeWords, stopwords('english'))
dtm2 <- TermDocumentMatrix(docs)
dtm2 <- as.matrix(dtm2)
freq2 <- sort(rowSums(dtm2), decreasing=T)
freq2[1:10]
barplot(freq2[1:10]) # 출현빈도에 따라 그래프 출력
