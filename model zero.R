getwd()
# setwd("C:\\Users\\Admin\\Desktop\\endstart")

life = read.csv("life2.csv")

head(life)
class(life) ; dim(life)

# 믹스 0  수컷 0  중성화안함 0  강아지 0  입양 1

colnames(life)[2] = 'catdog'

colnames(life)

usecol = c(1:7, 10)
usecol
life0 = life[ , usecol]

head(life0)

attach(life0)

positi = readLines("pos_pol_word.txt", encoding = 'UTF-8')
head(positi)
negati = readLines("neg_pol_word.txt", encoding = 'UTF-8')
is.vector(negati)

emotion = function(sentences, positive, negative) {
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence) # 숫자 제거
    
    word.list = str_split(sentence, '\\s+') # 공백기준으로 단어생성 \\s+ :공백 정규식, + :(1개 이상)
    words = unlist(word.list) 
    
    pos.matches = match(words, positive) # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
    
  }, positive, negative)
  
  scores.df = data.frame(score = scores, text = sentences)
  return(scores.df)
  
}

library(plyr)
library(stringr)

featureScore = emotion(feature, positi, negati)

dim(featureScore)
summary(featureScore)

length(feature)

life0 = life0[ ,-8]
summary(life0)
life1 = cbind(life0, featureScore$score)
summary(life1)
colnames(life1)[7:8] = c("location", "featurescore")
colnames(life1)
summary(life1)


feascore = as.data.frame(life1$featurescore)
feascore$remark[life1$featurescore > 0] = '긍정'
feascore$remark[life1$featurescore == 0] = '중립'
feascore$remark[life1$featurescore < 0] = '부정'

score_table = table(feascore$remark)
score_table

pie(score_table, main = '유기동물 특징기입란 긍부정 결과',
    col = c('blue', 'green', 'yellow'), radius=1.0)


hist(life1$featurescore)

attach(life1)
summary(state)
plot(life1)
plot(age, state) ; plot(kind, state) ; plot(featurescore, state)
dim(life1) 
sum(state)/4695
summary(life1)

catdog = as.factor(catdog) ; kind = as.factor(kind) ; sex = as.factor(sex)
neuter = as.factor(neuter) ; location = as.factor(location)
summary(life1)

obj = glm(state ~ . ,data = life1, family = binomial)
summary(obj)
plot(obj)

obj1 = glm(state ~ .-location, data = life1, family = binomial)
summary(obj1)
plot(obj1)

life2 = cbind(life1, feascore$remark)
colnames(life2)
colnames(life2)[9] = 'featuredirec'

attach(life2)
obj2 = glm(state ~. -location -featurescore, data = life2, family = binomial)
summary(obj2)
