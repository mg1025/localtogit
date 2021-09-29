# install.packages("languageserver")
library(languageserver)

getwd()
# setwd("C:\\Users\\Admin\\Desktop\\endstart")

life = read.csv("life2.csv")
colnames(life)[2] = 'catdog'
usecol = c(1:7, 10)
life0 = life[ , usecol]

attach(life0)
positi = readLines("pos_pol_word.txt", encoding = 'UTF-8')
negati = readLines("neg_pol_word.txt", encoding = 'UTF-8')

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

life0 = life0[ ,-8]
life1 = cbind(life0, featureScore$score)
colnames(life1)[7:8] = c("location", "featurescore")
attach(life1)

catdog = as.factor(catdog) ; kind = as.factor(kind) ; sex = as.factor(sex)
neuter = as.factor(neuter) ; location = as.factor(location)

train = sort(sample(1:nrow(life1), 0.6*nrow(life1)))
test = setdiff(1:nrow(life1), train)

obj.train = glm(state ~ . , data = life1, subset = train, family = binomial)
result = summary(obj.train)
phat = predict.glm(obj.train, newdata = life1[test, ], type = "response")
statehat = ifelse(phat>=0.5, 1, 0)

tt = table(statereal = life1$state[test], statehat = statehat)
tt
# install.packages("Epi")
library(Epi)
gof = ROC(test = statehat, stat = life1$state[test], plot = "ROC", AUC = TRUE,
          main = "logistic regression")

print(result)
print(gof)

write.csv(tt, 'Accura table.csv')


