data(iris)
summary(iris)
library(dplyr)
# 표준화
iris.std = scale(iris[,-5]) %>% as.data.frame()
fix(iris.std)

# 센트로이드 3 (NbClust 생략)
iris.k = kmeans(iris.std, centers = 3, iter.max = 1000)
iris.k
table(iris$Species, iris.k$cluster)

# 시각화
cluster <- as.factor(iris.k$cluster)
qplot(Petal.Width, Petal.Length, colour = cluster, data = iris.std)
qplot(Sepal.Width, Sepal.Length, colour = cluster, data = iris.std)
