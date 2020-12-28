install.packages("readxl")
library("readxl")

dataset <- read_excel("sat.xls")
head(dataset)
str(dataset)
plot(dataset$comp_GPA, dataset$univ_GPA, xlab="Computer Science GPA", ylab="University GPA")

plot(dataset$comp_GPA, dataset$univ_GPA, xlab="Computer Science GPA", ylab="University GPA")
abline(lm(dataset$comp_GPA~dataset$univ_GPA), col="red")

cor(dataset$comp_GPA,dataset$univ_GPA, method="pearson")

data.cor = cor(dataset)
data.cor

install.packages("corrplot")
library(corrplot)

corrplot(data.cor)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = data.cor, col = palette, symm = TRUE)