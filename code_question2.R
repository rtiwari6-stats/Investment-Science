#code for question 2

rm(list=ls())

setwd("~/UW/CFRM-501/HW7")

data = read.csv("us_treasury_yields.csv", header=T)
data = data[, !(names(data) %in% c("Date"))]

# compute X
x = data[2:nrow(data),] - data[1:nrow(data)-1,]

#compute pca
x.pca = prcomp(x)

y1 = x.pca$rotation[,1]
y2 = x.pca$rotation[,2]
y3 = (-1)*x.pca$rotation[,3]

times = c(1,2,3,5,7,10,20,30)

plot(times,y1, main = "Plot of three factor loadings (betas)", xlab = "T", ylab = "Betas",  
     col="blue", pch = 19, ylim = c(-1,1.2))
points(times,y2,  col="green", pch = 19)
points(times,y3,  col="red", pch = 19)

legend("bottomright", legend = c("Beta1", "Beta2", "Beta3"), col = c("blue", "green", "red"), pch = c(19,19,19))