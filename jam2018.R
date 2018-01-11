# Ratemyprofessor
library(alr4)
str(Rateprof)
dat = Rateprof[, 1:12]

# add a variable that does not get additional information
# low variance  (not informative)
# Extreme example, they are all the same
dat = cbind(dat, 'Midwest') 
names(dat)[13] = 'area'
dat = dat[, -7]
# high correlation, extreme examples, the same thing but rescaled. 
pairs(dat)

dat1 = dat[sample( (1:nrow(dat)), nrow(dat)), ]
train.id = sample((1:nrow(dat1)), 290)
dat1.train = dat1[train.id, ]
dat1.test = dat1[-train.id, ]

write.csv(dat1.train, '/Users/zhuozhihuang/Desktop/JAM/prof_train.csv')
write.csv(dat1.test, '/Users/zhuozhihuang/Desktop/JAM/prof_test.csv')


########################################################
dat.train = read.csv('/Users/zhuozhihuang/Desktop/JAM/prof_train.csv')
dat.test = read.csv('/Users/zhuozhihuang/Desktop/JAM/prof_test.csv', 
                    stringsAsFactors = F)

# remove area
dat1.train = dat.train[, -c(1, 13)]

# helpful visualization for detecting high correlations between attributes
pairs(dat1.train)
pairs(dat1.train[, 7:11])

library(GGally)
library(ggplot2)
ggpairs(dat1.train[, 7:11])


# simple model 1, use only helpfulness
mod0 = lm(quality ~ helpfulness, data = dat1.train)
summary(mod0)

# PCA in RateMyProf
dat2.train = dat1.train[, c(7:11)]
pca2 = prcomp(dat2.train[, -1])
pca2.1 = prcomp(dat2.train[, -1], center = T, scale = T)
# the plot shows the informativeness of each Principle Components
plot(pca2)
plot(pca2.1)  # similar results

pca2$x

plot(x = pca2.1$x[, 1], y = dat2.train$quality)


cor(pca2.1$x[, 1], dat2.train$quality)

dim(pca2.1$x)

# only need to choose the first PC



predict()

# variable reduction
mod1 = lm(quality ~ gender + numYears + numRaters + numCourses
          + pepper + discipline + helpfulness + clarity + easiness 
          + raterInterest, data = dat1.train)
b1 =step(mod1)  # backward elimination
b1.coef = b1$coefficients

# variable extraction


# A random bitmap
d = 28
library(imager)
matrix(sample(c(0, 1), d*d, replace = T), nrow = d, ncol = d) %>% as.cimg %>% plot


pairs(rp)


# iris
iris = read.csv('/Users/zhuozhihuang/Desktop/JAM/iris.txt', header = F) 
# V1, V2, V3, V4, V5 (y)

library(reshape2)
iris2 = melt(iris, id.vars = c('V1', 'V2', 'V3', 'V4') )

library(ggplot2)
g = ggplot(data = iris)
g + geom_point(aes(x = V1, y = V2, color = V5))
g + geom_point(aes(x = V1, y = V3, color = V5))
g + geom_point(aes(x = V1, y = V4, color = V5))
g + geom_point(aes(x = V3, y = V2, color = V5))
g + geom_point(aes(x = V4, y = V2, color = V5))
g + geom_point(aes(x = V3, y = V4, color = V5))

# PCA
p1 = prcomp(iris[, 1:4],center = T, scale = T)

iris1 =as.matrix(iris[, 1:4]) %*%  as.matrix(p1$rotation)
iris11 = cbind(iris1, as.character(iris$V5)) %>% data.frame
iris11$PC1 = as.numeric(iris11$PC1)
iris11$PC2 = as.numeric(iris11$PC2)
iris11$PC3 = as.numeric(iris11$PC3)
iris11$PC4 = as.numeric(iris11$PC4)

plot(p1,type = 'l')
g1 = ggplot(data = iris11)
g1 + geom_point(aes(x = PC1, y = PC2, color = V5))


# most spread out, but doesn't help with classification
library(MASS)


# jam

library(devtools)
install_github('ramhiser/datamicroarray')
library(datamicroarray)


b = data('borovecki')  #2005
data('borovecki', package = 'datamicroarray')
b = borovecki
b1 = b[[1]]
b2 = b[[2]]


# 17 symptomatic, 14 control
train.id = sample((c(sample(1:17, 14), sample(18:31, 11))), 25)
test.id = (1:31)[-train.id]


# PCA
pca = prcomp(b1[train.id, ], center = TRUE, scale = TRUE) 
plot(pca, type = "l")
summary(pca)

library(ggplot2)

# LDA


x = matrix(1:16, nrow = 4, ncol = 4)
hist(x)


# digits dataset, (1797, 65) ----------------------------

dig = read.csv('/Users/zhuozhihuang/Desktop/JAM/digits.csv', header = F) 
dig.y = dig[, 65]  # 0, 1, 2,... 9
dig.x = 1 - as.matrix(dig[, 1:64])/16

# draw image in R
mat = dig.x[1, ] %>% matrix(nrow = 8, ncol = 8)
for (i in 2:400){
  mat = dig.x[i, ] %>% matrix(nrow = 8, ncol = 8) %>% cbind(mat)
}

mat.n = mat[, 1:160]
for (j in 2:20){
  mat.n = rbind(mat.n, mat[, (160*(j-1) + 1) : (160*j)])
}

library(imager)
mat.n %>% as.cimg %>% plot(xaxt='n', yaxt = 'n', ann=FALSE)

# pca
pca = prcomp(dig.x)
print(pca)

pca1 = prcomp(t(dig.x), center = T, scale = T)
rot1 = pca1$rotation
rot1[1, ] %>% matrix(nrow = 8, ncol = 8) %>% as.cimg %>% plot
rot1[2, ] %>% matrix(nrow = 8, ncol = 8) %>% as.cimg %>% plot
rot1[3, ] %>% matrix(nrow = 8, ncol = 8) %>% as.cimg %>% plot

# par(mar = rep(2, 4))
plot(pca1, type = 'l')

# Fashion MNIST data
fas = read.csv('/Users/zhuozhihuang/Desktop/JAM/fashion-mnist_train.csv')

fas.x = as.matrix(fas[, -1])
fas.y = fas[, 1]

# Example images
fas.x[3, ] %>% matrix(nrow = 28, ncol = 28) %>% as.cimg %>% plot

pca2 = prcomp(t(fas.x), center = T, scale = T)
plot(pca2, type = 'l')
pca2$rotation[4, ] %>% matrix(nrow = 28, ncol = 28) %>% as.cimg %>% plot
