library(ISLR)
rm(list=ls())

head(Default)
tail(Default)

str(Default) #to display type of variables

# ===== LINEARLY SEPARABLE CASE =====
# Menggambarkan titik
set.seed(10)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep (1,10))
x[y==1,] <- x[y==1, ] + 5/2 # to make them completely
dat = data.frame(x=x, y=as.factor(y))
dat

library(ggplot2)
ggplot(data=dat, aes(x=x.2, y=x.1, color=y, shape=y))+
  geom_point(size = 2)+
  scale_color_manual(values = c("#000000", "#FF0000"))+
  theme(legend.position = "none")

# Pemodelan kernel linear
library(e1071)
svmfit = svm(y~., data=dat, kernel="linear", scale = FALSE)
plot(svmfit,dat)

# Pemodelan kernel vanilladot
library(kernlab)
kernfit = ksvm(x, y, type="C-svc", kernel="vanilladot")
plot(kernfit, data=x)

# ===== LINEARLY SEPARABLE CASE =====
# Menggambarkan titik
set.seed(10)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep (1,10))
x[y==1,] <- x[y==1, ] + 0,5 # to make them completely
dat = data.frame(x=x, y=as.factor(y))
dat

library(ggplot2)
ggplot(data=dat, aes(x=x.2, y=x.1, color=y, shape=y))+
  geom_point(size = 2)+
  scale_color_manual(values = c("#000000", "#FF0000"))+
  theme(legend.position = "none")

# Pemodelan Kernel Linear
svmfit = svm (y~., data=dat, kernel="linear", cost=10)
plot(svmfit, dat)

# Pemodelan kernel vanilladot
library(kernlab)
kernfit = ksvm(x, y, type="C-svc", kernel="vanilladot")
plot(kernfit, data=x)

# ===== NON-LINEARLY SEPARABLE CASE (BINARY CLASS) =====
# Membentuk titik
x = matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] + 2.5
x[101:150, ] = x[101:150, ] - 2.5
y = c(rep(1,150), rep(2,50))
dat = data.frame(x=x, y=as.factor(y))

library(ggplot2)
ggplot(data=dat, aes(x=x.2, y=x.1, color=y, shape=y))+
  geom_point(size=2)+
  scale_color_manual(values = c("#000000", "#FF0000"))+
  theme(legend.position = "none")

# Pemodelan Kernel Radial
train = base::sample(200,100, replace=FALSE)
svmfit = svm(y~., data=dat[train, ], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat)

# Pemodelan Kernel rbfdot
kernfit = ksvm(x[train, ], y[train], type="C-svc", kernel="rbfdot", C=1, scaled=c())
plot(kernfit, data=x[train, ])

# ===== NON-LINEARLY SEPARABLE CASE (MULTI CLASS) =====
x = rbind(x, matrix(rnorm(50*2), ncol=2))
y = c(y,rep(0,50))
x[y==0.2] = x[y==0.2] + 2.5
dat = data.frame(x=x, y=as.factor(y))

ggplot(data=dat, aes(x=x.2, y=x.1, color=y, shape=y))+
  geom_point(size=2)+
  scale_color_manual(values=c("#000000", "#FF0000", "#00BA00"))+
  theme(legend.position = "none")

# Pemodelan Kernel Radial
svmfit = svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit,dat)

# Pemodelan Kernel rbfdot
kernfit = ksvm(as.matrix(dat[ ,2:1]), dat$y, type="C-svc", kernel="rbfdot",
               C=100, scaled=c())
x.1 = seq(from=min(dat$x.1),to=max(dat$x.1), length=100)
x.2 = seq(from=min(dat$x.2),to=max(dat$x.2), length=100)
x.grid = expand.grid(x.2, x.1)

pred = predict(kernfit, newdata=x.grid)
library(RColorBrewer)
cols = brewer.pal(3, "Set1")
plot(x.grid, pch=19, col=adjustcolor(cols[pred], alpha.f=0.05))
classes = matrix(pred, nrow=100, ncol=100)
contour(x=x.2, y=x.1, z=classes, levels=1:3, labels="", add=TRUE)
points(dat[,2:1], pch=19, col=cols[predict(kernfit)])
