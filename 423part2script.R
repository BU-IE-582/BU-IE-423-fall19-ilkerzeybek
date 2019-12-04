library(jpeg)
img <- readJPEG("C:/Users/ilker zeybek/Desktop/ie423 part2/group_16grayscale.jpg", native = FALSE)
img2 <- img
img3 <- img
for(i in 1:400){
for(j in 1:400){
rows <- img2[i,]
rowsucl <- mean(rows) + 3*sd(rows)
rowslcl <- mean(rows) - 3*sd(rows)
if(img2[i,j] > rowsucl | img2[i,j] < rowslcl){
img2[i,j] <- 0}}}
par(mfrow=c(1,2))
plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)),xlab="Horizontal",ylab="Vertical")
rasterImage(img,0,0,nrow(img),ncol(img))
plot(NA,xlim=c(0,nrow(img2)),ylim=c(0,ncol(img2)),xlab="Horizontal",ylab="Vertical")
rasterImage(img2,0,0,nrow(img2),ncol(img2))
img2[which(img2 == 0)]
for(i in 1:400){
for(j in 1:400){
columns <- img3[,i]
columnsucl <- mean(columns) + 3*sd(columns)
columnslcl <- mean(columns) - 3*sd(columns)
if(img3[j,i] > columnsucl | img3[j,i] < columnslcl){
img3[j,i] <- 0}}}
par(mfrow=c(1,2))
plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)),xlab="Horizontal",ylab="Vertical")
rasterImage(img,0,0,nrow(img),ncol(img))
plot(NA,xlim=c(0,nrow(img3)),ylim=c(0,ncol(img3)),xlab="Horizontal",ylab="Vertical")
rasterImage(img3,0,0,nrow(img3),ncol(img3))
img3[which(img3 == 0)]
img4 <- img
counter <- 1
datamatrix <- matrix(0,nrow=4900,ncol=2601)
for(i in 1:69){
for(j in 1:69){
patch <- img[(5*(i-1)+1):((5*(i-1))+51),(5*(j-1)+1):((5*(j-1))+51)]
patchrows <- as.matrix(t(patch))
datamatrix[counter,] <- patchrows
counter <- counter + 1}}
data <- data.frame(datamatrix)
model <- lm(X1301~.,data)
residuals <- resid(model)
plot(residuals)
hist(residuals)
ucl <- mean(residuals) + 3*sd(residuals)
lcl <- mean(residuals) - 3*sd(residuals)
ucl
lcl
for(i in 1:70){
for(j in 1:70){
if(residuals[j+70*(i-1)] > ucl | residuals[j+70*(i-1)]< lcl){
img4[26+5*(i-1),26+5*(j-1)] <- 0}}}
par(mfrow=c(1,2))
plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)),xlab="Horizontal",ylab="Vertical")
rasterImage(img,0,0,nrow(img),ncol(img))
plot(NA,xlim=c(0,nrow(img4)),ylim=c(0,ncol(img4)),xlab="Horizontal",ylab="Vertical")
rasterImage(img4,0,0,nrow(img4),ncol(img4))
img4[which(img4 == 0)]