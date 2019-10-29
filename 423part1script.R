install.packages("jpeg")
library(jpeg)
img <- readJPEG("C:/Users/ilker zeybek/Desktop/423part1.jpg", native = FALSE)
plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)),xlab="Horizontal",ylab="Vertical")
rasterImage(img,0,0,nrow(img),ncol(img))
a <- img[,,1]
b <- img[,,2]
c <- img[,,3]
par(mfrow=c(1,3))
plot(NA,xlim=c(0,nrow(a)),ylim=c(0,ncol(a)))
rasterImage(a,0,0,nrow(a),ncol(a))
plot(NA,xlim=c(0,nrow(b)),ylim=c(0,ncol(b)))
rasterImage(b,0,0,nrow(b),ncol(b))
plot(NA,xlim=c(0,nrow(c)),ylim=c(0,ncol(c)))
rasterImage(c,0,0,nrow(c),ncol(c))
x <- colMeans(a)
y <- colMeans(b)
z <- colMeans(c)
par(mfrow=c(1,1))
plot(x, type = "l", col="red",xlab = "Columns" , ylab = "Mean", ylim = c(0.3,1))
lines(y,col="Green")
lines(z,col="Blue")
k <- a[1:256,1:512]
l <- a[257:512,1:512]
m <- k-l
plot(NA,xlim=c(0,nrow(m)),ylim=c(0,ncol(m)),xlab="Horizontal",ylab="Vertical")
rasterImage(abs(m),0,0,nrow(m),ncol(m))
k2 <- b[1:256,1:512]
l2 <- b[257:512,1:512]
m2 <- k2-l2
plot(NA,xlim=c(0,nrow(m2)),ylim=c(0,ncol(m2)),xlab="Horizontal",ylab="Vertical")
rasterImage(abs(m2),0,0,nrow(m2),ncol(m2))
k3 <- c[1:256,1:512]
l3 <- c[257:512,1:512]
m3 <- k3-l3
plot(NA,xlim=c(0,nrow(m3)),ylim=c(0,ncol(m3)),xlab="Horizontal",ylab="Vertical")
rasterImage(abs(m3),0,0,nrow(m3),ncol(m3))
install.packages("imagine")
library(imagine)
fivemedianfiltereda <- medianFilter(a,radius=5,times=1)
fivemedianfiltereda <- apply(fivemedianfiltereda,2,rev)
plot(NA,xlim=c(0,nrow(fivemedianfiltereda)),ylim=c(0,ncol(fivemedianfiltereda)),xlab="Horizontal",ylab="Vertical")
rasterImage(fivemedianfiltereda,0,0,nrow(fivemedianfiltereda),ncol(fivemedianfiltereda))
fivemedianfilteredb <- medianFilter(b,radius=5,times=1)
fivemedianfilteredb <- apply(fivemedianfilteredb,2,rev)
plot(NA,xlim=c(0,nrow(fivemedianfilteredb)),ylim=c(0,ncol(fivemedianfilteredb)),xlab="Horizontal",ylab="Vertical")
rasterImage(fivemedianfilteredb,0,0,nrow(fivemedianfilteredb),ncol(fivemedianfilteredb))
fivemedianfilteredc <- medianFilter(c,radius=5,times=1)
fivemedianfilteredc <- apply(fivemedianfilteredc,2,rev)
plot(NA,xlim=c(0,nrow(fivemedianfilteredc)),ylim=c(0,ncol(fivemedianfilteredc)),xlab="Horizontal",ylab="Vertical")
rasterImage(fivemedianfilteredc,0,0,nrow(fivemedianfilteredc),ncol(fivemedianfilteredc))
elevenmedianfiltereda <- medianFilter(a,radius=11,times=1)
elevenmedianfiltereda <- apply(elevenmedianfiltereda,2,rev)
plot(NA,xlim=c(0,nrow(elevenmedianfiltereda)),ylim=c(0,ncol(elevenmedianfiltereda)),xlab="Horizontal",ylab="Vertical")
rasterImage(elevenmedianfiltereda,0,0,nrow(elevenmedianfiltereda),ncol(elevenmedianfiltereda))
elevenmedianfilteredb <- medianFilter(b,radius=11,times=1)
elevenmedianfilteredb <- apply(elevenmedianfilteredb,2,rev)
plot(NA,xlim=c(0,nrow(elevenmedianfilteredb)),ylim=c(0,ncol(elevenmedianfilteredb)),xlab="Horizontal",ylab="Vertical")
rasterImage(elevenmedianfilteredb,0,0,nrow(elevenmedianfilteredb),ncol(elevenmedianfilteredb))
elevenmedianfilteredc <- medianFilter(c,radius=11,times=1)
elevenmedianfilteredc <- apply(elevenmedianfilteredc,2,rev)
plot(NA,xlim=c(0,nrow(elevenmedianfilteredc)),ylim=c(0,ncol(elevenmedianfilteredc)),xlab="Horizontal",ylab="Vertical")
rasterImage(elevenmedianfilteredc,0,0,nrow(elevenmedianfilteredc),ncol(elevenmedianfilteredc))
thirtyonemedianfiltereda <- medianFilter(a,radius=31,times=1)
thirtyonemedianfiltereda <- apply(thirtyonemedianfiltereda,2,rev)
plot(NA,xlim=c(0,nrow(thirtyonemedianfiltereda)),ylim=c(0,ncol(thirtyonemedianfiltereda)),xlab="Horizontal",ylab="Vertical")
rasterImage(thirtyonemedianfiltereda,0,0,nrow(thirtyonemedianfiltereda),ncol(thirtyonemedianfiltereda))
thirtyonemedianfilteredb <- medianFilter(b,radius=31,times=1)
thirtyonemedianfilteredb <- apply(thirtyonemedianfilteredb,2,rev)
plot(NA,xlim=c(0,nrow(thirtyonemedianfilteredb)),ylim=c(0,ncol(thirtyonemedianfilteredb)),xlab="Horizontal",ylab="Vertical")
rasterImage(thirtyonemedianfilteredb,0,0,nrow(thirtyonemedianfilteredb),ncol(thirtyonemedianfilteredb))
thirtyonemedianfilteredc <- medianFilter(c,radius=31,times=1)
thirtyonemedianfilteredc <- apply(thirtyonemedianfilteredc,2,rev)
plot(NA,xlim=c(0,nrow(thirtyonemedianfilteredc)),ylim=c(0,ncol(thirtyonemedianfilteredc)),xlab="Horizontal",ylab="Vertical")
rasterImage(thirtyonemedianfilteredc,0,0,nrow(thirtyonemedianfilteredc),ncol(thirtyonemedianfilteredc))
part2img <- readJPEG("C:/Users/ilker zeybek/Desktop/423part1grayscale.jpg", native = FALSE)
hist(part2img)
sample <- sample(part2img,1000,replace = FALSE)
shapiro.test(sample)
mean(sample)
sd(sample)
upperlimit <- qnorm(0.999,mean(sample),sd(sample))
lowerlimit <- qnorm(0.001,mean(sample),sd(sample))
part2img[part2img > upperlimit]
part2img[part2img < lowerlimit]
part2img[part2img > upperlimit] <- 0
part2img[part2img < lowerlimit] <- 0
part2img2 <- readJPEG("C:/Users/ilker zeybek/Desktop/423part1grayscale.jpg", native = FALSE)
plot(NA,xlim=c(0,nrow(part2img)),ylim=c(0,ncol(part2img)),xlab="Horizontal",ylab="Vertical")
rasterImage(part2img,0,0,nrow(part2img),ncol(part2img))
plot(NA,xlim=c(0,nrow(part2img2)),ylim=c(0,ncol(part2img2)),xlab="Horizontal",ylab="Vertical")
rasterImage(part2img2,0,0,nrow(part2img2),ncol(part2img2))
fracturepic <- part2img2
for (i in 1:10){
  for(j in 1:10){
    fiftyonepatch <- fracturepic[(51*(i-1)+ 1) : (51*i) , (51*(j-1)+1) : (51*j)]
    meanpatch <- mean(fiftyonepatch)
    stdpatch <- sd(fiftyonepatch)
    patchupperlimit <- qnorm(0.999,meanpatch,stdpatch)
    patchlowerlimit <- qnorm(0.001,meanpatch,stdpatch)
    fracturepic[(51*(i-1)+1):(51*i),(51*(j-1)+1):(51*j)] <- ifelse(fracturepic[(51*(i-1)+1):(51*i),(51*(j-1)+1):(51*j)] < patchlowerlimit,0,fracturepic[(51*(i-1)+1):(51*i),(51*(j-1)+1):(51*j)])
    fracturepic[(51*(i-1)+1):(51*i),(51*(j-1)+1):(51*j)] <- ifelse(fracturepic[(51*(i-1)+1):(51*i),(51*(j-1)+1):(51*j)] > patchupperlimit,0,fracturepic[(51*(i-1)+1):(51*i),(51*(j-1)+1):(51*j)])}}
par(mfrow=c(1,2))
plot(NA,xlim=c(0,nrow(fracturepic)),ylim=c(0,ncol(fracturepic)),xlab="Horizontal",ylab="Vertical")
rasterImage(fracturepic,0,0,nrow(fracturepic),ncol(fracturepic))
plot(NA,xlim=c(0,nrow(part2img)),ylim=c(0,ncol(part2img)),xlab="Horizontal",ylab="Vertical")
rasterImage(part2img2,0,0,nrow(part2img2),ncol(part2img2))