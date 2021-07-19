######## 2.1.1 Standard error,LLN,and CLT

#### page 62

# draws N samples of size n from Uniform(0,1)
# and plots the N means with a normal distribution overlay
demo.clt.unif<-function(N,n){
  # draw sample in a matrix with N columns and n rows
  sam<-matrix(runif(N*n,0,1),ncol=N)
  # calculate the mean of each column
  sam.mean<-colMeans(sam)
  # the sd of the mean is the SEM
  sam.se<-sd(sam.mean)
  # calculate the true SEM given the sample size n
  true.se<-sqrt((1/12)/n)
  # draw a histogram of the means
  hist(sam.mean,freq=FALSE,breaks=25,main=paste("True SEM=",round(true.se,4),",Est SEM=",round(sam.se,4)),xlab=paste("n=",n))
  # overlay a density curve for the sample means
  points(density(sam.mean),type="l")
  # overlay a normal distribution,bold and red
  x<-seq(0,1,length=1000)
  points(x,dnorm(x,mean=0.5,sd=true.se),type="l",lwd=2,col="blue")
  # place a rug of points under the plot
  rug(sam.mean)
}

par(mfrow=c(2,2));
demo.clt.unif(10000,1);
demo.clt.unif(10000,2);
demo.clt.unif(10000,6);
demo.clt.unif(10000,12);

#### page 63

#### Illustration of Central Limit Theorem,Empontential distribution
# demo.clt.exp(N,n)draws N samples of size n from Exponential(1)
# and plots the N means with a normal distribution overlay
demo.clt.exp<-function(N,n){
  # draw sample in a matrix with N columns and n rows
  sam<-matrix(rexp(N*n,1),ncol=N);
  # calculate the mean of each column
  sam.mean<-colMeans(sam)
  # the sd of the mean is the SEM
  sam.se<-sd(sam.mean)
  # calculate the true SEM given the sample size n
  true.se<-sqrt(1/n)
  # draw a histogram of the means
  hist(sam.mean,freq=FALSE,breaks=25,main=paste("True SEM=",round(true.se,4),",Est SEM=",round(sam.se,4)),xlab=paste("n=",n))
  # overlay a density curve for the sample means
  points(density(sam.mean),type="l")
  x<-seq(0,5,length=1000)
  points(x,dnorm(x,mean=1,sd=true.se),type="l",lwd=2,col="blue")
  # place a rug of points under the plot
  rug(sam.mean)
}

par(mfrow=c(2,2));
demo.clt.exp(10000,1);
demo.clt.exp(10000,6);
demo.clt.exp(10000,30);
demo.clt.exp(10000,100);

######## 2.1.2 t-distribution

#### page 66

#### Normal vs t-distributions with a range of degrees-of-freedom
x<-seq(-8,8,length=1000)
par(mfrow=c(1,1))
plot(x,dnorm(x),type="l",lwd=2,col="blue",main="Normal(blue) vs t-dist with df=1,2,6,12,30,100")
points(x,dt(x,1),type="l")
points(x,dt (x,2),type="l")
points(x,dt(x,6),type="l")
points(x,dt(x,12),type="l")
points(x,dt(x,30),type="l")
points(x,dt(x,100),type="l")

######## 2.2 CI for ¦Ì

#### page 69

#### Illustration of Confidence Intervals(consistent with their interpretation)
library(TeachingDemos)
ci.examp(mean.sim=10,sd=2,n=25,reps=100,conf.level=0.95,method="t")

#### page 70

#### Visual compurison of whether sampling distribution is close to Normal Bootstrap
# a function to compare the bootstrap sampling distribution with
# a normal distribution with mean and SEM estimated from the data

bs.one.samp.dist<-function(dat,N=1e4){
  n<-length(dat)
  # resample from data
  sam<-matrix(sample(dat,size=N*n,replace=TRUE),ncol=N);
  # draw a histogram of the means
  sam.mean<-colMeans(sam);
  # save par() settings
  old.par<-par(no.readonly=TRUE)
  # make smaller margins
  par(mfrow=c(2,1),mar=c(3,2,2,1),oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  hist(dat,freq=FALSE,breaks=6,main="Plot of data with smooth eddensity curve")
  points(density(dat),type="l")
  rug(dat)
  hist(sam.mean,freq=FALSE,breaks=25,main="Bootstrap sampling distribution of the mean",xlab=paste("Data:n=",n,",mean=",signif(mean(dat),digits=5),",se=",signif(sd(dat)/sqrt(n)),digits=5))
  
  # overlay a density curve for the sample means
  points(density(sam.mean),type="l")
  # overlay a normal distribution,bold and red
  x<-seq(min(sam.mean),max(sam.mean),length=1000)
  points(x,dnorm(x,mean=mean(dat),sd=sd(dat)/sqrt(n)),type="l",lwd=2,col="red")
  # place a rug of points under the plot
  rug(sam.mean)
  # restore par() settings
  par(old.par)
}

# example data,skewed---try others datasets to develop your inturtion
x<-rgamma(10,shape=.5,scale=20)
bs.one.samp.dist(x)

#### page 78

#### Example:Age at First Transplant
# enter data as a vector
age<-c(54,42,51,54,49,56,33,58,54,64,49)
par(mfrow=c(2,1))
hist(age,freq=FALSE,breaks=6)
points(density(age),type="l")
rug(age)

# violin plot
library(vioplot)
vioplot(age,horizontal=TRUE,col="gray")

# stem-and-leaf plot
stem(age,scale=2)

# t.crit
qt(1-0.05/2,df=length(age)-1)
# look at help for t.test
?t.test
# defaults include:alternative="two.sided",conf.level=0.95
t.summary<-t.test(age,mu=50)
t.summary
summary(age)

bs.one.samp.dist(age)

# Function at plot t-distribution with shaded p-value
t.dist.pval<-function(t.summary){
  par(mfrow=c(1,1))
  lim.extreme<-max(4,abs(t.summary$statistic)+0.5)
  lim.lower<--lim.extreme;
  lim.upper<-lim.extreme;
  x.curve<-seq(lim.lower,lim.upper,length=200)
  y.curve<-dt(x.curve,df=t.summary$parameter)
  plot(x.curve,y.curve,type="n"
       ,ylab=paste("t-dist(df=",signif(t.summary$parameter,3),")")
       ,xlab=paste("t-stat=",signif(t.summary$statistic,5)
                   ,"£¬Shaded area is p-value=",signif(t.summary$p.value,5)))
  if((t.summary$alternative=="less")|(t.summary$alternative=="two.sided")){
    x.pval.l<-seq(lim.lower,-abs(t.summary$statistic),length=200)
    y.pval.l<-dt(x.pval.l,df=t.summary$parameter)
    polygon(c(lim.lower,x.pval.l,-abs(t.summary$statistic))
            ,c(0,y.pval.l,0),col="gray")
  }
  if((t.summary$alternative=="greater")|(t.summary$alternative=="two.sided")){
    x.pval.u<-seq(abs(t.summary$statistic),lim.upper,length=200)
    y.pval.u<-dt(x.pval.u,df=t.summary$parameter)
    polygon(c(abs(t.summary$statistic),x.pval.u,lim.upper)
            ,c(0,y.pval.u,0),col="gray")
  }
  points(x.curve,y.curve,type="l",lwd=2,col="blue")
}

# for the age example
t.dist.pval(t.summary)

names(t.summary)

t.summary$statistic

t.summary$parameter

t.summary$p.vaule

t.summary$conf.int

t.summary$estimate

t.summary$null.value

t.summary$alternative

t.summary$method

t.summary$data.name

#### page 83

# Example:Meteorites
# enter data as a vector
toco<-c(5.6,2.7,6.2,2.9,1.5,4.0,4.3,3.0,3.6,2.4,6.7,3.8)

par(mfrow=c(2,1))
# Histogram overlaid with kernel density curve
hist(toco,freq=FALSE,breaks=6)
points(density(toco),type="l")
rug(toco)

# violin plot
vioplot(toco,horizontal=TRUE,col="gray")

# stem-and-leaf plot
stem(toco,scale=2)

# t.crit
qt(1-0.05/2,df=length(toco)-1)

t.summary<-t.test(toco,mu=0.54)
t.summary

summary(toco)

t.dist.pval(t.summary)
bs.one.samp.dist(toco)

