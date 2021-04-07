## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(svplots)
library(ggplot2)
library(stats)

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(2021)
X<-matrix(rnorm(50,mean=2,sd=5))
svplot1(X,title="Sv-plot1",xlab="x",lbcol="grey5",lscol="grey60",rbcol="grey45",rscol="grey75")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(5)
X <- matrix(rbeta(50,shape1=10,shape2=2))
    g<-svplot1(X,title="Sv-plot1",lbcol="grey5",lscol="grey60",rbcol="grey45",rscol="grey75")
        g1=g$Svplot1+theme(panel.grid.minor= element_blank(),
            panel.grid.major= element_blank(),
            legend.position = 'none',
            panel.background = element_rect(fill = "transparent",color = "black"))
    g$Summary
    g1        

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(10)
X<-matrix(rf(50,df1=10,df2=5))
svplot2(X,title="Sv-plot2",xlab="x",lbcol="grey5",lscol="grey60",rbcol="grey45",rscol="grey75")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(10)
X<-matrix(rnorm(50,mean=8,sd=2))
svplot2(X,title="Sv-plot2",xlab="x",lbcol="grey5",lscol="grey60",rbcol="grey45",rscol="grey75")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(5)
X=matrix(rnorm(20,mean=3,sd=2))
test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=TRUE,sigma=NULL,xlab="x",
        title="Single mean: Hypothesis testing by Sv-plot2",
        samcol="grey5",popcol="grey45",thrcol="black")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(5)
X=matrix(rnorm(40,mean=3,sd=2))
test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=FALSE,sigma=2,xlab="x",
        title="Single mean: Hypothesis testing by Sv-plot2",
        samcol="grey5",popcol="grey45",thrcol="black")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
     test1musm(n=20,xbar=3,s=2,mu0=4.5,alpha=0.05, unkwnsigma=TRUE,sigma=NULL,xlab="x",
     title="Single mean summary: Hypothesis testing by Sv-plot2",
     samcol="grey5",popcol="grey45",thrcol="black")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(5)
 test2mu(X1=matrix(rnorm(10,mean=3,sd=2)),X2=matrix(rnorm(20,mean=4,sd=2.5)),
        paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
        sigma1=NULL,sigma2=NULL,alpha=0.05,
        sam1col="grey5",sam2col="grey45",thrcol="black")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
set.seed(5)
X1=matrix(rnorm(10,mean=4,sd=2))
X2=2*X1
test2mu(X1,X2,
        paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
        sigma1=NULL,sigma2=NULL,alpha=0.05,
        sam1col="grey5",sam2col="grey45",thrcol="black")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
test2musm(n1=20,n2=25,xbar1=3,xbar2=4,s1=1,s2=1.5,
          paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
          sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
          xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
          sam1col="grey5",sam2col="grey45",thrcol="black")

## ---- eval=TRUE, fig.width=7, fig.height=4.5----------------------------------
test2musm(n1=20,n2=25,xbar1=3,xbar2=3.7552127,s1=1,s2=1.5,
          paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
          sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
          xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
          sam1col="grey5",sam2col="grey45",thrcol="black")

