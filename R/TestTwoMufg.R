#' Tests the hypothesis over two population means based on two samples by Sv-plot2.
#'
#' @description Decision on hypothesis testing over two means is made by graphing two sample Sv-plot2s along with the threshold line. If the intersection point of two Sv-plot2s locates on or above the threshold line, the null hypothesis is rejected at specified significance level, otherwise, failed to reject.
#' @usage test2mu(X1,X2,paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'                sigma1=NULL,sigma2=NULL,alpha=0.05,xlab="x",
#'                title="Two means: Hypothesis testing by Sv-plot2",
#'                sam1col="grey5",sam2col="grey45",thrcol="black",...)
#' @param X1 an \eqn{n1} by \eqn{1} matrix, equivalently, a column vector of length \eqn{n1}, where \eqn{n1} is number of observations.
#' @param X2 an \eqn{n2} by \eqn{1} matrix, equivalently, a column vector of length \eqn{n2}, where \eqn{n2} is number of observations.
#' @param paired for dependent samples TRUE, \emph{FALSE} by default.
#' @param eqlvar population variances are equal, \emph{FALSE} by default.
#' @param unkwnsigmas population standard deviations are unknown, \emph{TRUE} by default.
#' @param sigma1 population1  standard deviation, \emph{NULL} by default.
#' @param sigma2 population2  standard deviation, \emph{NULL} by default.
#' @param alpha significance level, \emph{alpha=0.05} by default.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param title title of the plot, \emph{Two means: Hypothesis testing by Sv-plot2} by default.
#' @param sam1col sample1 Sv-plot2 color, \emph{grey5} by default.
#' @param sam2col sample2 Sv-plot2 color, \emph{grey45} by default.
#' @param thrcol threshold color, \emph{black} by default.
#' @param ... other graphical parameters.
#'
#' @import ggplot2 stats
#' @importFrom stats pnorm pt qnorm qt quantile sd
#' @return Decision on testing hypotheses over two population means by Sv-plot2.
#' @references Wijesuriya, U. A. (2020). Sv-plots for identifying characteristics of the
#'             distribution and testing hypotheses. \emph{Communications in Statistics-Simulation and Computation}, \doi{10.1080/03610918.2020.1851716}.
#' @examples
#' set.seed(5)
#' test2mu(X1=matrix(rnorm(10,mean=3,sd=2)),X2=matrix(rnorm(20,mean=4,sd=2.5)),
#'        paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'        sigma1=NULL,sigma2=NULL,alpha=0.05,
#'        sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#' test2mu(X1=matrix(rnorm(10,mean=3,sd=2)),X2=matrix(rnorm(20,mean=4,sd=2.5)),
#'        paired=FALSE,eqlvar=TRUE,unkwnsigmas=TRUE,
#'        sigma1=NULL,sigma2=NULL,alpha=0.05,
#'        sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#' test2mu(X1=matrix(rnorm(50,mean=3,sd=2)),X2=matrix(rnorm(30,mean=4,sd=2.5)),
#'        xlab="x",title="Two means: Hypothesis testing by Sv-plot2",
#'        paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'        sigma1=NULL,sigma2=NULL,alpha=0.05,
#'        sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#' test2mu(X1=matrix(rnorm(50,mean=3,sd=2)),X2=matrix(rnorm(30,mean=4,sd=2.5)),
#'        paired=FALSE,eqlvar=FALSE,unkwnsigmas=FALSE,
#'        sigma1=2,sigma2=4.920782,alpha=0.05,
#'        sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#' X1=matrix(rnorm(10,mean=3,sd=2))
#' X2=2*X1
#' test2mu(X1,X2,
#'        paired=TRUE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'        sigma1=NULL,sigma2=NULL,alpha=0.05,
#'        sam1col="blue",sam2col="red",thrcol="black")
#' @export
test2mu<-function(X1,X2,paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
                  sigma1=NULL,sigma2=NULL,alpha=0.05,xlab="x",
                  title="Two means: Hypothesis testing by Sv-plot2",
                  sam1col="grey5",sam2col="grey45",thrcol="black",...)
{
xbar1<-mean(X1)
s1<-sd(X1)
xbar2<-mean(X2)
s2<-sd(X2)
n1<-length(X1)
n2<-length(X2)
d<-abs(xbar1-xbar2)
lx<-min(xbar1,xbar2)-d/4
rx<-max(xbar1,xbar2)+d/4
if (paired==FALSE)
{
  if (n1<30 && n2<30){
    if (eqlvar==FALSE)
    {
      nu<-((s1^2/n1+s2^2/n2)^2)/((s1^2/n1)^2*(n1-1)^(-1)+(s2^2/n2)^2*(n2-1)^(-1))
      qntl<-qt(alpha/2,df=nu,lower.tail = FALSE)
      E<-qntl*sqrt(s1^2/n1+s2^2/n2)
      teststat<-(xbar1-xbar2)/sqrt(s1^2/n1+s2^2/n2)
      pval<-2*pt(abs(teststat),df=nu,lower.tail = FALSE)
    }
    if (eqlvar==TRUE)
    {
      sp<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
      qntl<-qt(alpha/2,df=n1+n2-2,lower.tail = FALSE)
      E<-qntl*sp*sqrt(1/n1+1/n2)
      teststat<-(xbar1-xbar2)/(sp*sqrt(1/n1+1/n2))
      pval<-2*pt(abs(teststat),df=n1+n2-2,lower.tail = FALSE)
    }
  }
  if (n1>=30 && n2>=30)
  {
    if(unkwnsigmas==TRUE)
    {
      qntl<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
      E<-qntl*sqrt(s1^2/n1+s2^2/n2)
      teststat<-(xbar1-xbar2)/sqrt(s1^2/n1+s2^2/n2)
      pval<-2*pnorm(abs(teststat),mean=0,sd=1,lower.tail = FALSE)
    }
    if(unkwnsigmas==FALSE)
    {
      if (ifelse(is.null(sigma1)||is.null(sigma2),TRUE,FALSE)==TRUE) {
        stop("Provide positive values for sigma1 and sigma2.")
      }
      if (ifelse(is.null(sigma1)&&is.null(sigma2),TRUE,FALSE)==FALSE) {
      qntl<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
      E<-qntl*sqrt(sigma1^2/n1+sigma2^2/n2)
      teststat<-(xbar1-xbar2)/sqrt(sigma1^2/n1+sigma2^2/n2)
      pval<-2*pnorm(abs(teststat),mean=0,sd=1,lower.tail = FALSE)
      }
    }
  }
}

if (paired==TRUE)
{
  if (ifelse(n1!=n2,TRUE,FALSE)==TRUE) {
    stop("Sample sizes n1 and n2 should be equal.")
  }
  if (ifelse(n1!=n2,TRUE,FALSE)==FALSE) {
  s<-sd(X1-X2)
  qntl<-qt(alpha/2,df=n1-1,lower.tail = FALSE)
  E<-qntl*s/sqrt(n1)
  teststat<-(xbar1-xbar2)/(s/sqrt(n1))
  pval<-2*pt(abs(teststat),df=n1-1,lower.tail = FALSE)
  }
}

if(round(((xbar1-xbar2)/2)^2,2)>round((E/2)^2,2)){position="Above the threshold"; decision="Reject H_0"}
if(round(((xbar1-xbar2)/2)^2,2)==round((E/2)^2,2)){position="On the threshold"; decision="Reject H_0"}
if(round(((xbar1-xbar2)/2)^2,2)<round((E/2)^2,2)){position="Below the threshold" ;decision="Fail to reject H_0"}

x=seq(lx,rx,max(n1,n2))
f1<-function(x) (x-xbar1)^2
f2<-function(x) (x-xbar2)^2
f3<-function(x)  (E/2)^2
g<-ggplot()+
  labs(title = title,y="Squared deviation")+
  theme(legend.justification="top",legend.key=element_blank())+
  stat_function(fun = f1,aes(color="f1",linetype="1"))+
  stat_function(fun = f2,aes(color="f2",linetype="2"))+
  stat_function(fun = f3,aes(color="f3",linetype="3"))+
  scale_x_continuous(xlab,limits=c(lx, rx)) +
  scale_linetype_manual("",values=c("1"="solid","2"="solid","3"="dashed"),guide = FALSE)+
  scale_color_manual("",values=c("f1"=sam1col,"f2"=sam2col,"f3"=thrcol),
                     labels=c("Sample1 Sv-plot2","Sample2 Sv-plot2","Threshold"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("1"="solid","2"="solid","3"="dashed"),fill=NA)))
R<-data.frame("Sample"=c(1,2),"Size"=c(n1,n2), "Average"=round(c(xbar1,xbar2),4), "Stdev"=round(c(s1,s2),4),"Intersection_Point"=c(position,""), "Decision_by_Svplot2"=c(decision,"") ,"pvalue"=c(round(pval,4),""))
return(list("Summary"=R,"Svplot2s"=g))
}
