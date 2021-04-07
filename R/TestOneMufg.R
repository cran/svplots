#' Tests the hypothesis over population mean based on one sample by Sv-plot2.
#'
#' @description Decision on hypothesis testing over single mean is made by graphing sample and population Sv-plot2s along with the threshold line. If the intersection point of two Sv-plot2s locates on or above the threshold line, the null hypothesis is rejected at specified significance level, otherwise, failed to reject.
#' @usage test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=TRUE,sigma=NULL,xlab="x",
#'                title="Single mean: Hypothesis testing by Sv-plot2",
#'                samcol="grey5",popcol="grey45",thrcol="black",...)
#' @param X an \eqn{n} by \eqn{1} matrix, equivalently, a column vector of length \eqn{n}, where \eqn{n} is number of observations.
#' @param mu0 hypothesized population mean, \emph{mu0=3.5} by default.
#' @param alpha significance level, \emph{alpha=0.05} by default.
#' @param unkwnsigma population standard deviation is unknown, \emph{TRUE} by default.
#' @param sigma population standard deviation, \emph{NULL} by default.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param title title of the plot, \emph{Single mean: Hypothesis testing by Sv-plot2} by default.
#' @param samcol sample Sv-plot2 color, \emph{grey5} by default.
#' @param popcol sample Sv-plot2 color, \emph{grey45} by default.
#' @param thrcol threshold color, \emph{black} by default.
#' @param ... other graphical parameters.
#'
#' @import ggplot2 stats
#' @importFrom stats pnorm pt qnorm qt quantile sd
#' @return Decision on testing hypotheses over single population mean by Sv-plot2.
#' @references Wijesuriya, U. A. (2020). Sv-plots for identifying characteristics of the
#'             distribution and testing hypotheses. \emph{Communications in Statistics-Simulation and Computation}, \doi{10.1080/03610918.2020.1851716}.
#' @examples
#'    set.seed(5)
#'    X=matrix(rnorm(20,mean=3,sd=2))
#'    test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=TRUE,sigma=NULL,xlab="x",
#'            title="Single mean: Hypothesis testing by Sv-plot2",
#'            samcol="grey5",popcol="grey45",thrcol="black")
#' @export
test1mu<-function(X,mu0=3.5,alpha=0.05,unkwnsigma=TRUE,sigma=NULL,xlab="x",
                  title="Single mean: Hypothesis testing by Sv-plot2",
                  samcol="grey5",popcol="grey45",thrcol="black",...)
{
xbar<-mean(X)
s<-sd(X)
n<-length(X)
d<-abs(xbar-mu0)
lx<-min(xbar,mu0)-d/4
rx<-max(xbar,mu0)+d/4
if (n<30){
  qntl<-qt(alpha/2,df=n-1,lower.tail = FALSE)
  E<-qntl*s/sqrt(n)
  teststat<-(xbar-mu0)/(s/sqrt(n))
  pval<-2*pt(abs(teststat),df=n-1,lower.tail = FALSE)
}
if (n>=30)
{
  if(unkwnsigma==TRUE)
  {
    qntl<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
    E<-qntl*s/sqrt(n)
    teststat<-(xbar-mu0)/(s/sqrt(n))
    pval<-2*pnorm(abs(teststat),mean=0,sd=1,lower.tail = FALSE)
  }
  if(unkwnsigma==FALSE)
  {
   if (ifelse(is.null(sigma),TRUE,FALSE)==TRUE) {
     stop("Provide a positive value for sigma.")
   }
   if (ifelse(is.null(sigma),TRUE,FALSE)==FALSE) {
    qntl<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
    E<-qntl*sigma/sqrt(n)
    teststat<-(xbar-mu0)/(sigma/sqrt(n))
    pval<-2*pnorm(abs(teststat),mean=0,sd=1,lower.tail = FALSE)
   }
  }
}
if(round(((xbar-mu0)/2)^2,2)>round((E/2)^2,2)){position="Above the threshold"; decision="Reject H_0"}
if(round(((xbar-mu0)/2)^2,2)==round((E/2)^2,2)){position="On the threshold"; decision="Reject H_0"}
if(round(((xbar-mu0)/2)^2,2)<round((E/2)^2,2)){position="Below the threshold" ;decision="Fail to reject H_0"}
f1<-function(X) (X-xbar)^2
f2<-function(X) (X-mu0)^2
f3<-function(X) (E/2)^2
g<-ggplot()+
  labs(title = title,y="Squared deviation")+
  theme(legend.justification="top",legend.key=element_blank())+
  stat_function(fun = f1,aes(color="f1",linetype="1"))+
  stat_function(fun = f2,aes(color="f2",linetype="2"))+
  stat_function(fun = f3,aes(color="f3",linetype="3"))+
  scale_x_continuous(xlab,limits=c(lx, rx)) +
  scale_linetype_manual("",values=c("1"="solid","2"="solid","3"="dashed"),guide = FALSE)+
  scale_color_manual("",values=c("f1"=samcol,"f2"=popcol,"f3"=thrcol),
                     labels=c("Sample Sv-plot2","Population Sv-plot2","Threshold"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("1"="solid","2"="solid","3"="dashed"),fill=NA)))
R<-data.frame("Sample_Size"=n, "Average"= round(xbar,4), "Stdev"= round(s,4), "Intersection_Point"=position, "Decision_by_Svplot2"=decision ,"pvalue"=round(pval,4))
return(list("Summary"=R,"Svplot2s"=g))
}
