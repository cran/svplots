#' Tests the hypothesis over population mean based on one sample summary statistics by Sv-plot2.
#'
#' @description Decision on hypothesis testing over single mean is made by graphing sample and population Sv-plot2s along with the threshold line. Intersecting Sv-plots on or above the horizontal line concludes the alternative hypothesis.
#' @usage test1musm(n=20,xbar=3,s=2,mu0=4.5,alpha=0.05,
#'                  unkwnsigma=TRUE,sigma=NULL,xlab="x",
#'                  title="Single mean summary: Hypothesis testing by Sv-plot2",
#'                  samcol="grey5",popcol="grey45",thrcol="black",...)
#' @param n sample size, \emph{n=20} by default.
#' @param xbar sample average, \emph{xbar=3} by default.
#' @param s sample standard deviation, \emph{s=2} by default.
#' @param mu0 hypothesized population mean, \emph{mu0=4.5} by default.
#' @param alpha significance level, \emph{alpha=0.05} by default.
#' @param unkwnsigma population standard deviation is unknown, \emph{TRUE} by default.
#' @param sigma population standard deviation, \emph{NULL} by default.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param title title of the plot, \emph{Single mean: Hypothesis testing by Sv-plot2 by default} by default.
#' @param samcol sample Sv-plot2 color, \emph{grey5} by default.
#' @param popcol sample Sv-plot2 color, \emph{grey45} by default.
#' @param thrcol threshold color, \emph{black}.
#' @param ... other graphical parameters.
#'
#' @import ggplot2 stats
#' @importFrom stats pnorm pt qnorm qt quantile sd
#' @return Decision on testing hypotheses over single population mean by Sv-plot2.
#' @references Wijesuriya, U. A. (2020). Sv-plots for identifying characteristics of the
#'             distribution and testing hypotheses. \emph{Communications in Statistics-Simulation and Computation}, \doi{10.1080/03610918.2020.1851716}.
#' @examples  ## For summary data
#'     test1musm(n=20,xbar=3,s=2,mu0=4.5,alpha=0.05, unkwnsigma=TRUE,sigma=NULL,xlab="x",
#'     title="Single mean summary: Hypothesis testing by Sv-plot2",
#'     samcol="grey5",popcol="grey45",thrcol="black")
#' @export
test1musm<-function(n=20,xbar=3,s=2,mu0=4.5,alpha=0.05, unkwnsigma=TRUE,sigma=NULL,xlab="x",
                    title="Single mean summary: Hypothesis testing by Sv-plot2",
                    samcol="grey5",popcol="grey45",thrcol="black",...)
{
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

  X=seq(lx,rx,1/n)
  f1<-function(X) (X-xbar)^2
  f2<-function(X) (X-mu0)^2
  f3<-function(X)  (E/2)^2
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
  R<-data.frame("Intersection_Point"=position, "Decision_by_Svplot2"=decision ,"pvalue"=round(pval,4))
  return(list("Summary"=R,"Svplot2s"=g))
}
