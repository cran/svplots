#' Tests the hypothesis over two population means based on two samples summary statistics by Sv-plot2.
#'
#' @description Decision on hypothesis testing over two means is made by graphing two sample Sv-plot2s along with the threshold line. Intersecting Sv-plots on or above the horizontal line concludes the alternative hypothesis.
#' @usage test2musm(n1=20,n2=25,xbar1=3,xbar2=4,s1=1,s2=1.5,
#'                  paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'                  sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
#'                  xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
#'                  sam1col="grey5",sam2col="grey45",thrcol="black",...)
#' @param n1 sample1 size,  \emph{n1=20} by default.
#' @param n2 sample2 size,  \emph{n2=25} by default.
#' @param xbar1 sample1 average, \emph{xbar1=3} by default.
#' @param xbar2 sample2 average, \emph{xbar2=4} by default.
#' @param s1 sample1 standard deviation, \emph{s1=1} by default.
#' @param s2 sample2 standard deviation, \emph{s2=1.5} by default.
#' @param paired for dependent samples TRUE, \emph{FALSE} by default.
#' @param eqlvar population variances are equal, \emph{FALSE} by default.
#' @param unkwnsigmas population standard deviations are unknown, \emph{TRUE} by default.
#' @param sigma1 population1  standard deviation, \emph{NULL} by default.
#' @param sigma2 population2  standard deviation, \emph{NULL} by default.
#' @param sdevdif standard deviation of the differences, \emph{NULL} by default.
#' @param alpha significance level, \emph{alpha=0.05} by default.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param title title of the plot, \emph{Two means: Hypothesis testing by Sv-plot2} by default.
#' @param sam1col sample1 Sv-plot2 color, \emph{grey5} by default.
#' @param sam2col sample2 Sv-plot2 color, \emph{grey45} by default.
#' @param thrcol threshold color, \emph{black} by default.
#' @param ... other graphical parameter.
#'
#' @import ggplot2 stats
#' @importFrom stats pnorm pt qnorm qt quantile sd
#' @return Decision on testing hypotheses over two population means by Sv-plot2.
#' @references Wijesuriya, U. A. (2020). Sv-plots for identifying characteristics of the
#'             distribution and testing hypotheses. \emph{Communications in Statistics-Simulation and Computation}, \doi{10.1080/03610918.2020.1851716}.
#' @examples  ## For summary data
#'test2musm(n1=20,n2=25,xbar1=3,xbar2=4,s1=1,s2=1.5,
#'          paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'          sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
#'          xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
#'          sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#'test2musm(n1=20,n2=25,xbar1=3,xbar2=4,s1=1.5,s2=1.5,
#'         paired=FALSE,eqlvar=TRUE,unkwnsigmas=TRUE,
#'         sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
#'         xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
#'         sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#'test2musm(n1=50,n2=35,xbar1=3,xbar2=4,s1=1,s2=1.5,
#'          paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'          sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
#'          xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
#'          sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#'test2musm(n1=50,n2=35,xbar1=3,xbar2=4,s1=1,s2=1.5,
#'          paired=FALSE,eqlvar=FALSE,unkwnsigmas=FALSE,
#'          sigma1=2,sigma2=3,sdevdif=NULL,alpha=0.05,
#'          xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
#'          sam1col="grey5",sam2col="grey45",thrcol="black")
#'
#'test2musm(n1=20,n2=20,xbar1=3,xbar2=4,s1=1,s2=1.5,
#'          paired=TRUE,eqlvar=FALSE,unkwnsigmas=TRUE,
#'          sigma1=NULL,sigma2=NULL,sdevdif=2,alpha=0.05,
#'          xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
#'          sam1col="grey45",sam2col="grey5",thrcol="black")
#' @export
test2musm<-function(n1=20,n2=25,xbar1=3,xbar2=4,s1=1,s2=1.5,
                    paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
                    sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
                    xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                    sam1col="grey5",sam2col="grey45",thrcol="black",...)
{
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
    if (ifelse(n1!=n2||is.null(sdevdif),TRUE,FALSE)==TRUE) {
      stop("Sample sizes n1 and n2 should be equal and sdevdif should be a positive value.")
    }
    if (ifelse(n1!=n2 && is.null(sdevdif),TRUE,FALSE)==FALSE) {
    qntl<-qt(alpha/2,df=n1-1,lower.tail = FALSE)
    E<-qntl*sdevdif/sqrt(n1)
    teststat<-(xbar1-xbar2)/(sdevdif/sqrt(n1))
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
  R<-data.frame("Intersection_Point"=position, "Decision_by_Svplot2"=decision ,"pvalue"=round(pval,4))
  return(list("Summary"=R,"Svplot2s"=g))
}
