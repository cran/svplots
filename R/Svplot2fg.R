#' Creates Sv-plot2, the second version of the sample variance plots.
#'
#' @description Sv-plot2 identifies the characteristics of the distribution illustrating squared deviation values in the sample variance against each data value.
#' @usage svplot2(X,title="Sv-plot2",xlab="x",lbcol="grey5", lsdcol="grey60",
#'              rbcol="grey45",rsdcol="grey75",...)
#' @param X an \eqn{n} by \eqn{1} matrix, equivalently, a column vector of length \eqn{n}, where \eqn{n} is number of observations.
#' @param title title of the plot, \emph{Sv-plot2} by default.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param lbcol left bound color, \emph{grey5} by default.
#' @param lsdcol left squared deviation color, \emph{grey60} by default.
#' @param rbcol right bound color, \emph{grey45} by default.
#' @param rsdcol right squared deviation color, \emph{grey75} by default.
#' @param ... other graphical parameters.
#'
#' @import ggplot2 stats
#' @return Sv-plot2
#' @references Wijesuriya, U. A. (2020). Sv-plots for identifying characteristics of the
#'             distribution and testing hypotheses. \emph{Communications in Statistics-Simulation and Computation}, \doi{10.1080/03610918.2020.1851716}.
#' @examples
#'    set.seed(0)
#'    X1 <- matrix(rnorm(50,mean=2,sd=5))
#'    svplot2(X1)
#'
#'    X2 <- matrix(rf(50,df1=10,df2=5))
#'    svplot2(X2)
#'
#'    X3 <- matrix(rbeta(50,shape1=10,shape2=2))
#'    svplot2(X3,lbcol="blue",lsdcol="blue",rbcol="red",rsdcol="red")
#' @export
svplot2<-function(X,title="Sv-plot2",xlab="x",lbcol="grey5",lsdcol="grey60",
                  rbcol="grey45",rsdcol="grey75",...)
{
n<-nrow(X)                   # Finds sample size
Q1<-quantile(X,0.25)         # Finds the 1st quartile
Q3<-quantile(X,0.75)         # Finds the 3rd quartile
UB<-Q3+1.5*(Q3-Q1)           # Finds left bound
LB<-Q1-1.5*(Q3-Q1)           # Finds right bound
xbar<-matrix(mean(X),n,1)    # Creates a vector of sample average
S<-matrix(NA,n,2)            # Creates a bank matrix n rows and 2 columns
for (i in 1:n){
  S[i,1]<-(X[i,1]-xbar[i])^2     # Computes the ith sample deviation square
  if (X[i,1]<= xbar[i]){
    S[i,2]<-3               # Assigns -1 for left data
  }
  if (X[i,1]>xbar[i]){
    S[i,2]<-4                # Assigns 1 for right data
  }
}
LftBnd<-(LB-mean(X))^2
RgtBnd<-(UB-mean(X))^2
Bnd<-rbind(c(min(X),LftBnd,mean(X),LftBnd,1),c(mean(X),RgtBnd,max(X),RgtBnd,2))     # Creates a matrix of 2 by 4 matrix of bounds deviations incuding the sample average left bound being assigned by -2 while righr bound being assigned by 2
x1=Bnd[,1]
y1=Bnd[,2]
x2=Bnd[,3]
y2=Bnd[,4]
catln=Bnd[,5]
Bf<-data.frame(x1,y1,x2,y2,catln)
XS<-cbind(X,S)                         # Combines data and bounds to make a n+2 by 4 matrix
LS<-sum(XS[which(XS[,3]==3),2])        # Computes left sum-sum of areas of left squares
RS<-sum(XS[which(XS[,3]==4),2])        # Computes right sum-sum of areas of right squares
dat<-XS[,1]                            # Creates dat-data column
devsqr<-abs(XS[,2])                    # Creates devsqr-squared deviation column
cat<-XS[,3]                            # Creates cat-category column

Df<-data.frame(dat,devsqr,cat) # Creates the required data frame
g<-ggplot(data=Df)+
  labs(title =title, x=xlab,y="Squared deviation")+
  geom_point(data=Df, mapping=aes(x=dat,y=devsqr,color=as.factor(cat)),alpha=1)+
  theme(legend.position = "bottom",legend.key=element_blank())+
  geom_segment(data=Bf, mapping=aes(x=x1,y=y1, xend=x2,yend=y2,color=as.factor(catln)),linetype="dashed")+
  scale_color_manual("",values=c("1"=lbcol,"2"=rbcol,"3"= lsdcol,"4"= rsdcol),
                     labels=c("Left bound","Right bound","Left squared deviations","Right squared deviations",paste("Left sum = ",LS),paste("Right sum = ",RS)),
                     guide = guide_legend(override.aes = list(
                       linetype = c("dashed","dashed","blank", "blank"),
                       shape = c(NA,NA,16,16),fill=NA)))
R<-data.frame("Sample_Size"=n,"Average"=round(mean(X),4), "Left_SS"= round(LS,4), "Right_SS"= round(RS,4),"Sample_Variance"=round((LS+RS)*(n-1)^(-1),4))
return(list("Summary"=R,"Svplot2"=g))
}
