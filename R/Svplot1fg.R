#' Creates Sv-plot1, the first version of the sample variance plots.
#'
#' @description Sv-plot1 identifies the characteristics of the distribution illustrating  squared deviations in the sample variance by squares for each data value.
#' @usage svplot1(X,title="Sv-plot1",xlab="x",lbcol="grey5",lscol="grey60",
#'               rbcol="grey45",rscol="grey75",...)
#' @param X an \eqn{n} by \eqn{1} matrix, equivalently, a column vector of length \eqn{n}, where \eqn{n} is number of observations.
#' @param title title of the plot, \emph{Sv-plot1} by default.
#' @param xlab \eqn{x}-axis label, \eqn{x} by default.
#' @param lbcol left bound color, \emph{grey5} by default.
#' @param lscol left square color, \emph{grey60} by default.
#' @param rbcol right bound color, \emph{grey45} by default.
#' @param rscol right square color, \emph{grey75} by default.
#' @param ... other graphical parameters.
#'
#' @import ggplot2 stats
#' @return Sv-plot1
#' @references Wijesuriya, U. A. (2020). Sv-plots for identifying characteristics of the
#'             distribution and testing hypotheses. \emph{Communications in Statistics-Simulation and Computation}, \doi{10.1080/03610918.2020.1851716}.
#' @examples
#'    set.seed(0)
#'    X1 <- matrix(rnorm(50,mean=2,sd=5))
#'    svplot1(X1)
#'
#'    X2 <- matrix(rf(50,df1=10,df2=5))
#'    svplot1(X2)
#'
#'    X3 <- matrix(rbeta(50,shape1=10,shape2=2))
#'    svplot1(X3,title="",lbcol="blue",lscol="blue",rbcol="red",rscol="grey75")
#' @export
svplot1<-function(X,title="Sv-plot1",xlab="x",lbcol="grey5",lscol="grey60",
                  rbcol="grey45",rscol="grey75",...)
{
  n<-nrow(X)                   # Finds sample size
  Q1<-quantile(X,0.25)         # Finds the 1st quartile
  Q3<-quantile(X,0.75)         # Finds the 3rd quartile
  UB<-Q3+1.5*(Q3-Q1)           # Finds left bound
  LB<-Q1-1.5*(Q3-Q1)           # Finds right bound
  xbar<-matrix(mean(X),n,1)    # Creates a vector of sample average
  S<-matrix(NA,n,2)            # Creates a bank matrix n rows and 2 columns
  for (i in 1:n){
    S[i,1]<-X[i,1]-xbar[i]     # Computes the ith sample deviation
    if (X[i,1]<= xbar[i]){
      S[i,2]<-3                # Assigns -1 for left data
    }
    if (X[i,1]>xbar[i]){
      S[i,2]<-4                # Assigns 1 for right data
    }
  }
  Bnd<-rbind(c(LB,LB-mean(X),1,mean(X)),c(UB,UB-mean(X),2,mean(X)))     # Creates a matrix of 2 by 4 matrix of bounds deviations including the sample average left bound being assigned by -2 while right bound being assigned by 2
  XB<-rbind(cbind(X,S,xbar),Bnd)                                        # Combines data and bounds to make a n+2 by 4 matrix
  LS<-sum(XB[which(XB[,3]==3),2]^2)         # Computes left sum-sum of areas of left squares
  RS<-sum(XB[which(XB[,3]==4),2]^2)         # Computes right sum-sum of areas of right squares
  datbnd<-XB[,1]                            # Creates datbnd-data for bounds column
  absdev<-abs(XB[,2])                       # Creates absdev-data for absolute deviations column
  cat<-XB[,3]                               # Creates cat-category column
  Avg<-XB[,4]                               # Creates Avg-Average column
  Df<-data.frame(datbnd,absdev,cat,Avg) # Creates the required data frame
  g<-ggplot(data=Df)+
    labs(title = title,x=xlab,y="")+
    geom_rect(data=Df, mapping=aes(xmin=datbnd,xmax=Avg,ymin=0,ymax=absdev,color=as.factor(cat),linetype=as.factor(cat)),alpha=0)+
    theme(legend.position = "bottom",legend.key=element_blank())+
    coord_fixed(ratio=1)+
    scale_linetype_manual("",values=c("1"="dashed","3"="solid","2"="dashed","4"="solid"),labels=c(paste("Left SS = ",LS),paste("Right SS = ",RS),"",""),guide = FALSE)+
    scale_color_manual("",values=c("1"=lbcol,"2"=rbcol,"3"= lscol,"4"= rscol),
                       labels=c("Left bound","Right bound","Left squares","Right squares"),
                       guide = guide_legend(override.aes = list(
                         linetype = c("1"="dashed","2"="dashed","3"="solid", "4"="solid"),fill=NA)))
  R<-data.frame("Sample_Size"=n, "Average"=round(mean(X),4), "Left_SS"= round(LS,4), "Right_SS"= round(RS,4),"Sample_Variance"=round((LS+RS)*(n-1)^(-1),4))
  return(list("Summary"=R,"Svplot1"=g))
  }
