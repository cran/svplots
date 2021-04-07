test_that("test2musm works", {
# Independent Small samples with unknown-unequal sigmas
  Out1=capture.output(test2musm(n1=20,n2=25,xbar1=3,xbar2=4,s1=1,s2=1.5,
                                paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
                                sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
                                xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                                sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out1[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out1[3],"1 Above the threshold          Reject H_0 0.0107")

  Out11=capture.output(test2musm(n1=20,n2=25,xbar1=3,xbar2=3.7552127,s1=1,s2=1.5,
                                 paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
                                 sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
                                 xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                                 sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out11[2],"  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out11[3],"1   On the threshold          Reject H_0   0.05")

  Out2=capture.output(test2musm(n1=20,n2=25,xbar1=3,xbar2=4,s1=1.5,s2=1.5,
                                paired=FALSE,eqlvar=TRUE,unkwnsigmas=TRUE,
                                sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
                                xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                                sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out2[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out2[3],"1 Above the threshold          Reject H_0 0.0316")

  Out4=capture.output(test2musm(n1=50,n2=35,xbar1=3,xbar2=4,s1=1,s2=1.5,
                                paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
                                sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
                                xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                                sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out4[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out4[3],"1 Above the threshold          Reject H_0  6e-04")

  Out5=capture.output(test2musm(n1=50,n2=35,xbar1=3,xbar2=4,s1=1,s2=4,
                                paired=FALSE,eqlvar=FALSE,unkwnsigmas=FALSE,
                                sigma1=2,sigma2=3,sdevdif=NULL,alpha=0.05,
                                xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                                sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out5[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out5[3],"1 Below the threshold  Fail to reject H_0  0.085")

# Error Message
#  f1 <- function() stop("Provide a positive values for sigma1 and sigma2.")
#  expect_error(f1())
#  expect_error(f1(),"Provide a positive values for sigma1 and sigma2.")

  # Error Message for positive sigmas
  expect_error(test2musm(n1=50,n2=35,xbar1=3,xbar2=4,s1=1,s2=4,
                         paired=FALSE,eqlvar=FALSE,unkwnsigmas=FALSE,
                         sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
                         xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                         sam1col="grey5",sam2col="grey45",thrcol="black"),"Provide positive values for sigma1 and sigma2.")



# Paired samples
  Out6=capture.output(test2musm(n1=20,n2=20,xbar1=3,xbar2=4,s1=1,s2=1.5,
                                paired=TRUE,eqlvar=FALSE,unkwnsigmas=TRUE,
                                sigma1=NULL,sigma2=NULL,sdevdif=2,alpha=0.05,
                                xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                                sam1col="grey45",sam2col="grey5",thrcol="black"))

  expect_equal(Out6[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out6[3],"1 Above the threshold          Reject H_0 0.0375")

#Error Message
#f2 <- function() stop("Sample sizes n1 and n2 should be equal and sdevdif should be a positive value.")
#expect_error(f2())
#expect_error(f2(),"Sample sizes n1 and n2 should be equal and sdevdif should be a positive value.")

# Error Message for equal sample sizes

expect_error(test2musm(n1=20,n2=21,xbar1=3,xbar2=4,s1=1,s2=1.5,
                       paired=TRUE,eqlvar=FALSE,unkwnsigmas=TRUE,
                       sigma1=NULL,sigma2=NULL,sdevdif=NULL,alpha=0.05,
                       xlab="x",title="Two means summary: Hypothesis testing by Sv-plot2",
                       sam1col="grey45",sam2col="grey5",thrcol="black"),"Sample sizes n1 and n2 should be equal and sdevdif should be a positive value.")

})
