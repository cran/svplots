test_that("test2mu works", {
# Independent Small samples with unknown-unequal sigmas
  set.seed(2021)
  X1=matrix(rnorm(10,mean=3,sd=2))
  X2=matrix(rnorm(20,mean=4,sd=2.5))
  Out1=capture.output(test2mu(X1,X2,paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
                              sigma1=NULL,sigma2=NULL,alpha=0.05,
                              sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out1[2],"  Sample Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out1[3],"1      1   10  3.6070 1.8918 Below the threshold  Fail to reject H_0 0.5189")
  expect_equal(Out1[4],"2      2   20  4.1881 2.9387                                               ")

  # Independent small samples with unknown-equal variances
  set.seed(2021)
  X1=matrix(rnorm(10,mean=3,sd=2))
  X2=matrix(rnorm(20,mean=6,sd=2))
  Out2=capture.output(test2mu(X1,X2,paired=FALSE,eqlvar=TRUE,unkwnsigmas=TRUE,
                              sigma1=NULL,sigma2=NULL,alpha=0.05,
                              sam1col="grey5",sam2col="grey45",thrcol="black"))
  expect_equal(Out2[2],"  Sample Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out2[3],"1      1   10  3.6070 1.8918 Above the threshold          Reject H_0 0.0061")
  expect_equal(Out2[4],"2      2   20  6.1504 2.3509                                               ")

# Independent large samples with unknown sigmas
  set.seed(2021)
  X1=matrix(rnorm(50,mean=3,sd=2))
  X2=matrix(rnorm(30,mean=4,sd=2.5))
  Out3=capture.output(test2mu(X1,X2,paired=FALSE,eqlvar=FALSE,unkwnsigmas=TRUE,
                              sigma1=NULL,sigma2=NULL,alpha=0.05,
                              sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out3[2],"  Sample Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out3[3],"1      1   50  3.0318 2.2656 Below the threshold  Fail to reject H_0 0.7675")
  expect_equal(Out3[4],"2      2   30  3.1857 2.2488                                               ")


# Independent large samples with known sigmas
  set.seed(2021)
  X1=matrix(rnorm(50,mean=3,sd=2))
  X2=matrix(rnorm(30,mean=4,sd=2.5))
  Out4=capture.output(test2mu(X1,X2,paired=FALSE,eqlvar=FALSE,unkwnsigmas=FALSE,
                              sigma1=2,sigma2=1,alpha=0.05,
                              sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out4[2],"  Sample Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out4[3],"1      1   50  3.0318 2.2656 Below the threshold  Fail to reject H_0 0.6475")
  expect_equal(Out4[4],"2      2   30  3.1857 2.2488                                               ")

  set.seed(2021)
  X1=matrix(rnorm(50,mean=4,sd=2))
  X2=matrix(rnorm(30,mean=3,sd=2.5))
  Out5=capture.output(test2mu(X1,X2,paired=FALSE,eqlvar=FALSE,unkwnsigmas=FALSE,
                              sigma1=2,sigma2=4.920782,alpha=0.05,
                              sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out5[2],"  Sample Size Average  Stdev Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out5[3],"1      1   50  4.0318 2.2656   On the threshold          Reject H_0   0.05")
  expect_equal(Out5[4],"2      2   30  2.1857 2.2488                                              ")

# Error Message for positive sigmas
  expect_error(test2mu(X1,X2,paired=FALSE,eqlvar=FALSE,unkwnsigmas=FALSE,
                       sigma1=NULL,sigma2=NULL,alpha=0.05,
                       sam1col="grey5",sam2col="grey45",thrcol="black"),"Provide positive values for sigma1 and sigma2.")

# Paired samples
  set.seed(2021)
  X1=matrix(rnorm(10,mean=3,sd=2))
  X2=matrix(rnorm(10,mean=5,sd=2.5))
  Out6=capture.output(test2mu(X1,X2,paired=TRUE,eqlvar=FALSE,unkwnsigmas=TRUE,
                              sigma1=NULL,sigma2=NULL,alpha=0.05,
                              sam1col="grey5",sam2col="grey45",thrcol="black"))

  expect_equal(Out6[2], "  Sample Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out6[3],"1      1   10  3.6070 1.8918 Above the threshold          Reject H_0 0.0063")
  expect_equal(Out6[4],"2      2   10  6.2119 3.1519                                               ")

# Error Message for equal sample sizes
  set.seed(2021)
  X1=matrix(rnorm(10,mean=3,sd=2))
  X2=matrix(rnorm(12,mean=5,sd=2.5))
  expect_error(test2mu(X1,X2,paired=TRUE,eqlvar=FALSE,unkwnsigmas=TRUE,
                       sigma1=NULL,sigma2=NULL,alpha=0.05,
                       sam1col="grey5",sam2col="grey45",thrcol="black"),"Sample sizes n1 and n2 should be equal.")
})

