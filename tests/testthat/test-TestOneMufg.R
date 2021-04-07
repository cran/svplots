test_that("test1mu works", {
# Small sample with unknown sigma
  set.seed(2021)
  X=matrix(rnorm(20,mean=3,sd=2))
  Out1=capture.output( test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=TRUE,sigma=NULL,xlab="x",
                            title="Single mean: Hypothesis testing by Sv-plot2",
                            samcol="grey5",popcol="grey45",thrcol="black"))
  expect_equal(Out1[2],"  Sample_Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out1[3],"1          20  3.7882 2.1775 Below the threshold  Fail to reject H_0 0.5608")

  set.seed(2021)
  X=matrix(rnorm(20,mean=3,sd=2))
  Out11=capture.output(test1mu(X,mu0=2.769125,alpha=0.05,unkwnsigma=TRUE,sigma=NULL,xlab="x",
                               title="Single mean: Hypothesis testing by Sv-plot2",
                               samcol="grey5",popcol="grey45",thrcol="black"))
  expect_equal(Out11[2], "  Sample_Size Average  Stdev Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out11[3],"1          20  3.7882 2.1775   On the threshold          Reject H_0   0.05")

# Large sample with unknown sigma
  set.seed(2021)
  X=matrix(rnorm(40,mean=3,sd=2))
  Out2=capture.output(test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=TRUE,sigma=NULL,xlab="x",
                           title="Single mean: Hypothesis testing by Sv-plot2",
                           samcol="grey5",popcol="grey45",thrcol="black"))
  expect_equal(Out2[2],"  Sample_Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out2[3],"1          40  2.8107 2.2848 Below the threshold  Fail to reject H_0 0.0564")

# Large sample with known sigma
  set.seed(2021)
  X=matrix(rnorm(40,mean=3,sd=2))
  Out3=capture.output(test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=FALSE,sigma=1.5,xlab="x",
                           title="Single mean: Hypothesis testing by Sv-plot2",
                           samcol="grey5",popcol="grey45",thrcol="black"))
  expect_equal(Out3[2],"  Sample_Size Average  Stdev  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out3[3],"1          40  2.8107 2.2848 Above the threshold          Reject H_0 0.0037")

# Error message
  expect_error(test1mu(X,mu0=3.5,alpha=0.05,unkwnsigma=FALSE,sigma=NULL,xlab="x",
                       title="Single mean: Hypothesis testing by Sv-plot2",
                       samcol="grey5",popcol="grey45",thrcol="black"),"Provide a positive value for sigma.")
})



