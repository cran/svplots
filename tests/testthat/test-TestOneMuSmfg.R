test_that("testimusm works", {
# Small sample with unknown sigma
  Out1=capture.output(test1musm(n=8,xbar=3,s=2,mu0=4.5,alpha=0.05, unkwnsigma=TRUE,sigma=NULL,xlab="x",
                             title="Single mean summary: Hypothesis testing by Sv-plot2",
                             samcol="grey5",popcol="grey45",thrcol="black"))
  expect_equal(Out1[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out1[3],"1 Below the threshold  Fail to reject H_0 0.0716")

  Out11=capture.output(test1musm(n=8,xbar=3,s=2,mu0=4.672042,alpha=0.05, unkwnsigma=TRUE,sigma=NULL,xlab="x",
                 title="Single mean summary: Hypothesis testing by Sv-plot2",
                 samcol="grey5",popcol="grey45",thrcol="black"))
  expect_equal(Out11[2],"  Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out11[3],"1   On the threshold          Reject H_0   0.05")

# Lage sample with unknown sigma
  Out2=capture.output(test1musm(n=40,xbar=3,s=2,mu0=4.5,alpha=0.05, unkwnsigma=TRUE,sigma=NULL,xlab="x",
                             title="Single mean summary: Hypothesis testing by Sv-plot2",
                             samcol="grey5",popcol="grey45",thrcol="black"))

  expect_equal(Out2[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out2[3],"1 Above the threshold          Reject H_0      0")

# Lage sample with known sigma
  Out3=capture.output(test1musm(n=40,xbar=3,s=2,mu0=4.5,alpha=0.05, unkwnsigma=FALSE,sigma=4,xlab="x",
                             title="Single mean summary: Hypothesis testing by Sv-plot2",
                             samcol="grey5",popcol="grey45",thrcol="black"))

  expect_equal(Out3[2],"   Intersection_Point Decision_by_Svplot2 pvalue")
  expect_equal(Out3[3],"1 Above the threshold          Reject H_0 0.0177")

# Error message
  expect_error(test1musm(n=40,xbar=3,s=2,mu0=4.5,alpha=0.05, unkwnsigma=FALSE,sigma=NULL,xlab="x",
                         title="Single mean summary: Hypothesis testing by Sv-plot2",
                         samcol="grey5",popcol="grey45",thrcol="black"),"Provide a positive value for sigma.")

})
