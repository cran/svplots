test_that("svplot1 works", {
  set.seed(2021)
  X=matrix(rnorm(50,mean=1,sd=2))
  Out1=capture.output(svplot1(X,title="Sv-plot1",xlab="x",lbcol="grey5",lscol="grey60",rbcol="grey45",rscol="grey75"))
  expect_equal(Out1[2],"  Sample_Size Average  Left_SS Right_SS Sample_Variance")
  expect_equal(Out1[3],"1          50  1.0318 128.0086 123.5074           5.133")
})

