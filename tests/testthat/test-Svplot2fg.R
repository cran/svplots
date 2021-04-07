test_that("svplot2 works", {
  set.seed(2021)
  X=matrix(rnorm(50,mean=1,sd=2))
  Out1=capture.output(svplot2(X,xlab="x",lbcol="grey5",lsdcol="grey60",rbcol="grey45",rsdcol="grey75"))
  expect_equal(Out1[2],"  Sample_Size Average  Left_SS Right_SS Sample_Variance")
  expect_equal(Out1[3],"1          50  1.0318 128.0086 123.5074           5.133")
})


