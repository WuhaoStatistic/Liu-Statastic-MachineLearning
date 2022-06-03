test_that("lab5 test",
{
  expect_that(round(as.numeric(lab5class("se-7")$getdata()[1,1]),2),equals(1))
  expect_that(round(as.numeric(lab5class("fi-8")$getdata()[1,1]),2),equals(1))
  expect_that(round(as.numeric(lab5class("se-7")$getdata()[2,1]),2),equals(2))
  expect_that(round(as.numeric(lab5class("se-7")$getdata()[3,1]),2),equals(3))
  expect_that(round(as.numeric(lab5class("se-7")$getdata()[4,1]),2),equals(4))
  expect_that(round(as.numeric(lab5class("se-7")$getdata()[5,1]),2),equals(5))
  expect_that(round(as.numeric(lab5class("se-7")$getdata()[6,1]),2),equals(6))
  expect_error(lab5class("")$getdata(),"available dataset are fi-8, ch-8, no-7, no-4, dk-7, se-7, se-4, us-4, gl-7")
  expect_error(lab5class('f')$getdata(),"available dataset are fi-8, ch-8, no-7, no-4, dk-7, se-7, se-4, us-4, gl-7")
  expect_error(lab5class()$getdata(),'argument "Name" is missing, with no default')
  expect_error(lab5class("Fi-9")$getdata(),"available dataset are fi-8, ch-8, no-7, no-4, dk-7, se-7, se-4, us-4, gl-7")
})