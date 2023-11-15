test_that("Code blocks can be executed", {
  expect_equal(executeCode("2 * 2"), 4)
})

test_that("Code blocks can be executed", {
  expect_equal(executeCode("\n#we are doing a t.test now\nt.test(c(1,2),c(1,2))$p.value\n\n"),
               t.test(c(1,2),c(1,2))$p.value )
})

test_that("function is stopped when output is invalid", {
  expect_error(executeCode("2*2", ouput="nonexisting"))
})

test_that("Code blocks can be executed with html output", {
  expect_message(executeCode("2 * 2",output ="html"))
})


test_that("Improper code gives a warning", {
 expect_error(executeCode("2@@2"))
})
