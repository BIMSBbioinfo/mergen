test_that("Code blocks can be executed", {
  expect_equal(executeCode("2 * 2"), 4)
})

test_that("Code blocks can be executed", {
  expect_equal(executeCode("\n#we are doing a t.test now\nt.test(c(1,2),c(1,2))$p.value\n\n"), t.test(c(1,2),c(1,2))$p.value )
})
