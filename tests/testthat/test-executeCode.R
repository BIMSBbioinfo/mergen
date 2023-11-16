test_argument_validation(
  function_name = "executeCode",
  argument_name = "code",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "executeCode",
  argument_name = "output",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "executeCode",
  argument_name = "output.file",
  argument_type = "string",
  allow_null = TRUE
)


test_that("Code blocks can be executed", {
  expect_equal(executeCode("2 * 2"), 4)
})

test_that("Code blocks can be executed", {
  expect_equal(executeCode("\n#we are doing a t.test now\nt.test(c(1,2),c(1,2))$p.value\n\n"),
               t.test(c(1,2),c(1,2))$p.value )
})

test_that("function is stopped when output is invalid", {
  expect_error(executeCode("2*2", output="nonexisting"))
})

test_that("Code blocks can be executed with html output", {
  expect_message(executeCode("2 * 2",output ="html"))
})

test_that("Improper code gives an error", {
 expect_error(executeCode("2@@2"))
})

test_that("Warnings are seen and saved",{
  expect_warning(executeCode("warning()"))
})
