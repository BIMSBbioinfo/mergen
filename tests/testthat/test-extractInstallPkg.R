test_argument_validation(
  function_name = "check_install",
  argument_name = "package_name",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "extractInstallPkg",
  argument_name = "codes",
  argument_type = "string",
  allow_null = FALSE
)


test_that("extractInstallPkg extracts packages from code", {
  prompt <- "#code tells you to load a library\nlibrary(mergen)\n x<-5"
  expect_message(extractInstallPkg(prompt))
  })


test_that("extractInstallPkg extracts does not extract when package is not called with library", {
  prompt <- "#code tells you to load a library\nx<-5\n mergen::extractInstallPkg()"
  expect_no_message(extractInstallPkg(prompt))
})

