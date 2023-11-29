test_that("Non-existing files can't be found", {
  expect_error(fileHeaderPrompt("nofile.xlsx"))
  expect_warning(expect_error(fileHeaderPrompt("nofile.txt")))
})

test_that("fileheaders can be read, but not too much",{
  res <- fileHeaderPrompt(system.file("extdata", "fileheader.xlsx", package = "mergen"))
  expect_true(grepl("\ntest\treading\tfile\theader\nreading\tis\tfun\tright\n" , res, fixed = TRUE))
  expect_false(grepl("But\tdont\tread\tthis" , res, fixed = TRUE))
  res2 <- fileHeaderPrompt(system.file("extdata", "fileheader.txt", package = "mergen"))
  expect_true(grepl("\ntest\treading\tfile\theader\nreading\tis\tfun\tright\n" , res2, fixed = TRUE))
  expect_false(grepl("But\tdont\tread\tthis" , res2, fixed = TRUE))
})
