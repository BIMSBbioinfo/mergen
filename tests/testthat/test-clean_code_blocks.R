test_that("Code blocks are clean", {
  test_obj <- "```R\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)"
  expect_equal(clean_code_blocks(test_obj), "```\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)")
})


test_that("Code blocks are clean", {
  test_obj <- "```r\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)"
  expect_equal(clean_code_blocks(test_obj), "```\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)")
})

test_that("Code blocks are clean", {
  test_obj <- "```{r}\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)"
  expect_equal(clean_code_blocks(test_obj), "```\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)")
})

test_that("Code blocks are clean", {
  test_obj <- "```{R}\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)"
  expect_equal(clean_code_blocks(test_obj), "```\nlibrary(mergen)\ncleaned<-clean_code_blocks(code)")
})


test_that("Code blocks are clean", {
  test_obj <- "```\ninstall.packages(mergen)\ncleaned<-clean_code_blocks(code)"
  expect_equal(clean_code_blocks(test_obj), "```\n\ncleaned<-clean_code_blocks(code)")
})
