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

test_that("Code blocks are clean and text remains", {
  test_obj <- "I have some nice packages for you```r\ninstall.packages(mergen)\ncleaned<-clean_code_blocks(code)"
  expect_equal(clean_code_blocks(test_obj), "I have some nice packages for you```\n\ncleaned<-clean_code_blocks(code)")
})

test_that("Code blocks are clean and text remains, so do the R and r letters within code", {
  test_obj <- "I have some nice packages for you```r\ninstall.packages(mergen)\ncleaned_r<-clean_code_blocks(Rcode)"
  expect_equal(clean_code_blocks(test_obj), "I have some nice packages for you```\n\ncleaned_r<-clean_code_blocks(Rcode)")
})

