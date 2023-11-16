test_argument_validation(
  function_name = "extractCode",
  argument_name = "text",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "extractCode",
  argument_name = "delimiter",
  argument_type = "string",
  allow_null = FALSE
)

test_that("Code can be extracted, but not the text.", {
  resp <- "To conduct a t-test in R, you use the `t.test()` function. The two main types of t-tests are:\n- Independent t-tests (comparison between two different groups)\n- Paired t-tests (comparison of two variables for a single group)\n\nHere's how to do them:\n\nFor an independent t-test:\n\n```R\n# Here, 'variable1' and 'variable2' are two different groups you want to compare.\nresult <- t.test(variable1, variable2)\nprint(result)\n```\n\nFor a paired t-test:\n\n```R\n# Here, 'pair1' and 'pair2' are paired measurements from the same group.\nresult <- t.test(pair1, pair2, paired = TRUE)\nprint(result)\n```\n\nThis will print the t-statistic, degrees of freedom, p-value, and confidence interval.\n\nPlease replace `variable1`, `variable2`, `pair1`, `pair2` with your actual data vectors.\n\nAlso, keep in mind that a t-test assumes your data is normally distributed, or approximately so. If your data do not meet this assumption, you may need to apply a different statistical test."
  clean_resp <- clean_code_blocks(resp)
  actual_code <- extractCode(clean_resp)$code
  expect_equal(actual_code,"\n# Here, 'variable1' and 'variable2' are two different groups you want to compare.\nresult <- t.test(variable1, variable2)\nprint(result)\n\n\n# Here, 'pair1' and 'pair2' are paired measurements from the same group.\nresult <- t.test(pair1, pair2, paired = TRUE)\nprint(result)\n")
})

test_that("Not only default delimiter works",{
  resp <-"Now my code looks different.\n111x=5111\n but this should not matter."
  actual_code <- extractCode(resp,delimiter = "111")$code
  expect_equal("x=5",actual_code)
})

test_that("Forgetting proper delimiter results in no codeblock",{
  resp <-"Now my code looks different.\n111x=5111\n but this should not matter."
  actual_code <- extractCode(resp)$code
  expect_equal("",actual_code)
})

test_that("Text is cleaned of codeblock",{
  resp <-"Now my code looks different.\n111x=5111\n but this should not matter."
  actual_text <- extractCode(resp,delimiter="111")$text
  expect_equal("Now my code looks different.\n\n\n but this should not matter.",actual_text)
})

test_that("Text is cleaned of codeblock",{
  resp <-"My code looks good.\n```x=5```\n How nice is that."
  actual_text <- extractCode(resp,delimiter="```")$text
  expect_equal("My code looks good.\n\n\n How nice is that.",actual_text)
})

