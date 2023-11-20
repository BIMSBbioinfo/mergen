testthat::test_that("A bot can self-correct",{
  myAgent <- list(name="testAgent",model="nomodel",type="chat",openai_api_key="none")
  res<-selfcorrect(myAgent,"Do I have a question?",attempts = 3)
  expect_equal(res$final.blocks$code, "\nplot(1:10)")
  expect_equal (res$final.response, "\n\nThe third response.The following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\nplot(1:10)```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n")
  expect_equal(res$init.response, "\n\nThe following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\ndata <- read.table(\"test.txt\", header = TRUE, sep = \"\\t\")\n```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n```\nnormalized.data <- scale(data, center = TRUE, scale = TRUE)\n```\n\nFinally, the normalized data will be used to do a Principal Component Analysis (PCA):\n\n```\npca <- princomp(normalized.data)\n```")
})



test_argument_validation(
  function_name = "selfcorrect",
  argument_name = "agent",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "selfcorrect",
  argument_name = "prompt",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "selfcorrect",
  argument_name = "context",
  argument_type = "string",
  allow_null = TRUE
)

test_argument_validation(
  function_name = "selfcorrect",
  argument_name = "attempts",
  argument_type = "count",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "selfcorrect",
  argument_name = "output.file",
  argument_type = "string",
  allow_null = FALSE
)

