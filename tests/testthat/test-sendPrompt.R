test_argument_validation(
  function_name = "sendPrompt",
  argument_name = "agent",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "sendPrompt",
  argument_name = "prompt",
  argument_type = "string",
  allow_null = FALSE
)

test_argument_validation(
  function_name = "sendPrompt",
  argument_name = "context",
  argument_type = "string",
  allow_null = TRUE
)

test_argument_validation(
  function_name = "sendPrompt",
  argument_name = "return.type",
  argument_type = "string",
  allow_null = FALSE
)

test_that("invalid LLM gives error",{
  agent<-list(name="nollm",model="gpt-4",type="chat",openai_api_key=Sys.getenv("OPENAI_API_KEY"))
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
})

test_that("openai-agent needs to have all correct fields",{
  # no key
  agent<-list(name="openai",model="gpt-4",type="chat")
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
  # no type
  agent<-list(name="openai",model="gpt-4",openai_api_key = "key")
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
  # no model
  agent<-list(name="openai",type="chat",openai_api_key = "key")
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
})

test_that("userAgent needs to have all correct fields",{
  # no key
  agent<-list(name="userAgent",model="llama2",url="example.com",headers=list())
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
  # no model
  agent<-list(name="userAgent",ai_api_key="key",url="example.com",headers=list())
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
  # no url
  agent<-list(name="userAgent",model="llama2",ai_api_key="key",headers=list())
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
  # no headers
  agent<-list(name="userAgent",model="llama2",ai_api_key="key",url="example.com")
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
})




test_that("testagent gives 3 responses when called 3 times",{
  agent<-list(name="testAgent",model="gpt-4",type="chat",openai_api_key=Sys.getenv("OPENAI_API_KEY"))
  expect_equal(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"),"\n\nThe following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\ndata <- read.table(\"test.txt\", header = TRUE, sep = \"\\t\")\n```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n```\nnormalized.data <- scale(data, center = TRUE, scale = TRUE)\n```\n\nFinally, the normalized data will be used to do a Principal Component Analysis (PCA):\n\n```\npca <- princomp(normalized.data)\n```")
  expect_equal(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"),"\n\nThe second response.The following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\ndata <- read.table(\"test.txt\", header = TRUE, sep = \"\\t\")\n```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n```\nnormalized.data <- scale(data, center = TRUE, scale = TRUE)\n```\n\nFinally, the normalized data will be used to do a Principal Component Analysis (PCA):\n\n```\npca <- princomp(normalized.data)\n```")
  expect_equal(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"),"\n\nThe third response.The following R code will read the file called \"test.txt\", normalize the table and do PCA. First, the code will read the file into an R data frame: \n\n```\nplot(1:10)```\n\nNext, the data will be normalized to the range of 0 to 1:\n\n")
})



test_that("error when non-existing url is used.",{
  agent<-list(name="userAgent",model="llama2",url="example.com",taks="completion",key="key")
  expect_error(sendPrompt(agent=agent,prompt="this is my prompt",return.type = "text"))
})

