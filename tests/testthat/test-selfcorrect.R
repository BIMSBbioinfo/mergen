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
