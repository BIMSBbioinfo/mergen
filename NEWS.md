# mergen Version 0.2.0 _2023-12-29_
  * URLs in DESCRIPTION added 
  * new feature: now users can specify openai-like APIs by providing url and setting
    agent name as "generic" in setupAgent() function
  * new feature: clean_code_blocks() now automatically runs when extractCode() is called
  * new feature: sendPrompt() now can send previous message correspondence with LLM APIs via "previous.msgs" argument
  * fix: typos in setupAgent fixed
  * fix: sendPrompt fixed which was broken due to openai API changes
  * fix: extractInstallPkg can now deal with require() calls in code blocks
  * updated help pages accordingly
