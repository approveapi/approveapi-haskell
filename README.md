# approveapi-haskell

Haskell API bindings for the [ApproveAPI HTTP API](https://approveapi.com).

*ApproveAPI is a simple API to request a user's real-time approval on anything via email, sms + mobile push.*

## Getting Started

To get started, we create a config:

```haskell
import qualified ApproveApi as A
...
config <- (`A.addAuthMethod` A.AuthBasicApiKey "sk_test_yourapikeyhere" "")
          <$> A.newConfig 
```

Now we can make API calls. For example, let's send an approval prompt to confirm a financial transaction.

```haskell
import qualified Network.HTTP.Client.TLS as NH
...
let body = "Your AcmeBank credit card was just charged $3249.99 at APPLE STORE,\
\ San Francisco. Authorize this transaction?"
let prompt = (A.mkCreatePromptRequest "alice@approveapi.com" body) {
                A.createPromptRequestLongPoll = Just True,
		            A.createPromptRequestApproveText = Just "Authorize",
		            A.createPromptRequestRejectText = Just "Reject"
              }
let createPromptRequest = A.createPrompt prompt

mgr <- NH.newTlsManager
createPromptResponse <- A.dispatchMime mgr config createPromptRequest

flip mapM_ createPromptResponse (\r ->
  case A.promptAnswer r of 
    Nothing -> putStrLn $ "No response yet"
    Just answer -> case A.promptAnswerResult answer of
      True -> putStrLn $ "Request approved"
      False -> putStrLn $ "Request rejected"
  )
```

## Documentation

Full documentation is available here: [approveapi.com/docs](https://www.approveapi.com/docs/?haskell).



