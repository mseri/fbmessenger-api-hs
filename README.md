# FBMessenger API

High-level bindings to the Messenger Platform API based on [servant](https://haskell-servant.github.io/) library. We try to maintain the structure compatible with [telegram-api](https://github.com/klappvisor/haskell-telegram-api).

# Usage

... todo ...

Run `stack ghci` then copy and paste the following

    :m +Network.HTTP.Client
    :m +Network.HTTP.Client.TLS
    :m +Data.Text

    let manager = newManager tlsManagerSettings
    manager >>= \m -> subscribedApps (Just $ Token $ Data.Text.pack "souncazzo") m

You should get the response: 

    Left (FailureResponse {responseStatus = Status {statusCode = 400, statusMessage = "Bad Request"}, responseContentType = application/json, responseBody = "{\"error\":{\"message\":\"Invalid OAuth access token.\",\"type\":\"OAuthException\",\"code\":190,\"fbtrace_id\":\"ESxHmUos2B+\"}}"})

# Contribution

1. Fork repository
2. Do some changes
3. Create pull request
4. Wait for CI build and review

You can use stack to build project

    stack build

To run test you have to create your own bot. Keep your access token safe!

    stack test --test-arguments "$BOT_TOKEN"

_(this will need review)_

# TODO

- Still everything!
- Set up Travis or whatever for testing
