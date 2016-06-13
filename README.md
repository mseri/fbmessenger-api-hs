# FBMessenger API

High-level bindings to the [Messenger Platform API](https://developers.facebook.com/docs/messenger-platform/) based on [servant](https://haskell-servant.github.io/) library.
We try to maintain the overall structure compatible with [telegram-api](https://github.com/klappvisor/haskell-telegram-api).

This library is alpha software and the api could change to improve composability, ergonomicity and ease of use. We recommend using stack for dealing with this library.

<!-- Useful links: 
- [servant tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial)
- [simple python bindings for the messenger api](https://github.com/geeknam/messengerbot)
- [bytemyapp](http://bitemyapp.com/archive.html)
- [aeson tutorial](https://artyom.me/aeson)
- [haskell is easy](https://haskelliseasy.readthedocs.io/en/latest/)
/ -->

# Usage

Before being able to test and use the bot, you will need to verify your key. 
The example app in `example-app/example.hs` contains a servant server that implements the verification and a trivial echo-server.
You can run it with
    
    VERIFY_TOKEN="your_token_goes_here" stack exec example  

and pass it some data (here assuming you have `httpie` installed)

    http get 'localhost:3000/webhook/?hub.verify_token=your_token_goes_here&hub.challenge=test'
    http post :3000/webhook < test-files/wsTextMessageRequest.json

Otherwise run `stack ghci` then copy and paste the following

    :m +Network.HTTP.Client
    :m +Network.HTTP.Client.TLS
    :m +Data.Text
    
    let token = Token $ Data.Text.pack "your_token_goes_here"
    let manager = newManager tlsManagerSettings
    manager >>= \m -> subscribedApps $ Just token m

You should get a positive response or (in case of inactive token): 

    Left (FailureResponse {responseStatus = Status {statusCode = 400, statusMessage = "Bad Request"}, responseContentType = application/json, responseBody = "{\"error\":{\"message\":\"Invalid OAuth access token.\",\"type\":\"OAuthException\",\"code\":190,\"fbtrace_id\":\"ESxHmUos2B+\"}}"})

# Contribution

1. Fork repository
2. Do some changes
3. Create pull request
4. Wait for CI build and review

You can use stack to build project

    stack build

To run tests

    stack test

# TODO

- Tests for the network part of the api (hard, requires a bot setted up and permanently running)
- Cleanup Webhooks API Requests and add higher level helpers
