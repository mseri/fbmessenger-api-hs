# FBMessenger API

[![Build Status](https://travis-ci.org/mseri/fbmessenger-api-hs.svg?branch=master)](https://travis-ci.org/mseri/fbmessenger-api-hs)
![Hackage](https://img.shields.io/hackage/v/fbmessenger-api.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/fbmessenger-api.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

High-level bindings to the [Messenger Platform API](https://developers.facebook.com/docs/messenger-platform/) based on [servant](https://haskell-servant.github.io/) library.
We try to maintain the overall structure compatible with [telegram-api](https://github.com/klappvisor/haskell-telegram-api).

There was an incongruence between the spec and the actual serialization of the webhook requests that became apparent when testing an actual messenger bot. For this reason you should **use only versions of the library that are `>= 0.1.1`**!

This library is alpha software and the API design could change to improve composability, ergonomicity and ease of use. We recommend using `stack` for dealing with this library (you will need to add it to the `extra-deps` in `stack.yaml`).

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

```{.bash}
VERIFY_TOKEN="your_token_goes_here" stack exec example  
```

and pass it some data (here assuming you have `httpie` installed)

```{.bash}
http get 'localhost:3000/webhook/?hub.verify_token=your_token_goes_here&hub.challenge=test'
http post :3000/webhook < test-files/wsTextMessageRequest.json
```

Otherwise run `stack ghci` then copy and paste the following

```{.haskell}
:m +Network.HTTP.Client
:m +Network.HTTP.Client.TLS
:m +Data.Text

let token = Token $ Data.Text.pack "your_token_goes_here"
let manager = newManager tlsManagerSettings
manager >>= \m -> subscribedApps $ Just token m
```

You should get a positive response or (in case of inactive token): 

```{.haskell}
Left (FailureResponse {responseStatus = Status {statusCode = 400, statusMessage = "Bad Request"}, responseContentType = application/json, responseBody = "{\"error\":{\"message\":\"Invalid OAuth access token.\",\"type\":\"OAuthException\",\"code\":190,\"fbtrace_id\":\"ESxHmUos2B+\"}}"})
```

# Contribution

1. Fork repository
2. Do some changes
3. Create pull request
4. Wait for CI build and review

You can use stack to build the project

    stack build

To run tests

    stack test

# TODO

- Tests for the network part of the api (hard, requires a bot setted up and permanently running)
- Cleanup Webhooks API Requests and add higher level helpers
