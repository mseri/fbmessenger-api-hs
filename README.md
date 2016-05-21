# FBMessenger API

High-level bindings to the [Messenger Platform API](https://developers.facebook.com/docs/messenger-platform/) based on [servant](https://haskell-servant.github.io/) library.
We try to maintain the structure compatible with [telegram-api](https://github.com/klappvisor/haskell-telegram-api).

Useful links: 
- [servant tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial)
- [simple python bindings for the messenger api](https://github.com/geeknam/messengerbot)

# Note per framp

Ancora non mi è chiaro al 100% come strutturare la libreria. 
Per ora ho convertito quello che stavo facendo in una implementazione delle Send API.
Piano piano la implemento tutta.

Per il resto pensavo:

Rimettiamo Data e ci mettiamo i tipi delle chiamate che Facebook fa al websocket, 
così il bot si riduce a importare la libreria e implementare l'api che risponde alle chiamate di fb 
usando i tipi che abbiamo già preimpostato nella libreria (a quel punto uno può usare quello che vuole,
tanto servant-server quanto qualunque altro framework).

Che dici? 

# Usage

Before being able to test and use the bot, you will need to verify your key. 
The example app in `example-app/example.hs` contains a servant server that implements the verification.
You can run it with

     VERIFY_TOKEN="your_token_goes_here" stack exec example  

Run `stack ghci` then copy and paste the following

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

_(this will need review)_

# TODO

- Image Message (multipart with upload)
- Tests
- Webhooks API Requests Body types (NOTE: the body for these will be an array of an appropriate sum type... annoying)
- Set up Travis, CircleCI or whatever is supported by bitbucket for testing
