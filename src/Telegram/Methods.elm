module Telegram.Methods exposing
    ( Token
    , answerCallbackQuery
    , answerInlineQuery
    , decodeToken
    , deleteWebhook
    , getMe
    , getUpdates
    , sendMessage
    , stringFromToken
    )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Telegram
import Url exposing (Url)


type Token
    = Token String


stringFromToken : Token -> String
stringFromToken (Token token) =
    token


decodeToken : Decode.Decoder Token
decodeToken =
    Decode.string |> Decode.map Token



-- METHODS


getMe : Token -> (Result Http.Error Telegram.User -> msg) -> Cmd msg
getMe token tagger =
    Http.get
        { url = getMeUrl token |> Url.toString
        , expect = Http.expectJson tagger (Decode.field "result" Telegram.decodeUser)
        }


getMeUrl : Token -> Url
getMeUrl =
    getMethodUrl "getMe"


deleteWebhook : Token -> (Result String () -> msg) -> Cmd msg
deleteWebhook token tagger =
    Http.post
        { url = deleteWebhookUrl token |> Url.toString
        , expect = expectJsonOk tagger
        , body = Http.emptyBody
        }


deleteWebhookUrl : Token -> Url
deleteWebhookUrl =
    getMethodUrl "deleteWebhook"


expectJsonOk : (Result String () -> msg) -> Http.Expect msg
expectJsonOk tagger =
    Http.expectJson
        (\result ->
            Result.mapError httpErrorToString result
                |> Result.andThen
                    (\response ->
                        if response.ok then
                            Ok ()

                        else
                            case response.description of
                                Just description ->
                                    Err ("Response JSON indicated error:\n" ++ description)

                                Nothing ->
                                    Err "Response JSON indicated error, but no description was given."
                    )
                |> tagger
        )
        (Decode.map2 (\ok description -> { ok = ok, description = description })
            (Decode.field "ok" Decode.bool)
            (Decode.maybe (Decode.field "description" Decode.string))
        )


getUpdates : Token -> Int -> (Result Http.Error (List Telegram.Update) -> msg) -> Cmd msg
getUpdates token offset tagger =
    Http.post
        { url = getUpdatesUrl token |> Url.toString
        , expect =
            Http.expectJson
                tagger
                (Decode.field "result" <| Decode.list Telegram.decodeUpdate)
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "offset", Encode.int offset )
                    ]
                )
        }


getUpdatesUrl : Token -> Url
getUpdatesUrl =
    getMethodUrl "getUpdates"


sendMessage : Token -> Telegram.SendMessage -> (Result String () -> msg) -> Cmd msg
sendMessage token toSend tagger =
    Http.post
        { url = sendMessageUrl token |> Url.toString
        , expect = expectJsonOk tagger
        , body = Http.jsonBody (Telegram.encodeSendMessage toSend)
        }


sendMessageUrl : Token -> Url
sendMessageUrl =
    getMethodUrl "sendMessage"


answerInlineQuery : Token -> Telegram.AnswerInlineQuery -> (Result String () -> msg) -> Cmd msg
answerInlineQuery token toSend tagger =
    Http.post
        { url = answerInlineQueryUrl token |> Url.toString
        , expect = expectJsonOk tagger
        , body = Http.jsonBody (Telegram.encodeAnswerInlineQuery toSend)
        }


answerInlineQueryUrl : Token -> Url
answerInlineQueryUrl =
    getMethodUrl "answerInlineQuery"


answerCallbackQuery : Token -> Telegram.AnswerCallbackQuery -> (Result String () -> msg) -> Cmd msg
answerCallbackQuery token toSend tagger =
    Http.post
        { url = answerCallbackQueryUrl token |> Url.toString
        , expect = expectJsonOk tagger
        , body = Http.jsonBody (Telegram.encodeAnswerCallbackQuery toSend)
        }


answerCallbackQueryUrl : Token -> Url
answerCallbackQueryUrl =
    getMethodUrl "answerCallbackQuery"


getMethodUrl : String -> Token -> Url
getMethodUrl method token =
    let
        baseUrl =
            getBaseUrl token
    in
    { baseUrl | path = baseUrl.path ++ "/" ++ method }


getBaseUrl : Token -> Url
getBaseUrl token =
    { protocol = Url.Https
    , host = "api.telegram.org"
    , port_ = Nothing
    , path = "/bot" ++ stringFromToken token
    , query = Nothing
    , fragment = Nothing
    }



-- HELPERS


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Requested url '" ++ url ++ "' is invalid."

        Http.Timeout ->
            "Timed out."

        Http.NetworkError ->
            "Network error."

        Http.BadStatus status ->
            "Response had bad status " ++ String.fromInt status ++ "."

        Http.BadBody bodyError ->
            "Response body had an issue:\n" ++ bodyError
