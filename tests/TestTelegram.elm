module TestTelegram exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Json.Decode as Decode
import Telegram
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    describe "Telegram"
        [ describe "decode User"
            [ test "valid full User" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeUser
                        """
                        {
                            "id": 123,
                            "is_bot": false,
                            "first_name": "Kevin",
                            "last_name": "Spacey",
                            "username": "John Doe",
                            "language_code": "en"
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (Telegram.User
                                    (Telegram.makeTestId 123)
                                    False
                                    "Kevin"
                                    (Just "Spacey")
                                    (Just "John Doe")
                                    (Just "en")
                                )
                            )
            , test "minimal User" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeUser
                        """
                        {
                            "id": 59234,
                            "is_bot": false,
                            "first_name": "Minimalist"
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (Telegram.User
                                    (Telegram.makeTestId 59234)
                                    False
                                    "Minimalist"
                                    Nothing
                                    Nothing
                                    Nothing
                                )
                            )
            ]
        , describe "decode Chat"
            [ describe "valid chat type"
                (List.map
                    (\( field, constructor ) ->
                        test field <|
                            \_ ->
                                Decode.decodeString
                                    Telegram.decodeChat
                                    ("""
                                        {
                                            "id": 92533,
                                    """
                                        ++ ("\"type\": \"" ++ field ++ "\"")
                                        ++ """
                                        }
                                        """
                                    )
                                    |> Expect.equal
                                        (Ok
                                            (Telegram.Chat
                                                (Telegram.makeTestId 92533)
                                                constructor
                                            )
                                        )
                    )
                    [ ( "private", Telegram.Private )
                    , ( "group", Telegram.Group )
                    , ( "supergroup", Telegram.Supergroup )
                    , ( "channel", Telegram.Channel )
                    ]
                )
            , test "invalid chat type" <|
                \_ ->
                    expectError
                        (Decode.decodeString
                            Telegram.decodeChat
                            """
                                {
                                    "id": 286,
                                    "type": "i am an invalid type"
                                }
                            """
                        )
                        (expectErrorMessageToContain "i am an invalid type")
            ]
        , describe "decode TextMessage"
            [ describe "decode MessageEntity"
                [ describe "decode Bounds"
                    [ fuzz2 int int "valid" <|
                        \offset length ->
                            Decode.decodeString
                                Telegram.decodeBounds
                                ("""
                                {
                                    """
                                    ++ ("\"offset\": " ++ String.fromInt offset ++ ",\n")
                                    ++ ("\"length\": " ++ String.fromInt length ++ "\n")
                                    ++ """
                                }
                                """
                                )
                                |> Expect.equal (Ok <| Telegram.Bounds offset length)
                    ]
                , describe "valid simple types"
                    (List.map
                        (\( field, constructor ) ->
                            test field <|
                                \_ ->
                                    Decode.decodeString
                                        Telegram.decodeMessageEntity
                                        ("""
                                        {
                                        """
                                            ++ ("\"type\": \"" ++ field ++ "\",\n")
                                            ++ """
                                            "offset": 0,
                                            "length": 3
                                        }
                                        """
                                        )
                                        |> Expect.equal
                                            (Ok <|
                                                constructor <|
                                                    Telegram.Bounds 0 3
                                            )
                        )
                        [ ( "mention", Telegram.Mention )
                        , ( "hashtag", Telegram.Hashtag )
                        , ( "cashtag", Telegram.Cashtag )
                        , ( "bot_command", Telegram.BotCommand )
                        , ( "url", Telegram.Url )
                        , ( "email", Telegram.Email )
                        , ( "phone_number", Telegram.PhoneNumber )
                        , ( "bold", Telegram.Bold )
                        , ( "italic", Telegram.Italic )
                        , ( "code", Telegram.Code )
                        , ( "pre", Telegram.Pre )
                        ]
                    )
                , describe "TextLink"
                    [ test "valid full" <|
                        \_ ->
                            Decode.decodeString
                                Telegram.decodeMessageEntity
                                """
                            {
                                "type": "text_link",
                                "offset": 49,
                                "length": 13,
                                "url": "https://elm-lang.org/"
                            }
                            """
                                |> Expect.equal
                                    (Ok <|
                                        Telegram.TextLink
                                            (Url Url.Https "elm-lang.org" Nothing "/" Nothing Nothing)
                                            (Telegram.Bounds 49 13)
                                    )
                    , test "invalid url" <|
                        \_ ->
                            expectError
                                (Decode.decodeString
                                    Telegram.decodeMessageEntity
                                    """
                                    {
                                        "type": "text_link",
                                        "offset": 1,
                                        "length": 39,
                                        "url": "i am an invalid url"
                                    }
                                    """
                                )
                                (expectErrorMessageToContain "i am an invalid url")
                    ]
                , describe "TextMention"
                    [ test "valid full" <|
                        \_ ->
                            Decode.decodeString
                                Telegram.decodeMessageEntity
                                """
                                {
                                    "type": "text_mention",
                                    "offset": 394,
                                    "length": 1,
                                    "user": {
                                        "id": 123,
                                        "is_bot": false,
                                        "first_name": "Kevin"
                                    }
                                }
                                """
                                |> Expect.equal
                                    (Ok <|
                                        Telegram.TextMention
                                            (Telegram.User
                                                (Telegram.makeTestId 123)
                                                False
                                                "Kevin"
                                                Nothing
                                                Nothing
                                                Nothing
                                            )
                                            (Telegram.Bounds 394 1)
                                    )
                    , test "invalid user" <|
                        \_ ->
                            expectError
                                (Decode.decodeString
                                    Telegram.decodeMessageEntity
                                    """
                                {
                                    "type": "text_mention",
                                    "offset": 394,
                                    "length": 1,
                                    "user": "i am an invalid user"
                                }
                                """
                                )
                                (expectErrorMessageToContain "i am an invalid user")
                    ]
                ]
            , test "valid full" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeTextMessage
                        """
                        {
                            "message_id": 125907,
                            "date": 2348945,
                            "chat": {
                                "id": 91583,
                                "type": "supergroup"
                            },
                            "text": "send /me @peter",
                            "entities": [
                                {
                                    "type": "bot_command",
                                    "offset": 5,
                                    "length": 3
                                },
                                {
                                    "type": "mention",
                                    "offset": 9,
                                    "length": 6
                                }
                            ]
                        }
                        """
                        |> Expect.equal
                            (Ok <|
                                Telegram.TextMessage
                                    (Telegram.makeTestId 125907)
                                    2348945
                                    (Telegram.Chat (Telegram.makeTestId 91583) Telegram.Supergroup)
                                    "send /me @peter"
                                    [ Telegram.BotCommand (Telegram.Bounds 5 3)
                                    , Telegram.Mention (Telegram.Bounds 9 6)
                                    ]
                            )
            , test "valid minimal" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeTextMessage
                        """
                        {
                            "message_id": 1,
                            "date": 2,
                            "chat": {
                                "id": 3,
                                "type": "channel"
                            },
                            "text": "min",
                            "entities": []
                        }
                        """
                        |> Expect.equal
                            (Ok <|
                                Telegram.TextMessage
                                    (Telegram.makeTestId 1)
                                    2
                                    (Telegram.Chat (Telegram.makeTestId 3) Telegram.Channel)
                                    "min"
                                    []
                            )
            , test "invalid chat" <|
                \_ ->
                    expectError
                        (Decode.decodeString
                            Telegram.decodeTextMessage
                            """
                        {
                            "message_id": 1,
                            "date": 2,
                            "chat": "i am an invalid chat"
                            "text": "min",
                            "entities": []
                        }
                        """
                        )
                        (expectErrorMessageToContain "i am an invalid chat")
            , test "missing entity" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeTextMessage
                        """
                        {
                            "message_id": 1,
                            "date": 2,
                            "chat": {
                                "id": 3,
                                "type": "channel"
                            },
                            "text": "min"
                        }
                        """
                        |> Expect.equal
                            (Ok <|
                                Telegram.TextMessage
                                    (Telegram.makeTestId 1)
                                    2
                                    (Telegram.Chat (Telegram.makeTestId 3) Telegram.Channel)
                                    "min"
                                    []
                            )
            ]
        , describe "InlineQuery"
            [ test "valid full" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeInlineQuery
                        """
                        {
                            "id": "2348",
                            "from": {
                                "id": 59234,
                                "is_bot": false,
                                "first_name": "Minimalist"
                            },
                            "query": "search for me",
                            "offset": "23"
                        }
                        """
                        |> Expect.equal
                            (Ok <|
                                Telegram.InlineQuery
                                    (Telegram.makeTestStringId "2348")
                                    (Telegram.User (Telegram.makeTestId 59234)
                                        False
                                        "Minimalist"
                                        Nothing
                                        Nothing
                                        Nothing
                                    )
                                    "search for me"
                                    "23"
                            )
            , test "non-numerical id and offset" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeInlineQuery
                        """
                        {
                            "id": "i am the id",
                            "from": {
                                "id": 59234,
                                "is_bot": false,
                                "first_name": "Minimalist"
                            },
                            "query": "stuff",
                            "offset": "i am the offset"
                        }
                        """
                        |> Expect.equal
                            (Ok <|
                                Telegram.InlineQuery
                                    (Telegram.makeTestStringId "i am the id")
                                    (Telegram.User (Telegram.makeTestId 59234)
                                        False
                                        "Minimalist"
                                        Nothing
                                        Nothing
                                        Nothing
                                    )
                                    "stuff"
                                    "i am the offset"
                            )
            , test "invalid user" <|
                \_ ->
                    expectError
                        (Decode.decodeString
                            Telegram.decodeInlineQuery
                            """
                                {
                                    "id": "2348",
                                    "from": "invalid user",
                                    "query": "search for me",
                                    "offset": "23"
                                }
                                """
                        )
                        (expectErrorMessageToContain "invalid user")
            ]
        , describe "Update"
            [ test "valid minimal MessageUpdate" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeUpdate
                        """
                        {
                            "update_id": 15780,
                            "message": {
                                "message_id": 5280,
                                "date": 348739,
                                "chat": {
                                    "id": 570129,
                                    "type": "private"
                                },
                                "text": "i am the message text"
                            }
                        }
                        """
                        |> Expect.equal
                            (Ok <|
                                Telegram.Update
                                    (Telegram.makeTestId 15780)
                                <|
                                    Telegram.MessageUpdate <|
                                        Telegram.TextMessage
                                            (Telegram.makeTestId 5280)
                                            348739
                                            (Telegram.Chat (Telegram.makeTestId 570129) Telegram.Private)
                                            "i am the message text"
                                            []
                            )
            , test "valid minimal InlineQuery" <|
                \_ ->
                    Decode.decodeString
                        Telegram.decodeUpdate
                        """
                        {
                            "update_id": 15780,
                            "inline_query": {
                                "id": "43",
                                "from": {
                                    "id": 59234,
                                    "is_bot": false,
                                    "first_name": "Minimalist"
                                },
                                "query": "stuff",
                                "offset": "88"
                            }
                        }
                        """
                        |> Expect.equal
                            (Ok <|
                                Telegram.Update
                                    (Telegram.makeTestId 15780)
                                <|
                                    Telegram.InlineQueryUpdate <|
                                        Telegram.InlineQuery
                                            (Telegram.makeTestStringId "43")
                                            (Telegram.User (Telegram.makeTestId 59234)
                                                False
                                                "Minimalist"
                                                Nothing
                                                Nothing
                                                Nothing
                                            )
                                            "stuff"
                                            "88"
                            )
            ]
        ]


expectError : Result Decode.Error a -> (Decode.Error -> Expectation) -> Expectation
expectError producer test =
    case
        producer
    of
        Err err ->
            test err

        Ok _ ->
            Expect.fail "Expected Err, got Ok."


expectErrorMessageToContain : String -> Decode.Error -> Expectation
expectErrorMessageToContain contained error =
    let
        message =
            Decode.errorToString error
    in
    if String.contains contained message then
        Expect.pass

    else
        Expect.fail
            ("Expected error message to contain '"
                ++ contained
                ++ "', but was '"
                ++ message
                ++ "'."
            )
