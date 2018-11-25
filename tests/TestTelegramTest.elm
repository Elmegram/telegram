module TestTelegramTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Json.Decode as Decode
import Telegram
import Telegram.Test as TeleTest
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    describe "TelegramTest"
        [ describe "makeMessage entity parsing"
            [ test "without entities" <|
                \_ ->
                    TeleTest.makeMessage "i contain no entities"
                        |> .entities
                        |> Expect.equalLists []
            , test "with bot command" <|
                \_ ->
                    TeleTest.makeMessage "i \tcontain a /botCommand"
                        |> .entities
                        |> Expect.equalLists
                            [ Telegram.BotCommand (Telegram.Bounds 13 11)
                            ]
            , test "with mention" <|
                \_ ->
                    TeleTest.makeMessage "i contain\n an @mention \t\n"
                        |> .entities
                        |> Expect.equalLists
                            [ Telegram.Mention (Telegram.Bounds 14 8)
                            ]
            ]
        ]
