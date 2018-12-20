module Telegram exposing
    ( AnswerCallbackQuery
    , AnswerInlineQuery
    , ArticleUrl(..)
    , Bounds
    , CallbackQuery
    , Chat
    , ChatType(..)
    , InlineKeyboard
    , InlineKeyboardButton(..)
    , InlineKeyboardRow
    , InlineQuery
    , InlineQueryResult(..)
    , InlineQueryResultArticle
    , InputMessageContent(..)
    , InputTextMessageContent
    , MessageEntity(..)
    , MessageReplyMarkup(..)
    , ParseMode(..)
    , SendMessage
    , TextMessage
    , Update
    , UpdateContent(..)
    , UpdateId
    , User
    , decodeBounds
    , decodeChat
    , decodeInlineQuery
    , decodeMessageEntity
    , decodeTextMessage
    , decodeUpdate
    , decodeUser
    , encodeAnswerCallbackQuery
    , encodeAnswerInlineQuery
    , encodeBounds
    , encodeChat
    , encodeInlineQuery
    , encodeMessageEntity
    , encodeSendMessage
    , encodeTextMessage
    , encodeUpdate
    , encodeUser
    , getNextOffset
    , makeTestId
    , makeTestStringId
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)



-- UPDATE


type alias Update =
    { update_id : UpdateId
    , content : UpdateContent
    }


type alias UpdateId =
    Id UpdateTag


type UpdateTag
    = UpdateTag


type UpdateContent
    = MessageUpdate TextMessage
    | InlineQueryUpdate InlineQuery
    | CallbackQueryUpdate CallbackQuery


getNextOffset : UpdateId -> Int
getNextOffset updateId =
    case updateId of
        Id num ->
            num + 1


decodeUpdate : Decode.Decoder Update
decodeUpdate =
    Decode.map2
        Update
        (Decode.field "update_id" decodeId)
        (Decode.oneOf
            [ Decode.field "message" decodeTextMessage |> Decode.map MessageUpdate
            , Decode.field "inline_query" decodeInlineQuery |> Decode.map InlineQueryUpdate
            , Decode.field "callback_query" decodeCallbackQuery |> Decode.map CallbackQueryUpdate
            ]
        )


encodeUpdate : Update -> Encode.Value
encodeUpdate update =
    let
        content =
            case update.content of
                MessageUpdate textMessage ->
                    ( "message", encodeTextMessage textMessage )

                InlineQueryUpdate inlineQuery ->
                    ( "inline_query", encodeInlineQuery inlineQuery )

                CallbackQueryUpdate callbackQuery ->
                    ( "callback_query", encodeCallbackQuery callbackQuery )
    in
    Encode.object
        [ ( "update_id", encodeId update.update_id )
        , content
        ]


type alias TextMessage =
    { message_id : Id MessageTag
    , date : Int
    , chat : Chat
    , text : String
    , entities : List MessageEntity
    }


type MessageTag
    = MessageTag


type MessageEntity
    = Mention Bounds
    | Hashtag Bounds
    | Cashtag Bounds
    | BotCommand Bounds
    | Url Bounds
    | Email Bounds
    | PhoneNumber Bounds
    | Bold Bounds
    | Italic Bounds
    | Code Bounds
    | Pre Bounds
    | TextLink Url Bounds
    | TextMention User Bounds


type alias Bounds =
    { offset : Int
    , length : Int
    }


decodeBounds : Decode.Decoder Bounds
decodeBounds =
    Decode.map2
        Bounds
        (Decode.field "offset" Decode.int)
        (Decode.field "length" Decode.int)


encodeBounds : Bounds -> List ( String, Encode.Value )
encodeBounds bounds =
    [ ( "offset", Encode.int bounds.offset )
    , ( "length", Encode.int bounds.length )
    ]


decodeMessageEntity : Decode.Decoder MessageEntity
decodeMessageEntity =
    let
        simple =
            Decode.map2
                Tuple.pair
                (Decode.field "type" Decode.string)
                decodeBounds
                |> Decode.andThen
                    (\( type_, bounds ) ->
                        case type_ of
                            "mention" ->
                                Decode.succeed (Mention bounds)

                            "hashtag" ->
                                Decode.succeed (Hashtag bounds)

                            "cashtag" ->
                                Decode.succeed (Cashtag bounds)

                            "bot_command" ->
                                Decode.succeed (BotCommand bounds)

                            "url" ->
                                Decode.succeed (Url bounds)

                            "email" ->
                                Decode.succeed (Email bounds)

                            "phone_number" ->
                                Decode.succeed (PhoneNumber bounds)

                            "bold" ->
                                Decode.succeed (Bold bounds)

                            "italic" ->
                                Decode.succeed (Italic bounds)

                            "code" ->
                                Decode.succeed (Code bounds)

                            "pre" ->
                                Decode.succeed (Pre bounds)

                            wrongType ->
                                Decode.fail
                                    ("Expected a simple type, but the field 'type' contained '"
                                        ++ wrongType
                                        ++ "'."
                                    )
                    )

        textLink =
            Decode.map3
                (\type_ bounds url -> ( type_, bounds, url ))
                (Decode.field "type" Decode.string)
                decodeBounds
                (Decode.field "url" Decode.string)
                |> Decode.andThen
                    (\( type_, bounds, urlString ) ->
                        case Url.fromString urlString of
                            Just url ->
                                if type_ == "text_link" then
                                    Decode.succeed (TextLink url bounds)

                                else
                                    Decode.fail
                                        ("Expected field 'type' to be 'text_link', but it was '"
                                            ++ type_
                                            ++ "'."
                                        )

                            Nothing ->
                                Decode.fail
                                    ("Expected field 'url' to contain a valid URL, but it was '"
                                        ++ urlString
                                        ++ "'."
                                    )
                    )

        textMention =
            Decode.map3
                (\type_ bounds user -> ( type_, bounds, user ))
                (Decode.field "type" Decode.string)
                decodeBounds
                (Decode.field "user" decodeUser)
                |> Decode.andThen
                    (\( type_, bounds, user ) ->
                        if type_ == "text_mention" then
                            Decode.succeed (TextMention user bounds)

                        else
                            Decode.fail
                                ("Expected field 'type' to be 'text_mention', but it was '"
                                    ++ type_
                                    ++ "'."
                                )
                    )
    in
    Decode.oneOf
        [ simple
        , textLink
        , textMention
        ]


encodeMessageEntity : MessageEntity -> Encode.Value
encodeMessageEntity messageEntity =
    let
        simple field bounds =
            complex field bounds []

        complex field bounds extra =
            Encode.object ([ ( "type", Encode.string field ) ] ++ extra ++ encodeBounds bounds)
    in
    case messageEntity of
        Mention bounds ->
            simple "mention" bounds

        Hashtag bounds ->
            simple "hashtag" bounds

        Cashtag bounds ->
            simple "cashtag" bounds

        BotCommand bounds ->
            simple "bot_command" bounds

        Url bounds ->
            simple "url" bounds

        Email bounds ->
            simple "email" bounds

        PhoneNumber bounds ->
            simple "phone_number" bounds

        Bold bounds ->
            simple "bold" bounds

        Italic bounds ->
            simple "italic" bounds

        Code bounds ->
            simple "code" bounds

        Pre bounds ->
            simple "pre" bounds

        TextLink url bounds ->
            complex "text_link" bounds [ ( "url", Encode.string <| Url.toString url ) ]

        TextMention user bounds ->
            complex "text_mention" bounds [ ( "user", encodeUser user ) ]


decodeTextMessage : Decode.Decoder TextMessage
decodeTextMessage =
    let
        decodeEntities =
            Decode.maybe (Decode.field "entities" (Decode.list decodeMessageEntity))
                |> Decode.map (Maybe.withDefault [])
    in
    Decode.map5
        TextMessage
        (Decode.field "message_id" decodeId)
        (Decode.field "date" Decode.int)
        (Decode.field "chat" decodeChat)
        (Decode.field "text" Decode.string)
        decodeEntities


encodeTextMessage : TextMessage -> Encode.Value
encodeTextMessage textMessage =
    let
        entities =
            if List.isEmpty textMessage.entities then
                [ ( "entities", Encode.list encodeMessageEntity textMessage.entities ) ]

            else
                []
    in
    Encode.object
        ([ ( "message_id", encodeId textMessage.message_id )
         , ( "date", Encode.int textMessage.date )
         , ( "chat", encodeChat textMessage.chat )
         , ( "text", Encode.string textMessage.text )
         ]
            ++ entities
        )


type alias InlineQuery =
    { id : StringId InlineQueryTag
    , from : User
    , query : String
    , offset : String
    }


type InlineQueryTag
    = InlineQueryTag


decodeInlineQuery : Decode.Decoder InlineQuery
decodeInlineQuery =
    Decode.map4
        InlineQuery
        (Decode.field "id" decodeStringId)
        (Decode.field "from" decodeUser)
        (Decode.field "query" Decode.string)
        (Decode.field "offset" Decode.string)


encodeInlineQuery : InlineQuery -> Encode.Value
encodeInlineQuery inlineQuery =
    Encode.object
        [ ( "id", encodeStringId inlineQuery.id )
        , ( "from", encodeUser inlineQuery.from )
        , ( "query", Encode.string inlineQuery.query )
        , ( "offset", Encode.string inlineQuery.offset )
        ]


type alias CallbackQuery =
    { id : StringId CallbackQueryTag
    , from : User
    , data : String
    }


decodeCallbackQuery : Decode.Decoder CallbackQuery
decodeCallbackQuery =
    Decode.map3
        CallbackQuery
        (Decode.field "id" decodeStringId)
        (Decode.field "from" decodeUser)
        (Decode.field "data" Decode.string)


encodeCallbackQuery : CallbackQuery -> Encode.Value
encodeCallbackQuery callbackQuery =
    Encode.object
        [ ( "id", encodeStringId callbackQuery.id )
        , ( "from", encodeUser callbackQuery.from )
        , ( "data", Encode.string callbackQuery.data )
        ]


type CallbackQueryTag
    = CallbackQueryTag


type alias Chat =
    { id : Id ChatTag
    , type_ : ChatType
    }


type ChatTag
    = ChatTag


type ChatType
    = Private
    | Group
    | Supergroup
    | Channel


encodeChatType : ChatType -> Encode.Value
encodeChatType chatType =
    let
        stringified =
            case chatType of
                Private ->
                    "private"

                Group ->
                    "group"

                Supergroup ->
                    "supergroup"

                Channel ->
                    "channel"
    in
    Encode.string stringified


decodeChat : Decode.Decoder Chat
decodeChat =
    Decode.map2
        Chat
        (Decode.field "id" decodeId)
        (Decode.field "type" Decode.string
            |> Decode.andThen
                (\typeString ->
                    case typeString of
                        "private" ->
                            Decode.succeed Private

                        "group" ->
                            Decode.succeed Group

                        "supergroup" ->
                            Decode.succeed Supergroup

                        "channel" ->
                            Decode.succeed Channel

                        _ ->
                            Decode.fail ("Chat type " ++ typeString ++ " is not known.")
                )
        )


encodeChat : Chat -> Encode.Value
encodeChat chat =
    Encode.object
        [ ( "id", encodeId chat.id )
        , ( "type", encodeChatType chat.type_ )
        ]


type alias User =
    { id : Id UserTag
    , is_bot : Bool
    , first_name : String
    , last_name : Maybe String
    , username : Maybe String
    , language_code : Maybe String
    }


type UserTag
    = UserTag


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map6
        User
        (Decode.field "id" decodeId)
        (Decode.field "is_bot" Decode.bool)
        (Decode.field "first_name" Decode.string)
        (Decode.maybe <| Decode.field "last_name" Decode.string)
        (Decode.maybe <| Decode.field "username" Decode.string)
        (Decode.maybe <| Decode.field "language_code" Decode.string)


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        ([ ( "id", encodeId user.id )
         , ( "is_bot", Encode.bool user.is_bot )
         , ( "first_name", Encode.string user.first_name )
         ]
            ++ encodeMaybe "last_name" Encode.string user.last_name
            ++ encodeMaybe "username" Encode.string user.username
            ++ encodeMaybe "language_code" Encode.string user.language_code
        )


type Id a
    = Id Int


type StringId a
    = StringId String


decodeId : Decode.Decoder (Id a)
decodeId =
    Decode.int |> Decode.map Id


encodeId : Id a -> Encode.Value
encodeId id =
    case id of
        Id rawId ->
            Encode.int rawId


decodeStringId : Decode.Decoder (StringId a)
decodeStringId =
    Decode.string |> Decode.map StringId


encodeStringId : StringId a -> Encode.Value
encodeStringId id =
    case id of
        StringId rawId ->
            Encode.string rawId



-- RESPONSE


type alias SendMessage =
    { chat_id : Id ChatTag
    , text : String
    , parse_mode : Maybe ParseMode
    , reply_to_message_id : Maybe (Id MessageTag)
    , reply_markup : Maybe MessageReplyMarkup
    }


type ParseMode
    = Markdown
    | Html


encodeParseMode : ParseMode -> Encode.Value
encodeParseMode mode =
    case mode of
        Markdown ->
            Encode.string "Markdown"

        Html ->
            Encode.string "HTML"


type MessageReplyMarkup
    = InlineKeyboardMarkup InlineKeyboard


encodeMessageReplyMarkup : MessageReplyMarkup -> Encode.Value
encodeMessageReplyMarkup markup =
    case markup of
        InlineKeyboardMarkup keyboard ->
            encodeInlineKeyboard keyboard


encodeSendMessage : SendMessage -> Encode.Value
encodeSendMessage sendMessage =
    Encode.object
        ([ ( "chat_id", encodeId sendMessage.chat_id )
         , ( "text", Encode.string sendMessage.text )
         ]
            ++ encodeMaybe "parse_mode" encodeParseMode sendMessage.parse_mode
            ++ encodeMaybe "reply_to_message_id" encodeId sendMessage.reply_to_message_id
            ++ encodeMaybe "reply_markup" encodeMessageReplyMarkup sendMessage.reply_markup
        )


type alias AnswerInlineQuery =
    { inline_query_id : StringId InlineQueryTag
    , results : List InlineQueryResult
    , cache_time : Maybe Int
    , is_personal : Maybe Bool
    , next_offset : Maybe String
    , switch_pm : Maybe SwitchPm
    }


type alias SwitchPm =
    { text : String
    , parameter : Maybe String
    }


encodeAnswerInlineQuery : AnswerInlineQuery -> Encode.Value
encodeAnswerInlineQuery inlineQuery =
    let
        switchPm =
            case inlineQuery.switch_pm of
                Just { text, parameter } ->
                    [ ( "switch_pm_text", Encode.string text ) ]
                        ++ encodeMaybe "switch_pm_parameter" Encode.string parameter

                Nothing ->
                    []
    in
    Encode.object
        ([ ( "inline_query_id", encodeStringId inlineQuery.inline_query_id )
         , ( "results", Encode.list encodeInlineQueryResult inlineQuery.results )
         ]
            ++ encodeMaybe "cache_time" Encode.int inlineQuery.cache_time
            ++ encodeMaybe "is_personal" Encode.bool inlineQuery.is_personal
            ++ encodeMaybe "next_offset" Encode.string inlineQuery.next_offset
            ++ switchPm
        )


type InlineQueryResult
    = Article InlineQueryResultArticle


encodeInlineQueryResult : InlineQueryResult -> Encode.Value
encodeInlineQueryResult inlineQueryResult =
    case inlineQueryResult of
        Article article ->
            Encode.object
                ([ ( "type", Encode.string "article" )
                 ]
                    ++ objectFromInlineQueryResultArticle article
                )


type alias InlineQueryResultArticle =
    { id : String
    , title : String
    , description : Maybe String
    , input_message_content : InputMessageContent
    , url : Maybe ArticleUrl
    , thumb_url : Maybe Url
    , reply_markup : Maybe InlineKeyboard
    }


type ArticleUrl
    = Show Url
    | Hide Url


objectFromArticleUrl : ArticleUrl -> List ( String, Encode.Value )
objectFromArticleUrl articleUrl =
    let
        ( url, hideUrl ) =
            case articleUrl of
                Show link ->
                    ( link, Encode.bool False )

                Hide link ->
                    ( link, Encode.bool True )
    in
    [ ( "url", Url.toString url |> Encode.string )
    , ( "hide_url", hideUrl )
    ]


objectFromInlineQueryResultArticle : InlineQueryResultArticle -> List ( String, Encode.Value )
objectFromInlineQueryResultArticle article =
    let
        articleUrl =
            case article.url of
                Just url ->
                    objectFromArticleUrl url

                Nothing ->
                    []
    in
    [ ( "id", Encode.string article.id )
    , ( "title", Encode.string article.title )
    , ( "input_message_content", encodeInputMessageContent article.input_message_content )
    ]
        ++ encodeMaybe "description" Encode.string article.description
        ++ encodeMaybe "thumb_url" (Url.toString >> Encode.string) article.thumb_url
        ++ encodeMaybe "reply_markup" encodeInlineKeyboard article.reply_markup
        ++ articleUrl


type InputMessageContent
    = Text InputTextMessageContent


encodeInputMessageContent : InputMessageContent -> Encode.Value
encodeInputMessageContent content =
    case content of
        Text textContent ->
            encodeInputTextMessageContent textContent


type alias InputTextMessageContent =
    { message_text : String
    , parse_mode : Maybe ParseMode
    }


encodeInputTextMessageContent : InputTextMessageContent -> Encode.Value
encodeInputTextMessageContent content =
    Encode.object
        ([ ( "message_text", Encode.string content.message_text )
         ]
            ++ encodeMaybe "parse_mode" encodeParseMode content.parse_mode
        )


type alias AnswerCallbackQuery =
    { callback_query_id : StringId CallbackQueryTag
    , text : Maybe String
    , show_alert : Bool
    , url : Maybe Url
    , cache_time : Int
    }


encodeAnswerCallbackQuery : AnswerCallbackQuery -> Encode.Value
encodeAnswerCallbackQuery query =
    Encode.object
        ([ ( "callback_query_id", encodeStringId query.callback_query_id )
         , ( "show_alert", Encode.bool query.show_alert )
         , ( "cache_time", Encode.int query.cache_time )
         ]
            ++ encodeMaybe "text" Encode.string query.text
            ++ encodeMaybe "url" (Url.toString >> Encode.string) query.url
        )


type alias InlineKeyboard =
    List InlineKeyboardRow


type alias InlineKeyboardRow =
    List InlineKeyboardButton


type InlineKeyboardButton
    = UrlButton Url InlineKeyboardButtonText
    | CallbackButton String InlineKeyboardButtonText


type alias InlineKeyboardButtonText =
    String


encodeInlineKeyboardButton : InlineKeyboardButton -> Encode.Value
encodeInlineKeyboardButton button =
    case button of
        UrlButton url text ->
            Encode.object
                [ ( "text", Encode.string text )
                , ( "url", Url.toString url |> Encode.string )
                ]

        CallbackButton data text ->
            Encode.object
                [ ( "text", Encode.string text )
                , ( "callback_data", Encode.string data )
                ]


encodeInlineKeyboard : InlineKeyboard -> Encode.Value
encodeInlineKeyboard keyboard =
    Encode.object
        [ ( "inline_keyboard", Encode.list (Encode.list encodeInlineKeyboardButton) keyboard )
        ]



-- TEST


makeTestId : Int -> Id a
makeTestId id =
    Id id


makeTestStringId : String -> StringId a
makeTestStringId id =
    StringId id



--HELPERS


encodeMaybe : String -> (a -> Encode.Value) -> Maybe a -> List ( String, Encode.Value )
encodeMaybe fieldName map maybe =
    case maybe of
        Just a ->
            [ ( fieldName, map a ) ]

        Nothing ->
            []
