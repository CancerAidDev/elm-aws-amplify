module Main exposing (main)

import AWS.Amplify as Amplify
import AWS.Amplify.ClientInfo exposing (ClientInfo)
import Browser
import Dict
import Html exposing (Html, button, div, h1, h2, h3, input, label, text)
import Html.Attributes exposing (disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Iso8601
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (initialSeed, step)
import RemoteData
import Time
import Time.Extra as TimeExtra


type alias Flags =
    { seed : ( Int, List Int )
    , date : String
    , pinpointProjectId : String
    , identityPoolId : String
    , clientInfo : ClientInfo
    , region : String
    }


type alias Model =
    { identityPoolId : String
    , pinpointProjectId : String
    , clientInfo : ClientInfo
    , applicationId : String
    , sessionId : String
    , sessionStartTime : Time.Posix
    , region : String
    , name : String
    , key : String
    , value : String
    , amplify : Amplify.Model
    }


initTime : String -> Time.Posix
initTime =
    let
        default =
            TimeExtra.partsToPosix Time.utc <|
                TimeExtra.Parts 2022 Time.Jan 1 0 0 0 0
    in
    Iso8601.toTime >> Result.withDefault default


init : Flags -> ( Model, Cmd Msg )
init { seed, date, identityPoolId, clientInfo, pinpointProjectId, region } =
    let
        ( baseSeed, seedExtension ) =
            seed

        ( sessionId, currentSeed ) =
            initialSeed baseSeed seedExtension
                |> step Uuid.generator

        ( amplify, cmd ) =
            Amplify.init
                { awsRegion = region
                , identityPoolId = identityPoolId
                , seed = currentSeed
                }
    in
    ( { identityPoolId = identityPoolId
      , pinpointProjectId = pinpointProjectId
      , clientInfo = clientInfo
      , applicationId = pinpointProjectId
      , sessionId = Uuid.toString sessionId
      , sessionStartTime = initTime date
      , region = region
      , name = "Test"
      , key = "Hello"
      , value = "World"
      , amplify = amplify
      }
    , Cmd.map AmplifyMsg cmd
    )


type Msg
    = AmplifyMsg Amplify.Msg
    | UpdateName String
    | UpdateKey String
    | UpdateValue String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AmplifyMsg subMsg ->
            Amplify.update
                { pinpointProjectId = model.pinpointProjectId
                , sessionId = model.sessionId
                , sessionStartTime = model.sessionStartTime
                , awsRegion = model.region
                , clientInfo = model.clientInfo
                , authConfigureFailedCmd = Nothing
                , analyticsConfigureFailedCmd = Nothing
                , recordFailedCmd = Nothing
                }
                subMsg
                model.amplify
                |> (\( updatedAmplify, cmd ) -> ( { model | amplify = updatedAmplify }, Cmd.map AmplifyMsg cmd ))

        UpdateName val ->
            ( { model | name = val }, Cmd.none )

        UpdateKey val ->
            ( { model | key = val }, Cmd.none )

        UpdateValue val ->
            ( { model | value = val }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.amplify.authIdentity of
        RemoteData.Success authIdentity ->
            div []
                [ h1 [] [ text "elm-aws-amplify example" ]
                , h2 [] [ text "IdentityId" ]
                , div [] [ text authIdentity.identityId ]
                , case model.amplify.analytics of
                    RemoteData.Success _ ->
                        div []
                            [ h2 [] [ text "Record" ]
                            , h3 [] [ text "Name" ]
                            , input [ value model.name, onInput UpdateName ] []
                            , h3 [] [ text "Attributes" ]
                            , div []
                                [ div []
                                    [ label [] [ text "Key: " ]
                                    , input [ value model.key, onInput UpdateKey ] []
                                    ]
                                , div []
                                    [ label [] [ text "Value: " ]
                                    , input [ value model.value, onInput UpdateValue ] []
                                    ]
                                ]
                            , let
                                viewResponse str =
                                    div [] [ h3 [] [ text "Response" ], text str ]
                              in
                              case model.amplify.analytics of
                                RemoteData.Success () ->
                                    text ""

                                RemoteData.Loading ->
                                    viewResponse "Loading..."

                                RemoteData.NotAsked ->
                                    text ""

                                RemoteData.Failure err ->
                                    viewResponse (Debug.toString err)
                            , div []
                                [ Html.map AmplifyMsg <|
                                    button
                                        [ onClick <|
                                            Amplify.Record
                                                { name = model.name
                                                , attributes = Dict.fromList [ ( model.key, model.value ) ]
                                                }
                                        , disabled (not (RemoteData.isSuccess model.amplify.analytics))
                                        , style "margin-top" "1.5em"
                                        ]
                                        [ text "Submit" ]
                                ]
                            ]

                    RemoteData.Loading ->
                        text "Loading..."

                    RemoteData.NotAsked ->
                        text ""

                    RemoteData.Failure err ->
                        text (Debug.toString err)
                ]

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.NotAsked ->
            text ""

        RemoteData.Failure err ->
            text (Debug.toString err)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
