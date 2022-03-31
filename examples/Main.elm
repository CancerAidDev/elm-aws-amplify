module Main exposing (main)

import AWS.Amplify.Analytics as Analytics
import AWS.Amplify.Auth as Auth
import AWS.Amplify.ClientInfo exposing (ClientInfo)
import AWS.Http
import AWS.Pinpoint as Pinpoint
import Browser
import Dict
import Html exposing (Html, button, div, h1, h2, h3, input, label, text)
import Html.Attributes exposing (disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import RemoteData exposing (RemoteData)
import Task


type alias Flags =
    { seed : ( Int, List Int )
    , pinpointProjectId : String
    , identityPoolId : String
    , clientInfo : ClientInfo
    , region : String
    }


type alias Model =
    { identityPoolId : String
    , clientInfo : ClientInfo
    , applicationId : String
    , sessionId : String
    , region : String
    , seed : Seed
    , name : String
    , key : String
    , value : String
    , identity : RemoteData String Auth.Identity
    , analyticsConfigured : RemoteData (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.UpdateEndpointResponse
    , analyticsRecorded : RemoteData (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.PutEventsResponse
    }


init : Flags -> ( Model, Cmd Msg )
init { seed, identityPoolId, clientInfo, pinpointProjectId, region } =
    let
        ( baseSeed, seedExtension ) =
            seed

        ( sessionId, currentSeed ) =
            initialSeed baseSeed seedExtension
                |> step Uuid.generator
    in
    ( { identityPoolId = identityPoolId
      , clientInfo = clientInfo
      , applicationId = pinpointProjectId
      , sessionId = Uuid.toString sessionId
      , region = region
      , seed = currentSeed
      , name = "Test"
      , key = "Hello"
      , value = "World"
      , identity = RemoteData.Loading
      , analyticsConfigured = RemoteData.NotAsked
      , analyticsRecorded = RemoteData.NotAsked
      }
    , Auth.configure { region = region, identityPoolId = identityPoolId }
        |> Task.attempt AuthConfigured
    )


type Msg
    = AuthConfigured (Result (AWS.Http.Error AWS.Http.AWSAppError) Auth.Identity)
    | AnalyticsConfigured (Result (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.UpdateEndpointResponse)
    | Record Auth.Identity
    | Recorded (Result (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.PutEventsResponse)
    | UpdateName String
    | UpdateKey String
    | UpdateValue String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthConfigured result ->
            result
                |> Result.map
                    (\identity ->
                        let
                            ( endpointId, seed1 ) =
                                step Uuid.generator model.seed

                            ( requestId, seed2 ) =
                                step Uuid.generator seed1
                        in
                        ( { model | seed = seed2, identity = RemoteData.Success identity }
                        , Task.attempt AnalyticsConfigured
                            (Analytics.configure
                                { credentials = identity.credentials
                                , clientInfo = model.clientInfo
                                , applicationId = model.applicationId
                                , sessionId = model.sessionId
                                , identityId = identity.identityId
                                , region = model.region
                                }
                                { endpointId = Uuid.toString endpointId
                                , requestId = Uuid.toString requestId
                                }
                            )
                        )
                    )
                |> Result.withDefault
                    ( { model | identity = RemoteData.Failure "Failed to fetch identity" }
                    , Cmd.none
                    )

        AnalyticsConfigured result ->
            ( { model | analyticsConfigured = RemoteData.fromResult result }, Cmd.none )

        Record identity ->
            let
                ( eventId, seed1 ) =
                    step Uuid.generator model.seed
            in
            ( { model | seed = seed1 }
            , Task.attempt Recorded
                (Analytics.record
                    { credentials = identity.credentials
                    , clientInfo = model.clientInfo
                    , applicationId = model.applicationId
                    , sessionId = model.sessionId
                    , identityId = identity.identityId
                    , region = model.region
                    }
                    { eventId = Uuid.toString eventId
                    , name = model.name
                    , attributes = Dict.fromList [ ( model.key, model.value ) ]
                    }
                )
            )

        Recorded result ->
            ( { model | analyticsRecorded = RemoteData.fromResult result }, Cmd.none )

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
    case model.identity of
        RemoteData.Success identity ->
            div []
                [ h1 [] [ text "elm-aws-amplify example" ]
                , h2 [] [ text "IdentityId" ]
                , div [] [ text identity.identityId ]
                , case model.analyticsConfigured of
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
                              case model.analyticsRecorded of
                                RemoteData.Success val ->
                                    viewResponse (Debug.toString val)

                                RemoteData.Loading ->
                                    viewResponse "Loading..."

                                RemoteData.NotAsked ->
                                    text ""

                                RemoteData.Failure err ->
                                    viewResponse (Debug.toString err)
                            , div []
                                [ button
                                    [ onClick <| Record identity
                                    , disabled (not (RemoteData.isSuccess model.analyticsConfigured))
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
            text err


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
