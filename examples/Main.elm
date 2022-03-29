module Main exposing (main)

import AWS.Amplify.Analytics as Analytics
import AWS.Amplify.Auth as Auth
import AWS.Amplify.ClientInfo exposing (ClientInfo)
import AWS.Http
import AWS.Pinpoint as Pinpoint
import Browser
import Dict
import Html exposing (Html, button, div, h1, h2, input, label, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Task
import Time


type alias Flags =
    { seed : ( Int, List Int )
    , appId : String
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
    , identity : Maybe Auth.Identity
    , analyticsConfigured : Bool
    }


type alias ConfigureAuthResult =
    Result (AWS.Http.Error AWS.Http.AWSAppError) Auth.Identity


type alias ConfigureAnalyticsResult =
    Result (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.UpdateEndpointResponse


init : Flags -> ( Model, Cmd Msg )
init { seed, identityPoolId, clientInfo, appId, region } =
    let
        ( baseSeed, seedExtension ) =
            seed

        ( sessionId, currentSeed ) =
            initialSeed baseSeed seedExtension
                |> step Uuid.generator
    in
    ( { identityPoolId = identityPoolId
      , clientInfo = clientInfo
      , applicationId = appId
      , sessionId = Uuid.toString sessionId
      , region = region
      , seed = currentSeed
      , name = "Test"
      , key = "Hello"
      , value = "World"
      , identity = Nothing
      , analyticsConfigured = False
      }
    , Auth.configure { region = region, identityPoolId = identityPoolId }
        |> Task.attempt AuthConfigured
    )


type Msg
    = AuthConfigured ConfigureAuthResult
    | AnalyticsConfigured ConfigureAnalyticsResult
    | Record Auth.Identity
    | RecordWithTime Auth.Identity Time.Posix
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
                        ( { model | seed = seed2, identity = Just identity }
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
                |> Result.withDefault ( model, Cmd.none )

        AnalyticsConfigured result ->
            let
                _ =
                    Debug.log "" result
            in
            case result of
                Ok _ ->
                    ( { model | analyticsConfigured = True }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Record identity ->
            ( model
            , Time.now |> Task.perform (RecordWithTime identity)
            )

        RecordWithTime identity time ->
            let
                ( eventId, seed1 ) =
                    step Uuid.generator model.seed
            in
            ( { model | seed = seed1, identity = Just identity }
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
                    , timestamp = time
                    , attributes = Dict.fromList [ ( model.key, model.value ) ]
                    }
                )
            )

        Recorded result ->
            let
                _ =
                    Debug.log "" result
            in
            ( model, Cmd.none )

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
        Just identity ->
            div []
                [ h2 [] [ text "IdentityId" ]
                , div [] [ text identity.identityId ]
                , h1 [] [ text "Record" ]
                , h2 [] [ text "Name" ]
                , input [ value model.name, onInput UpdateName ] []
                , h2 [] [ text "Attributes" ]
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
                , div []
                    [ button
                        [ onClick <| Record identity
                        , disabled (not model.analyticsConfigured)
                        ]
                        [ text "Submit" ]
                    ]
                ]

        Nothing ->
            text ""


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
