module AWS.Amplify exposing (..)

import AWS.Amplify.Analytics as Analytics
import AWS.Amplify.Auth as Auth
import AWS.Amplify.ClientInfo exposing (ClientInfo)
import AWS.Http
import Dict exposing (Dict)
import Prng.Uuid as Uuid
import Random.Pcg.Extended as Seed exposing (Seed)
import RemoteData exposing (RemoteData)
import Result.Extra as ResultExtra
import Task
import Time



-- MSG


type Msg
    = AuthConfigured Auth.Identity
    | AuthConfigureFailed (AWS.Http.Error AWS.Http.AWSAppError)
    | AnalyticsConfigured Auth.Identity
    | AnalyticsConfigureFailed (AWS.Http.Error AWS.Http.AWSAppError)
    | AuthFetchedNewCredentials Auth.Identity
    | AuthFetchNewCredientalsFailed (AWS.Http.Error AWS.Http.AWSAppError)
    | RecordWithTime Event Time.Posix
    | RecordWithAuthAndTime Auth.Identity Analytics.Event Time.Posix
    | Recorded
    | RecordFailed (AWS.Http.Error AWS.Http.AWSAppError)


type Config
    = Config
        { identityPoolId : String
        , pinpointProjectId : String
        , sessionId : String
        , startTime : Time.Posix
        , awsRegion : String
        , clientInfo : ClientInfo
        , authConfigureFailed : Maybe (AWS.Http.Error AWS.Http.AWSAppError -> Cmd Msg)
        , analyticsConfigureFailed : Maybe (AWS.Http.Error AWS.Http.AWSAppError -> Cmd Msg)
        , recordFailed : Maybe (AWS.Http.Error AWS.Http.AWSAppError -> Cmd Msg)
        }



-- MODEL


type Model
    = Model
        { seed : Seed
        , authIdentity : RemoteData String Auth.Identity
        , analyticsConfigured : Bool
        , queue : Dict String Analytics.Event
        }


init : Config -> Seed -> ( Model, Cmd Msg )
init (Config { awsRegion, identityPoolId }) seed =
    ( Model
        { seed = seed
        , authIdentity = RemoteData.Loading
        , analyticsConfigured = False
        , queue = Dict.empty
        }
    , Auth.configure { region = awsRegion, identityPoolId = identityPoolId }
        |> Task.attempt (Result.map AuthConfigured >> ResultExtra.extract AuthConfigureFailed)
    )



-- TYPES


type alias Event =
    { name : String
    , attributes : Dict String String
    }



-- UPDATE


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update (Config config) msg (Model model) =
    case msg of
        AuthConfigured authIdentity ->
            configureAnalytics (Config config) (Model model) authIdentity

        AuthConfigureFailed err ->
            ( Model { model | authIdentity = RemoteData.Failure "Failed to fetch identity" }
            , Maybe.map (\f -> f err) config.authConfigureFailed |> Maybe.withDefault Cmd.none
            )

        AnalyticsConfigured authIdentity ->
            processQueue (Model model) authIdentity

        AnalyticsConfigureFailed err ->
            ( Model model
            , Maybe.map (\f -> f err) config.analyticsConfigureFailed |> Maybe.withDefault Cmd.none
            )

        AuthFetchedNewCredentials authIdentity ->
            processQueue (Model model) authIdentity

        AuthFetchNewCredientalsFailed err ->
            ( Model model
            , Maybe.map (\f -> f err) config.analyticsConfigureFailed |> Maybe.withDefault Cmd.none
            )

        RecordWithTime event time ->
            recordWithTime (Config config) (Model model) event time

        RecordWithAuthAndTime authIdentity event time ->
            recordWithAuthAndTime (Config config) (Model model) authIdentity event time

        Recorded ->
            ( Model model, Cmd.none )

        RecordFailed err ->
            ( Model model
            , Maybe.map (\f -> f err) config.recordFailed |> Maybe.withDefault Cmd.none
            )


configureAnalytics : Config -> Model -> Auth.Identity -> ( Model, Cmd Msg )
configureAnalytics (Config config) (Model model) authIdentity =
    let
        ( endpointId, seed1 ) =
            Seed.step Uuid.generator model.seed

        ( requestId, seed2 ) =
            Seed.step Uuid.generator seed1
    in
    ( Model { model | seed = seed2, authIdentity = RemoteData.Success authIdentity }
    , Task.attempt
        (Result.map (always (AnalyticsConfigured authIdentity))
            >> ResultExtra.extract AnalyticsConfigureFailed
        )
        (Analytics.configure
            { credentials = authIdentity.credentials
            , clientInfo = config.clientInfo
            , applicationId = config.pinpointProjectId
            , sessionId = config.sessionId
            , sessionStartTime = config.startTime
            , identityId = authIdentity.identityId
            , region = config.awsRegion
            }
            { endpointId = Uuid.toString endpointId
            , requestId = Uuid.toString requestId
            }
        )
    )


processQueue : Model -> Auth.Identity -> ( Model, Cmd Msg )
processQueue (Model model) authIdentity =
    Dict.foldl
        (\_ event cmds ->
            recordWithAuth authIdentity event
                |> (\cmd -> cmd :: cmds)
        )
        []
        model.queue
        |> (\cmds ->
                ( Model { model | authIdentity = RemoteData.Success authIdentity }
                , Cmd.batch cmds
                )
           )


record : Event -> Cmd Msg
record event =
    Task.perform (RecordWithTime event) Time.now


recordWithTime : Config -> Model -> Event -> Time.Posix -> ( Model, Cmd Msg )
recordWithTime config (Model model) event time =
    let
        analyticsEvent =
            { eventId = ""
            , eventTime = time
            , name = event.name
            , attributes = event.attributes
            }
    in
    case model.authIdentity of
        RemoteData.Success authIdentity ->
            if model.analyticsConfigured then
                recordWithAuthAndTime config (Model model) authIdentity analyticsEvent time

            else
                ( recordEnqueue (Model model) analyticsEvent, Cmd.none )

        RemoteData.Loading ->
            ( recordEnqueue (Model model) analyticsEvent, Cmd.none )

        RemoteData.Failure _ ->
            ( Model model, Cmd.none )

        RemoteData.NotAsked ->
            ( Model model, Cmd.none )


recordEnqueue : Model -> Analytics.Event -> Model
recordEnqueue (Model model) event =
    Model { model | queue = Dict.insert event.eventId event model.queue }


recordWithAuth : Auth.Identity -> Analytics.Event -> Cmd Msg
recordWithAuth authIdentity event =
    Task.perform (RecordWithAuthAndTime authIdentity event) Time.now


recordWithAuthAndTime : Config -> Model -> Auth.Identity -> Analytics.Event -> Time.Posix -> ( Model, Cmd Msg )
recordWithAuthAndTime (Config config) (Model model) authIdentity event time =
    if isValid authIdentity time then
        ( Model model
        , Analytics.record
            { credentials = authIdentity.credentials
            , clientInfo = config.clientInfo
            , applicationId = config.pinpointProjectId
            , sessionId = config.sessionId
            , sessionStartTime = config.startTime
            , identityId = authIdentity.identityId
            , region = config.awsRegion
            }
            event
            |> Task.attempt (Result.map (always Recorded) >> ResultExtra.extract RecordFailed)
        )

    else
        ( recordEnqueue (Model { model | authIdentity = RemoteData.Loading }) event
        , Auth.getCredentials config.awsRegion authIdentity.identityId
            |> Task.attempt
                (Result.map AuthFetchedNewCredentials
                    >> ResultExtra.extract AuthFetchNewCredientalsFailed
                )
        )


isValid : Auth.Identity -> Time.Posix -> Bool
isValid { credentials } currentTime =
    credentials.expiration
        |> String.toInt
        |> Maybe.map (\seconds -> 1000 * seconds)
        |> Maybe.map (\expirationTime -> Time.posixToMillis currentTime < expirationTime)
        |> Maybe.withDefault True
