module AWS.Amplify exposing
    ( Config, Model, init
    , update, Msg
    , record, Event
    )

{-| Amplify component that handles refresh of expired cognito credentials.


# Setup

@docs Config, Model, init


# Update

@docs update, Msg


# Record Events

@docs record, Event

-}

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


{-| Opaque Msg datatype
-}
type Msg
    = AuthConfigured Auth.Identity
    | AuthConfigureFailed (AWS.Http.Error AWS.Http.AWSAppError)
    | AnalyticsConfigured Auth.Identity
    | AnalyticsConfigureFailed (AWS.Http.Error AWS.Http.AWSAppError)
    | AuthFetchedNewCredentials Auth.Identity
    | AuthFetchNewCredientalsFailed (AWS.Http.Error AWS.Http.AWSAppError)
    | Record Event
    | RecordWithTime Event Time.Posix
    | RecordWithAuthAndTime Auth.Identity Analytics.Event Time.Posix
    | Recorded
    | RecordFailed (AWS.Http.Error AWS.Http.AWSAppError)


{-| Static configuration settings
-}
type alias Config =
    { pinpointProjectId : String
    , awsRegion : String
    , clientInfo : ClientInfo
    , cmds :
        { authConfigureFailed : Maybe (AWS.Http.Error AWS.Http.AWSAppError -> Cmd Msg)
        , analyticsConfigureFailed : Maybe (AWS.Http.Error AWS.Http.AWSAppError -> Cmd Msg)
        , recordFailed : Maybe (AWS.Http.Error AWS.Http.AWSAppError -> Cmd Msg)
        , fetchNewCredentialsFailed : Maybe (AWS.Http.Error AWS.Http.AWSAppError -> Cmd Msg)
        }
    }



-- MODEL


{-| Model datatype
-}
type alias Model =
    { seed : Seed
    , sessionId : String
    , sessionStartTime : Time.Posix
    , authIdentity : RemoteData (AWS.Http.Error AWS.Http.AWSAppError) Auth.Identity
    , analytics : RemoteData (AWS.Http.Error AWS.Http.AWSAppError) ()
    , queue : Dict String Analytics.Event
    }


{-| Initialise Model and fetch identity and credentials
-}
init : { awsRegion : String, identityPoolId : String, time : Time.Posix, seed : Seed } -> ( Model, Cmd Msg )
init { awsRegion, identityPoolId, time, seed } =
    let
        ( sessionId, seed1 ) =
            Seed.step Uuid.generator seed
    in
    ( { seed = seed1
      , sessionId = Uuid.toString sessionId
      , sessionStartTime = time
      , authIdentity = RemoteData.Loading
      , analytics = RemoteData.NotAsked
      , queue = Dict.empty
      }
    , Auth.configure { region = awsRegion, identityPoolId = identityPoolId }
        |> Task.attempt (Result.map AuthConfigured >> ResultExtra.extract AuthConfigureFailed)
    )



-- TYPES


{-| Event datatype
-}
type alias Event =
    { name : String
    , attributes : Dict String String
    }



-- UPDATE


{-| Update
-}
update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        AuthConfigured authIdentity ->
            configureAnalytics config model authIdentity

        AuthConfigureFailed err ->
            ( { model | authIdentity = RemoteData.Failure err }
            , Maybe.map (\f -> f err) config.cmds.authConfigureFailed |> Maybe.withDefault Cmd.none
            )

        AnalyticsConfigured authIdentity ->
            processQueue { model | analytics = RemoteData.Success () } authIdentity

        AnalyticsConfigureFailed err ->
            ( { model | analytics = RemoteData.Failure err }
            , Maybe.map (\f -> f err) config.cmds.analyticsConfigureFailed |> Maybe.withDefault Cmd.none
            )

        AuthFetchedNewCredentials authIdentity ->
            processQueue model authIdentity

        AuthFetchNewCredientalsFailed err ->
            ( model
            , Maybe.map (\f -> f err) config.cmds.fetchNewCredentialsFailed |> Maybe.withDefault Cmd.none
            )

        Record event ->
            ( model, record event )

        RecordWithTime event time ->
            recordWithTime config model event time

        RecordWithAuthAndTime authIdentity event time ->
            recordWithAuthAndTime config model authIdentity event time

        Recorded ->
            ( model, Cmd.none )

        RecordFailed err ->
            ( model
            , Maybe.map (\f -> f err) config.cmds.recordFailed |> Maybe.withDefault Cmd.none
            )


configureAnalytics : Config -> Model -> Auth.Identity -> ( Model, Cmd Msg )
configureAnalytics config model authIdentity =
    let
        ( endpointId, seed1 ) =
            Seed.step Uuid.generator model.seed

        ( requestId, seed2 ) =
            Seed.step Uuid.generator seed1
    in
    ( { model
        | seed = seed2
        , authIdentity = RemoteData.Success authIdentity
        , analytics = RemoteData.Loading
      }
    , Task.attempt
        (Result.map (always (AnalyticsConfigured authIdentity))
            >> ResultExtra.extract AnalyticsConfigureFailed
        )
        (Analytics.configure
            { credentials = authIdentity.credentials
            , clientInfo = config.clientInfo
            , applicationId = config.pinpointProjectId
            , sessionId = model.sessionId
            , sessionStartTime = model.sessionStartTime
            , identityId = authIdentity.identityId
            , region = config.awsRegion
            }
            { endpointId = Uuid.toString endpointId
            , requestId = Uuid.toString requestId
            }
        )
    )


processQueue : Model -> Auth.Identity -> ( Model, Cmd Msg )
processQueue model authIdentity =
    Dict.foldl
        (\_ event cmds ->
            recordWithAuth authIdentity event
                |> (\cmd -> cmd :: cmds)
        )
        []
        model.queue
        |> (\cmds ->
                ( { model | authIdentity = RemoteData.Success authIdentity }
                , Cmd.batch cmds
                )
           )


{-| Record event.

Events are stored in a queue if the identity, credentials, or analytics are loading.

-}
record : Event -> Cmd Msg
record event =
    Task.perform (RecordWithTime event) Time.now


recordWithTime : Config -> Model -> Event -> Time.Posix -> ( Model, Cmd Msg )
recordWithTime config model event time =
    let
        ( eventId, seed1 ) =
            Seed.step Uuid.generator model.seed

        analyticsEvent =
            { eventId = Uuid.toString eventId
            , eventTime = time
            , name = event.name
            , attributes = event.attributes
            }

        updatedModel =
            { model | seed = seed1 }
    in
    case model.authIdentity of
        RemoteData.Success authIdentity ->
            if RemoteData.isSuccess model.analytics then
                recordWithAuthAndTime config updatedModel authIdentity analyticsEvent time

            else
                ( recordEnqueue updatedModel analyticsEvent, Cmd.none )

        RemoteData.Loading ->
            ( recordEnqueue updatedModel analyticsEvent, Cmd.none )

        RemoteData.Failure _ ->
            ( model, Cmd.none )

        RemoteData.NotAsked ->
            ( model, Cmd.none )


recordEnqueue : Model -> Analytics.Event -> Model
recordEnqueue model event =
    { model | queue = Dict.insert event.eventId event model.queue }


recordWithAuth : Auth.Identity -> Analytics.Event -> Cmd Msg
recordWithAuth authIdentity event =
    Task.perform (RecordWithAuthAndTime authIdentity event) Time.now


recordWithAuthAndTime : Config -> Model -> Auth.Identity -> Analytics.Event -> Time.Posix -> ( Model, Cmd Msg )
recordWithAuthAndTime config model authIdentity event time =
    if isValid authIdentity time then
        ( { model | queue = Dict.remove event.eventId model.queue }
        , Analytics.record
            { credentials = authIdentity.credentials
            , clientInfo = config.clientInfo
            , applicationId = config.pinpointProjectId
            , sessionId = model.sessionId
            , sessionStartTime = model.sessionStartTime
            , identityId = authIdentity.identityId
            , region = config.awsRegion
            }
            event
            |> Task.attempt (Result.map (always Recorded) >> ResultExtra.extract RecordFailed)
        )

    else
        ( recordEnqueue { model | authIdentity = RemoteData.Loading } event
        , Auth.getCredentials config.awsRegion authIdentity.identityId
            |> Task.attempt
                (Result.map AuthFetchedNewCredentials
                    >> ResultExtra.extract AuthFetchNewCredientalsFailed
                )
        )


isValid : Auth.Identity -> Time.Posix -> Bool
isValid { credentials } currentTime =
    -- Treat credentials as expired if expiration is in less than 60 seconds
    Time.posixToMillis currentTime < Time.posixToMillis credentials.expiration - (60 * 1000)
