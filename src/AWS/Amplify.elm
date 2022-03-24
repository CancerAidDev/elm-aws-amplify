module AWS.Amplify exposing
    ( Model, init
    , Msg
    , update
    , record
    )

{-|


## Amazon Amplify


# Model definition.

@docs Model, init


# Msg

@docs Msg


# Update

@docs update


# Actions.

@docs record

-}

import AWS.ClientInfo exposing (ClientInfo)
import AWS.CognitoIdentity as CognitoIdentity
import AWS.Config
import AWS.Credentials as Credentials
import AWS.Http
import AWS.Pinpoint as Pinpoint
import Dict exposing (Dict)
import Iso8601
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, step)
import Task
import Time



-- MODEL


{-| -}
type alias Model =
    { credentials : Maybe Credentials.Credentials
    , clientInfo : ClientInfo
    , applicationId : String
    , identityId : Maybe String
    , sessionId : String
    , region : AWS.Config.Region
    , currentSeed : Seed
    }


{-| -}
init :
    { identityPoolId : String
    , clientInfo : ClientInfo
    , applicationId : String
    , region : String
    , seed : Seed
    }
    -> ( Model, Cmd Msg )
init { identityPoolId, clientInfo, applicationId, region, seed } =
    let
        ( sessionId, seed1 ) =
            step Uuid.generator seed
    in
    ( { credentials = Nothing
      , clientInfo = clientInfo
      , applicationId = applicationId
      , identityId = Nothing
      , sessionId = Uuid.toString sessionId
      , region = region
      , currentSeed = seed1
      }
    , getId identityPoolId region
    )



-- TYPES


type alias GetIdResult =
    Result
        (AWS.Http.Error AWS.Http.AWSAppError)
        CognitoIdentity.GetIdResponse


type alias GetCredentialsResult =
    Result
        (AWS.Http.Error AWS.Http.AWSAppError)
        CognitoIdentity.GetCredentialsForIdentityResponse


type alias UpdateEventResult =
    Result
        (AWS.Http.Error AWS.Http.AWSAppError)
        Pinpoint.UpdateEndpointResponse


type alias PutEventsResult =
    Result
        (AWS.Http.Error AWS.Http.AWSAppError)
        Pinpoint.PutEventsResponse


type alias Event =
    { name : String
    , timestamp : Time.Posix
    , attributes : Dict String String
    }


type alias EndpointRequest =
    { endpointId : String
    , requestId : String
    }



-- MSG


{-| -}
type Msg
    = HandleGetId GetIdResult
    | HandleGetCredentials GetCredentialsResult
    | HandleUpdateEvent UpdateEventResult
    | HandlePutEvents PutEventsResult



-- UPDATE


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleGetId result ->
            result
                |> Result.toMaybe
                |> Maybe.andThen .identityId
                |> Maybe.map
                    (\identityId ->
                        ( { model | identityId = Just identityId }
                        , getCredentials identityId model.region
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        HandleGetCredentials result ->
            case result of
                Ok { credentials } ->
                    credentials
                        |> Maybe.andThen
                            (\{ accessKeyId, secretKey, sessionToken } ->
                                Maybe.map2
                                    (\accessKeyId_ secretAccessKey_ ->
                                        let
                                            credentials_ =
                                                { accessKeyId = accessKeyId_
                                                , secretAccessKey = secretAccessKey_
                                                , sessionToken = sessionToken
                                                }
                                        in
                                        updateEndpoint credentials_
                                            { model | credentials = Just credentials_ }
                                    )
                                    accessKeyId
                                    secretKey
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| -}
getId : String -> AWS.Config.Region -> Cmd Msg
getId identityPoolId region =
    AWS.Http.sendUnsigned (CognitoIdentity.service region)
        (CognitoIdentity.getId
            { accountId = Nothing
            , identityPoolId = identityPoolId
            , logins = Nothing
            }
        )
        |> Task.attempt HandleGetId


getCredentials : String -> AWS.Config.Region -> Cmd Msg
getCredentials identityId region =
    AWS.Http.sendUnsigned (CognitoIdentity.service region)
        (CognitoIdentity.getCredentialsForIdentity
            { customRoleArn = Nothing
            , identityId = identityId
            , logins = Nothing
            }
        )
        |> Task.attempt HandleGetCredentials


{-| -}
updateEndpoint : Credentials.Credentials -> Model -> ( Model, Cmd Msg )
updateEndpoint credentials ({ clientInfo, applicationId, identityId, region } as model) =
    let
        ( requestId, seed1 ) =
            step Uuid.generator model.currentSeed

        ( endpointId, seed2 ) =
            step Uuid.generator seed1
    in
    ( { model | currentSeed = seed2 }
    , AWS.Http.send (Pinpoint.service region)
        credentials
        (Pinpoint.updateEndpoint
            { applicationId = applicationId
            , endpointId = Uuid.toString endpointId
            , endpointRequest =
                { address = Nothing
                , attributes = Just Dict.empty
                , channelType = Nothing
                , demographic =
                    Just
                        { appVersion = Just clientInfo.appVersion
                        , locale = Nothing
                        , make = Just clientInfo.make
                        , model = Just clientInfo.model
                        , modelVersion = Just clientInfo.version
                        , platform = Just clientInfo.platform
                        , platformVersion = Nothing
                        , timezone = Nothing
                        }
                , effectiveDate = Nothing
                , endpointStatus = Nothing
                , location = Nothing
                , metrics = Just Dict.empty
                , optOut = Nothing
                , requestId = Just <| Uuid.toString requestId
                , user =
                    Just
                        { userAttributes = Just Dict.empty
                        , userId = identityId
                        }
                }
            }
        )
        |> Task.attempt HandleUpdateEvent
    )


{-| -}
record : String -> Credentials.Credentials -> Model -> Event -> ( Model, Cmd Msg )
record identityId credentials ({ applicationId, sessionId, region } as model) { name, timestamp, attributes } =
    let
        ( eventId, seed1 ) =
            step Uuid.generator model.currentSeed
    in
    ( { model | currentSeed = seed1 }
    , AWS.Http.send (Pinpoint.service region)
        credentials
        (Pinpoint.putEvents
            { applicationId = applicationId
            , eventsRequest =
                { batchItem =
                    Just <|
                        Dict.fromList
                            [ ( identityId
                              , { endpoint =
                                    Just
                                        { address = Nothing
                                        , attributes = Nothing
                                        , channelType = Nothing
                                        , demographic = Nothing
                                        , effectiveDate = Nothing
                                        , endpointStatus = Nothing
                                        , location = Nothing
                                        , metrics = Nothing
                                        , optOut = Nothing
                                        , requestId = Nothing
                                        , user = Nothing
                                        }
                                , events =
                                    Just <|
                                        Dict.fromList
                                            [ ( Uuid.toString eventId
                                              , { appPackageName = Nothing
                                                , appTitle = Nothing
                                                , appVersionCode = Nothing
                                                , attributes = Just attributes
                                                , clientSdkVersion = Nothing
                                                , eventType = Just name
                                                , metrics = Nothing
                                                , sdkName = Nothing
                                                , session =
                                                    Just
                                                        { duration = Nothing
                                                        , id = Just sessionId
                                                        , startTimestamp = Just <| Iso8601.fromTime timestamp
                                                        , stopTimestamp = Nothing
                                                        }
                                                , timestamp = Just <| Iso8601.fromTime timestamp
                                                }
                                              )
                                            ]
                                }
                              )
                            ]
                }
            }
        )
        |> Task.attempt HandlePutEvents
    )
