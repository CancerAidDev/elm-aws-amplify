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

import AWS.CognitoIdentity as CognitoIdentity
import AWS.Credentials as Credentials
import AWS.Http
import AWS.Pinpoint as Pinpoint
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Task



-- MODEL


{-| -}
type alias Model =
    { credentials : Maybe Credentials.Credentials
    , clientInfo : ClientInfo
    , applicationId : String
    , endpointId : String
    , userId : String
    , sessionId : String
    , region : String
    }


{-| -}
init :
    { identityPoolId : String
    , clientInfo : ClientInfo
    , applicationId : String
    , endpointId : String
    , userId : String
    , sessionId : String
    , region : String
    }
    -> ( Model, Cmd Msg )
init { identityPoolId, clientInfo, applicationId, endpointId, userId, sessionId, region } =
    ( { credentials = Nothing
      , clientInfo = clientInfo
      , applicationId = applicationId
      , endpointId = endpointId
      , userId = userId
      , sessionId = sessionId
      , region = region
      }
    , getId identityPoolId
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


type alias ClientInfo =
    { platform : String
    , make : String
    , model : String
    , version : String
    , appVersion : String
    , language : String
    , timezone : String
    }


type alias Event =
    { eventId : String
    , eventType : String
    , timestamp : String
    , attributes : Dict String String
    }


type alias EndpointRequest =
    { requestId : String }


decoder : Decode.Decoder ClientInfo
decoder =
    Decode.succeed ClientInfo
        |> DecodePipeline.required "platform" Decode.string
        |> DecodePipeline.required "make" Decode.string
        |> DecodePipeline.required "model" Decode.string
        |> DecodePipeline.required "version" Decode.string
        |> DecodePipeline.required "appVersion" Decode.string
        |> DecodePipeline.required "language" Decode.string
        |> DecodePipeline.required "timezone" Decode.string



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
            ( model
            , result
                |> Result.toMaybe
                |> Maybe.andThen .identityId
                |> Maybe.map getCredentials
                |> Maybe.withDefault Cmd.none
            )

        HandleGetCredentials result ->
            ( result
                |> Result.toMaybe
                |> Maybe.andThen
                    (\{ credentials } ->
                        Maybe.andThen
                            (\{ accessKeyId, secretKey, sessionToken } ->
                                Maybe.map2
                                    (\accessKeyId_ secretAccessKey_ ->
                                        { model
                                            | credentials =
                                                Just
                                                    { accessKeyId = accessKeyId_
                                                    , secretAccessKey = secretAccessKey_
                                                    , sessionToken = sessionToken
                                                    }
                                        }
                                    )
                                    accessKeyId
                                    secretKey
                            )
                            credentials
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


{-| -}
getId : String -> Cmd Msg
getId identityPoolId =
    AWS.Http.sendUnsigned (CognitoIdentity.service "ap-southeast-2")
        (CognitoIdentity.getId
            { accountId = Nothing
            , identityPoolId = identityPoolId
            , logins = Nothing
            }
        )
        |> Task.attempt HandleGetId


getCredentials : String -> Cmd Msg
getCredentials identityId =
    AWS.Http.sendUnsigned (CognitoIdentity.service "ap-southeast-2")
        (CognitoIdentity.getCredentialsForIdentity
            { customRoleArn = Nothing
            , identityId = identityId
            , logins = Nothing
            }
        )
        |> Task.attempt HandleGetCredentials


{-| -}
updateEndpoint : Model -> EndpointRequest -> Maybe (Cmd Msg)
updateEndpoint { credentials, clientInfo, applicationId, endpointId, userId, region } { requestId } =
    credentials
        |> Maybe.map
            (\creds ->
                AWS.Http.send (CognitoIdentity.service region)
                    creds
                    (Pinpoint.updateEndpoint
                        { applicationId = applicationId
                        , endpointId = endpointId
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
                            , location =
                                Just
                                    { city = Nothing
                                    , country = Nothing
                                    , latitude = Nothing
                                    , longitude = Nothing
                                    , postalCode = Nothing
                                    , region = Nothing
                                    }
                            , metrics = Just Dict.empty
                            , optOut = Nothing
                            , requestId = Just requestId
                            , user =
                                Just
                                    { userAttributes = Just Dict.empty
                                    , userId = Just userId
                                    }
                            }
                        }
                    )
                    |> Task.attempt HandleUpdateEvent
            )


{-| -}
record : Model -> Event -> Maybe (Cmd Msg)
record { credentials, applicationId, endpointId, sessionId, region } { eventId, eventType, timestamp, attributes } =
    credentials
        |> Maybe.map
            (\creds ->
                AWS.Http.send (CognitoIdentity.service region)
                    creds
                    (Pinpoint.putEvents
                        { applicationId = applicationId
                        , eventsRequest =
                            { batchItem =
                                Just <|
                                    Dict.fromList
                                        [ ( endpointId
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
                                                        [ ( eventId
                                                          , { appPackageName = Nothing
                                                            , appTitle = Nothing
                                                            , appVersionCode = Nothing
                                                            , attributes = Just attributes
                                                            , clientSdkVersion = Nothing
                                                            , eventType = Just eventType
                                                            , metrics = Nothing
                                                            , sdkName = Nothing
                                                            , session =
                                                                Just
                                                                    { duration = Nothing
                                                                    , id = Just sessionId
                                                                    , startTimestamp = Just timestamp
                                                                    , stopTimestamp = Nothing
                                                                    }
                                                            , timestamp = Just timestamp
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
