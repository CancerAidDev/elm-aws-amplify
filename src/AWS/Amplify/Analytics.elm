module AWS.Amplify.Analytics exposing
    ( Config
    , Event
    , configure
    , record
    )

import AWS.Amplify.ClientInfo exposing (ClientInfo)
import AWS.Config
import AWS.Credentials as Credentials
import AWS.Http
import AWS.Pinpoint as Pinpoint
import Dict exposing (Dict)
import Iso8601
import Task exposing (Task)
import Time


type alias Config =
    { credentials : Credentials.Credentials
    , clientInfo : ClientInfo
    , applicationId : String
    , sessionId : String
    , identityId : String
    , region : AWS.Config.Region
    }


type alias Endpoint =
    { endpointId : String
    , requestId : String
    }


type alias Event =
    { eventId : String
    , name : String
    , timestamp : Time.Posix
    , attributes : Dict String String
    }


{-| -}
configure : Config -> Endpoint -> Task (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.UpdateEndpointResponse
configure { credentials, clientInfo, applicationId, identityId, region } { endpointId, requestId } =
    AWS.Http.send (Pinpoint.service region)
        credentials
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
                , location = Nothing
                , metrics = Just Dict.empty
                , optOut = Nothing
                , requestId = Just <| requestId
                , user =
                    Just
                        { userAttributes = Just Dict.empty
                        , userId = Just identityId
                        }
                }
            }
        )


{-| -}
record : Config -> Event -> Task (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.PutEventsResponse
record { credentials, applicationId, identityId, sessionId, region } { eventId, name, timestamp, attributes } =
    AWS.Http.send (Pinpoint.service region)
        credentials
        (Pinpoint.putEvents
            { applicationId = applicationId
            , eventsRequest =
                { batchItem =
                    Dict.fromList
                        [ ( identityId
                          , { endpoint =
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
                                Dict.fromList
                                    [ ( eventId
                                      , { appPackageName = Nothing
                                        , appTitle = Nothing
                                        , appVersionCode = Nothing
                                        , attributes = Just attributes
                                        , clientSdkVersion = Nothing
                                        , eventType = name
                                        , metrics = Nothing
                                        , sdkName = Nothing
                                        , session =
                                            Just
                                                { duration = Nothing
                                                , id = sessionId
                                                , startTimestamp = Iso8601.fromTime timestamp
                                                , stopTimestamp = Nothing
                                                }
                                        , timestamp = Iso8601.fromTime timestamp
                                        }
                                      )
                                    ]
                            }
                          )
                        ]
                }
            }
        )
