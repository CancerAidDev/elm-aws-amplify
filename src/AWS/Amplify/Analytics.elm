module AWS.Amplify.Analytics exposing
    ( Config, Endpoint, configure
    , Event, record
    )

{-| Collect Analytics data for your application using Amazon Pinpoint.


# Configure

@docs Config, Endpoint, configure


# Record

@docs Event, record

-}

import AWS.Amplify.ClientInfo as ClientInfo exposing (ClientInfo)
import AWS.Config
import AWS.Credentials exposing (Credentials)
import AWS.Http
import AWS.Pinpoint as Pinpoint
import Dict exposing (Dict)
import Iso8601
import Task exposing (Task)
import Time



-- CONFIGURE


{-| Analytics config
-}
type alias Config =
    { credentials : Credentials
    , clientInfo : ClientInfo
    , applicationId : String
    , sessionId : String
    , sessionStartTime : Time.Posix
    , identityId : String
    , region : AWS.Config.Region
    }


{-| Endpoint request
-}
type alias Endpoint =
    { endpointId : String
    , requestId : String
    }


{-| Configure analytics
-}
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
                , demographic = Just (ClientInfo.toEndpointDemographic clientInfo)
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



-- RECORD


{-| Event record
-}
type alias Event =
    { eventId : String
    , name : String
    , attributes : Dict String String
    }


{-| Record an event
-}
record : Config -> Event -> Task (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.PutEventsResponse
record config event =
    Time.now |> Task.andThen (recordWithTime config event)


recordWithTime : Config -> Event -> Time.Posix -> Task (AWS.Http.Error AWS.Http.AWSAppError) Pinpoint.PutEventsResponse
recordWithTime { credentials, clientInfo, applicationId, identityId, sessionId, sessionStartTime, region } { eventId, name, attributes } eventTime =
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
                                , demographic = Just (ClientInfo.toEndpointDemographic clientInfo)
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
                                                , startTimestamp = Iso8601.fromTime sessionStartTime
                                                , stopTimestamp = Nothing
                                                }
                                        , timestamp = Iso8601.fromTime eventTime
                                        }
                                      )
                                    ]
                            }
                          )
                        ]
                }
            }
        )
