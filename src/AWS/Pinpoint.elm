module AWS.Pinpoint exposing
    ( service
    , updateEndpoint, putEvents
    , ChannelType(..)
    , EndpointDemographic, EndpointItemResponse, EndpointLocation, EndpointUser
    , Event, EventItemResponse, EventsBatch, EventsRequest, EventsResponse
    , ItemsResponse
    , PublicEndpoint
    , PutEventsRequest, PutEventsResponse
    , Session
    , UpdateEndpointRequest, UpdateEndpointResponse
    )

{-|


## Amazon Pinpoint


# Service definition.

@docs service


# Service endpoints.

@docs updateEndpoint, putEvents


# API data model.

@docs Attributes
@docs ChannelType
@docs ChannelType
@docs EndpointDemographic, EndpointItemResponse, EndpointLocation, EndpointRequest, EndpointUser
@docs Event, EventItemResponse, EventsBatch, EventsRequest, EventsResponse
@docs ItemsResponse
@docs MessageBody
@docs Metrics
@docs PublicEndpoint
@docs PutEventsRequest, PutEventsResponse
@docs Session
@docs UpdateEndpointRequest, UpdateEndpointResponse

-}

import AWS.Config
import AWS.Http
import AWS.Service
import Codec exposing (Codec)
import Dict exposing (Dict)
import Enum exposing (Enum)
import Json.Decode
import Json.Decode.Pipeline as Pipeline


{-| Configuration for this service.
-}
service : AWS.Config.Region -> AWS.Service.Service
service region =
    AWS.Config.defineRegional "pinpoint" "2016-12-01" AWS.Config.REST_JSON AWS.Config.SignV4 region
        |> AWS.Config.withJsonVersion "1.1"
        |> AWS.Config.withTargetPrefix "Pinpoint"
        |> AWS.Config.withSigningName "mobiletargeting"
        |> AWS.Service.service


{-| Creates a new endpoint for an application or updates the settings and attributes of an existing endpoint for an application. You can also use this operation to define custom attributes for an endpoint. If an update includes one or more values for a custom attribute, Amazon Pinpoint replaces (overwrites) any existing values with the new values.
-}
updateEndpoint : UpdateEndpointRequest -> AWS.Http.Request AWS.Http.AWSAppError UpdateEndpointResponse
updateEndpoint req =
    let
        jsonBody =
            req.endpointRequest |> Codec.encoder endpointRequestCodec |> AWS.Http.jsonBody

        url =
            "/v1/apps/" ++ req.applicationId ++ "/endpoints/" ++ req.endpointId

        decoder =
            Json.Decode.succeed UpdateEndpointResponse
                |> Pipeline.optional "MessageBody" (Json.Decode.maybe (Codec.decoder messageBodyCodec)) Nothing
                |> AWS.Http.jsonBodyDecoder
    in
    AWS.Http.request "UpdateEndpoint" AWS.Http.PUT url jsonBody decoder AWS.Http.awsAppErrDecoder


{-| Creates a new event to record for endpoints, or creates or updates endpoint data that existing events are associated with.
-}
putEvents : PutEventsRequest -> AWS.Http.Request AWS.Http.AWSAppError PutEventsResponse
putEvents req =
    let
        jsonBody =
            req.eventsRequest |> Codec.encoder eventsRequestCodec |> AWS.Http.jsonBody

        url =
            "/v1/apps/" ++ req.applicationId ++ "/events"

        decoder =
            Json.Decode.succeed PutEventsResponse
                |> Pipeline.optional "EventsResponse" (Json.Decode.maybe (Codec.decoder eventsResponseCodec)) Nothing
                |> AWS.Http.jsonBodyDecoder
    in
    AWS.Http.request "PutEvents" AWS.Http.POST url jsonBody decoder AWS.Http.awsAppErrDecoder


{-| The UpdateEndpointRequest data model.
-}
type alias UpdateEndpointRequest =
    { applicationId : String
    , endpointId : String
    , endpointRequest : EndpointRequest
    }


{-| The UpdateEndpointResponse data model.
-}
type alias UpdateEndpointResponse =
    { messageBody : Maybe MessageBody
    }


{-| The MessageBody data model.
-}
type alias MessageBody =
    { message : Maybe String
    , requestID : Maybe String
    }


messageBodyCodec : Codec MessageBody
messageBodyCodec =
    Codec.object MessageBody
        |> Codec.optionalField "Message" .message Codec.string
        |> Codec.optionalField "RequestID" .requestID Codec.string
        |> Codec.buildObject


{-| The EndpointRequest data model.
-}
type alias EndpointRequest =
    { address : Maybe String
    , attributes : Maybe Attributes
    , channelType : Maybe ChannelType
    , demographic : Maybe EndpointDemographic
    , effectiveDate : Maybe String
    , endpointStatus : Maybe String
    , location : Maybe EndpointLocation
    , metrics : Maybe Metrics
    , optOut : Maybe String
    , requestId : Maybe String
    , user : Maybe EndpointUser
    }


endpointRequestCodec : Codec EndpointRequest
endpointRequestCodec =
    Codec.object EndpointRequest
        |> Codec.optionalField "Address" .address Codec.string
        |> Codec.optionalField "Attributes" .attributes attributesCodec
        |> Codec.optionalField "ChannelType" .channelType channelTypeCodec
        |> Codec.optionalField "Demographic" .demographic endpointDemographicCodec
        |> Codec.optionalField "EffectiveDate" .effectiveDate Codec.string
        |> Codec.optionalField "EndpointStatus" .endpointStatus Codec.string
        |> Codec.optionalField "Location" .location endpointLocationCodec
        |> Codec.optionalField "Metrics" .metrics metricsCodec
        |> Codec.optionalField "OptOut" .optOut Codec.string
        |> Codec.optionalField "RequestId" .requestId Codec.string
        |> Codec.optionalField "User" .user endpointUserCodec
        |> Codec.buildObject


{-| The Attributes data model.
-}
type alias Attributes =
    Dict String (List String)


attributesCodec : Codec Attributes
attributesCodec =
    Codec.dict (Codec.list Codec.string)


{-| The Metrics data model.
-}
type alias Metrics =
    Dict String Float


metricsCodec : Codec Metrics
metricsCodec =
    Codec.dict Codec.float


{-| The PutEventsRequest data model.
-}
type alias PutEventsRequest =
    { applicationId : String
    , eventsRequest : EventsRequest
    }


{-| The EventsRequest data model.
-}
type alias EventsRequest =
    { batchItem : Maybe (Dict String EventsBatch) }


eventsRequestCodec : Codec EventsRequest
eventsRequestCodec =
    Codec.object EventsRequest
        |> Codec.optionalField "BatchItem" .batchItem (Codec.dict eventsBatchCodec)
        |> Codec.buildObject


{-| The EventsBatch data model.
-}
type alias EventsBatch =
    { endpoint : Maybe PublicEndpoint
    , events : Maybe (Dict String Event)
    }


eventsBatchCodec : Codec EventsBatch
eventsBatchCodec =
    Codec.object EventsBatch
        |> Codec.optionalField "Endpoint" .endpoint publicEndpointCodec
        |> Codec.optionalField "Events" .events (Codec.dict eventCodec)
        |> Codec.buildObject


{-| The PublicEndpoint data model.
-}
type alias PublicEndpoint =
    { address : Maybe String
    , attributes : Maybe Attributes
    , channelType : Maybe ChannelType
    , demographic : Maybe EndpointDemographic
    , effectiveDate : Maybe String
    , endpointStatus : Maybe String
    , location : Maybe EndpointLocation
    , metrics : Maybe Metrics
    , optOut : Maybe String
    , requestId : Maybe String
    , user : Maybe EndpointUser
    }


publicEndpointCodec : Codec PublicEndpoint
publicEndpointCodec =
    Codec.object PublicEndpoint
        |> Codec.optionalField "Address" .address Codec.string
        |> Codec.optionalField "Attributes" .attributes attributesCodec
        |> Codec.optionalField "ChannelType" .channelType channelTypeCodec
        |> Codec.optionalField "Demographic" .demographic endpointDemographicCodec
        |> Codec.optionalField "EffectiveDate" .effectiveDate Codec.string
        |> Codec.optionalField "EndpointStatus" .endpointStatus Codec.string
        |> Codec.optionalField "Location" .location endpointLocationCodec
        |> Codec.optionalField "Metrics" .metrics metricsCodec
        |> Codec.optionalField "OptOut" .optOut Codec.string
        |> Codec.optionalField "RequestId" .requestId Codec.string
        |> Codec.optionalField "User" .user endpointUserCodec
        |> Codec.buildObject


{-| The ChannelType data model.
-}
type ChannelType
    = ChanelTypeAdm
    | ChanelTypeApns
    | ChanelTypeApnsSandbox
    | ChanelTypeApnsVoip
    | ChanelTypeApnsVoipSandbox
    | ChanelTypeBaidu
    | ChanelTypeCustom
    | ChanelTypeEmail
    | ChanelTypeGcm
    | ChanelTypeInApp
    | ChanelTypePush
    | ChanelTypeSms
    | ChanelTypeVoice


channelType : Enum ChannelType
channelType =
    Enum.define
        [ ChanelTypeAdm
        , ChanelTypeApns
        , ChanelTypeApnsSandbox
        , ChanelTypeApnsVoip
        , ChanelTypeApnsVoipSandbox
        , ChanelTypeBaidu
        , ChanelTypeCustom
        , ChanelTypeEmail
        , ChanelTypeGcm
        , ChanelTypeInApp
        , ChanelTypePush
        , ChanelTypeSms
        , ChanelTypeVoice
        ]
        (\val ->
            case val of
                ChanelTypeAdm ->
                    "ADM"

                ChanelTypeApns ->
                    "APNS"

                ChanelTypeApnsSandbox ->
                    "APNS_SANDBOX"

                ChanelTypeApnsVoip ->
                    "APNS_VOIP"

                ChanelTypeApnsVoipSandbox ->
                    "APNS_VOIP_SANDBOX"

                ChanelTypeBaidu ->
                    "BAIDU"

                ChanelTypeCustom ->
                    "CUSTOM"

                ChanelTypeEmail ->
                    "EMAIL"

                ChanelTypeGcm ->
                    "GCM"

                ChanelTypeInApp ->
                    "IN_APP"

                ChanelTypePush ->
                    "PUSH"

                ChanelTypeSms ->
                    "SMS"

                ChanelTypeVoice ->
                    "VOICE"
        )


channelTypeCodec : Codec ChannelType
channelTypeCodec =
    Codec.build (Enum.encoder channelType) (Enum.decoder channelType)


{-| The EndpointDemographic data model.
-}
type alias EndpointDemographic =
    { appVersion : Maybe String
    , locale : Maybe String
    , make : Maybe String
    , model : Maybe String
    , modelVersion : Maybe String
    , platform : Maybe String
    , platformVersion : Maybe String
    , timezone : Maybe String
    }


endpointDemographicCodec : Codec EndpointDemographic
endpointDemographicCodec =
    Codec.object EndpointDemographic
        |> Codec.optionalField "AppVersion" .appVersion Codec.string
        |> Codec.optionalField "Locale" .locale Codec.string
        |> Codec.optionalField "Make" .make Codec.string
        |> Codec.optionalField "Model" .model Codec.string
        |> Codec.optionalField "ModelVersion" .modelVersion Codec.string
        |> Codec.optionalField "Platform" .platform Codec.string
        |> Codec.optionalField "PlatformVersion" .platformVersion Codec.string
        |> Codec.optionalField "Timezone" .timezone Codec.string
        |> Codec.buildObject


{-| The EndpointLocation data model.
-}
type alias EndpointLocation =
    { city : Maybe String
    , country : Maybe String
    , latitude : Maybe Float
    , longitude : Maybe Float
    , postalCode : Maybe String
    , region : Maybe String
    }


endpointLocationCodec : Codec EndpointLocation
endpointLocationCodec =
    Codec.object EndpointLocation
        |> Codec.optionalField "City" .city Codec.string
        |> Codec.optionalField "Country" .country Codec.string
        |> Codec.optionalField "Latitude" .latitude Codec.float
        |> Codec.optionalField "Longitude" .longitude Codec.float
        |> Codec.optionalField "PostalCode" .postalCode Codec.string
        |> Codec.optionalField "Region" .region Codec.string
        |> Codec.buildObject


{-| The EndpointUser data model.
-}
type alias EndpointUser =
    { userAttributes : Maybe Attributes
    , userId : Maybe String
    }


endpointUserCodec : Codec EndpointUser
endpointUserCodec =
    Codec.object EndpointUser
        |> Codec.optionalField "UserAttributes" .userAttributes attributesCodec
        |> Codec.optionalField "UserId" .userId Codec.string
        |> Codec.buildObject


{-| The Event data model.
-}
type alias Event =
    { appPackageName : Maybe String
    , appTitle : Maybe String
    , appVersionCode : Maybe String
    , attributes : Maybe (Dict String String)
    , clientSdkVersion : Maybe String
    , eventType : Maybe String
    , metrics : Maybe Metrics
    , sdkName : Maybe String
    , session : Maybe Session
    , timestamp : Maybe String
    }


eventCodec : Codec Event
eventCodec =
    Codec.object Event
        |> Codec.optionalField "AppPackageName" .appPackageName Codec.string
        |> Codec.optionalField "AppTitle" .appTitle Codec.string
        |> Codec.optionalField "AppVersionCode" .appVersionCode Codec.string
        |> Codec.optionalField "Attributes" .attributes (Codec.dict Codec.string)
        |> Codec.optionalField "ClientSdkVersion" .clientSdkVersion Codec.string
        |> Codec.optionalField "EventType" .eventType Codec.string
        |> Codec.optionalField "Metrics" .metrics metricsCodec
        |> Codec.optionalField "SdkName" .sdkName Codec.string
        |> Codec.optionalField "Session" .session sessionCodec
        |> Codec.optionalField "Timestamp" .timestamp Codec.string
        |> Codec.buildObject


{-| The Session data model.
-}
type alias Session =
    { duration : Maybe Float
    , id : Maybe String
    , startTimestamp : Maybe String
    , stopTimestamp : Maybe String
    }


sessionCodec : Codec Session
sessionCodec =
    Codec.object Session
        |> Codec.optionalField "Duration" .duration Codec.float
        |> Codec.optionalField "Id" .id Codec.string
        |> Codec.optionalField "StartTimestamp" .startTimestamp Codec.string
        |> Codec.optionalField "StopTimestamp" .stopTimestamp Codec.string
        |> Codec.buildObject


{-| The PutEventsResponse data model.
-}
type alias PutEventsResponse =
    { eventsResponse : Maybe EventsResponse }


{-| The EventsResponse data model.
-}
type alias EventsResponse =
    { results : Maybe (Dict String ItemsResponse) }


eventsResponseCodec : Codec EventsResponse
eventsResponseCodec =
    Codec.object EventsResponse
        |> Codec.optionalField "Results" .results (Codec.dict itemsResponseCodec)
        |> Codec.buildObject


{-| The ItemsResponse data model.
-}
type alias ItemsResponse =
    { endpointItemResponse : Maybe EndpointItemResponse
    , eventsItemResponse : Maybe (Dict String EventItemResponse)
    }


itemsResponseCodec : Codec ItemsResponse
itemsResponseCodec =
    Codec.object ItemsResponse
        |> Codec.optionalField "EndpointItemResponse" .endpointItemResponse endpointItemResponseCodec
        |> Codec.optionalField "EventsItemResponse" .eventsItemResponse (Codec.dict eventItemResponseCodec)
        |> Codec.buildObject


{-| The EndpointItemResponse data model.
-}
type alias EndpointItemResponse =
    { message : Maybe String
    , statusCode : Maybe Int
    }


endpointItemResponseCodec : Codec EndpointItemResponse
endpointItemResponseCodec =
    Codec.object EndpointItemResponse
        |> Codec.optionalField "Message" .message Codec.string
        |> Codec.optionalField "StatusCode" .statusCode Codec.int
        |> Codec.buildObject


{-| The EventItemResponse data model.
-}
type alias EventItemResponse =
    { message : Maybe String
    , statusCode : Maybe Int
    }


eventItemResponseCodec : Codec EventItemResponse
eventItemResponseCodec =
    Codec.object EventItemResponse
        |> Codec.optionalField "Message" .message Codec.string
        |> Codec.optionalField "StatusCode" .statusCode Codec.int
        |> Codec.buildObject
