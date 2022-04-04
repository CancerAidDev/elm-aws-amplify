module AWS.Amplify.ClientInfo exposing (ClientInfo, decoder, toEndpointDemographic)

{-| ClientInfo data type.


# ClientInfo

@docs ClientInfo, decoder, toEndpointDemographic

-}

import AWS.Pinpoint exposing (EndpointDemographic)
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


{-| Data type for storing client information.

See [ClientDevice/browser.ts][browser] for how to populate this record

[browser]: https://github.com/aws-amplify/amplify-js/blob/5b4641b8568e3106db81958f1cb2ce0b6d684ab6/packages/core/src/ClientDevice/browser.ts

-}
type alias ClientInfo =
    { platform : String
    , make : String
    , model : String
    , version : String
    , appVersion : String
    , language : String
    , timezone : String
    }


{-| Json Decoder for ClientInfo
-}
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


{-| Convert to EndpointDemographic
-}
toEndpointDemographic : ClientInfo -> EndpointDemographic
toEndpointDemographic clientInfo =
    { appVersion = Just clientInfo.appVersion
    , locale = Nothing
    , make = Just clientInfo.make
    , model = Just clientInfo.model
    , modelVersion = Just clientInfo.version
    , platform = Just clientInfo.platform
    , platformVersion = Nothing
    , timezone = Nothing
    }
