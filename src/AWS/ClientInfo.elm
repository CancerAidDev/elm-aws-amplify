module AWS.ClientInfo exposing (ClientInfo, decoder)

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline


type alias ClientInfo =
    { platform : String
    , make : String
    , model : String
    , version : String
    , appVersion : String
    , language : String
    , timezone : String
    }


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
