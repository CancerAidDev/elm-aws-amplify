module Main exposing (main)

import AWS.Amplify as Amplify
import Browser
import Html exposing (Html)
import Random.Pcg.Extended exposing (initialSeed)


type alias Model =
    Amplify.Model


type alias Flags =
    { seed : ( Int, List Int )
    , appId : String
    , identityPoolId : String
    , clientInfo :
        { platform : String
        , make : String
        , model : String
        , version : String
        , appVersion : String
        , language : String
        , timezone : String
        }
    , region : String
    }


type Msg
    = Record String String
    | AmplifyMsg Amplify.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Record _ _ ->
            ( model, Cmd.none )

        AmplifyMsg subMsg ->
            Amplify.update subMsg model |> Tuple.mapSecond (Cmd.map AmplifyMsg)


view : Model -> Html Msg
view _ =
    Html.text ""


init : Flags -> ( Model, Cmd Msg )
init { seed, identityPoolId, clientInfo, appId, region } =
    let
        ( baseSeed, seedExtension ) =
            seed
    in
    Amplify.init
        { identityPoolId = identityPoolId
        , clientInfo = clientInfo
        , applicationId = appId
        , region = region
        , seed = initialSeed baseSeed seedExtension
        }
        |> Tuple.mapSecond (Cmd.map AmplifyMsg)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
