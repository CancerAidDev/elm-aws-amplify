module Main exposing (main)

import AWS.Amplify as Amplify
import AWS.ClientInfo exposing (ClientInfo)
import AWS.Credentials exposing (Credentials)
import Browser
import Dict
import Html exposing (Html, button, div, form, h1, h2, input, label, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Random.Pcg.Extended exposing (initialSeed)
import Task
import Time


type alias Model =
    { amplify : Amplify.Model
    , name : String
    , key : String
    , value : String
    }


type alias Flags =
    { seed : ( Int, List Int )
    , appId : String
    , identityPoolId : String
    , clientInfo : ClientInfo
    , region : String
    }


type Msg
    = Record String Credentials
    | RecordWithTime String Credentials Time.Posix
    | AmplifyMsg Amplify.Msg
    | UpdateName String
    | UpdateKey String
    | UpdateValue String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Record identityId credentials ->
            ( model
            , Time.now
                |> Task.attempt
                    (Result.map (RecordWithTime identityId credentials)
                        >> Result.withDefault NoOp
                    )
            )

        RecordWithTime identityId credentials time ->
            Amplify.record identityId
                credentials
                model.amplify
                { name = model.name
                , timestamp = time
                , attributes = Dict.fromList [ ( model.key, model.value ) ]
                }
                |> Tuple.mapBoth (\updated -> { model | amplify = updated }) (Cmd.map AmplifyMsg)

        AmplifyMsg subMsg ->
            Amplify.update subMsg model.amplify
                |> Tuple.mapBoth (\updated -> { model | amplify = updated }) (Cmd.map AmplifyMsg)

        UpdateName val ->
            ( { model | name = val }, Cmd.none )

        UpdateKey val ->
            ( { model | key = val }, Cmd.none )

        UpdateValue val ->
            ( { model | value = val }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case ( model.amplify.credentials, model.amplify.identityId ) of
        ( Just credentials, Just identityId ) ->
            div []
                [ h2 [] [ text "IdentityId" ]
                , div [] [ text identityId ]
                , form []
                    [ h1 [] [ text "Record" ]
                    , h2 [] [ text "Name" ]
                    , input [ value model.name, onInput UpdateName ] []
                    , h2 [] [ text "Attributes" ]
                    , div []
                        [ div []
                            [ label [] [ text "Key: " ]
                            , input [ value model.key, onInput UpdateKey ] []
                            ]
                        , div []
                            [ label [] [ text "Value: " ]
                            , input [ value model.value, onInput UpdateValue ] []
                            ]
                        ]
                    , div [] [ button [ onClick <| Record identityId credentials ] [ text "Submit" ] ]
                    ]
                ]

        _ ->
            text ""


init : Flags -> ( Model, Cmd Msg )
init { seed, identityPoolId, clientInfo, appId, region } =
    let
        ( baseSeed, seedExtension ) =
            seed

        ( amplify, cmd ) =
            Amplify.init
                { identityPoolId = identityPoolId
                , clientInfo = clientInfo
                , applicationId = appId
                , region = region
                , seed = initialSeed baseSeed seedExtension
                }
    in
    ( { amplify = amplify, name = "Test", key = "Hello", value = "World" }, Cmd.map AmplifyMsg cmd )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
