module AWS.Amplify.Auth exposing
    ( Identity, Credentials
    , Config, configure, getCredentials
    )

{-| Configure authentication using Amazon Cognito.


# Identity

@docs Identity, Credentials


# Configure

@docs Config, configure, getCredentials

-}

import AWS.CognitoIdentity as CognitoIdentity
import AWS.Config
import AWS.Http
import Http
import Task exposing (Task)
import Time



-- IDENTITY


{-| IdentityId
-}
type alias Identity =
    { identityId : String
    , credentials : Credentials
    }


{-| Credentials
-}
type alias Credentials =
    { accessKeyId : String
    , secretAccessKey : String
    , expiration : Time.Posix
    , sessionToken : Maybe String
    }



-- CONFIGURE


{-| Auth config
-}
type alias Config =
    { region : AWS.Config.Region
    , identityPoolId : String
    }


{-| Configure authentication.

Fetches identity id from identity pool and then fetches credentials for identity id.

    AWS.Amplify.Auth.configure
        { region = "ap-southeast-2"
        , identityPoolId = "ap-southeast-2:123e4567-e89b-12d3-a456-426614174000"
        }

-}
configure : Config -> Task (AWS.Http.Error AWS.Http.AWSAppError) Identity
configure { region, identityPoolId } =
    getId region identityPoolId
        |> Task.andThen
            (\{ identityId } ->
                identityId
                    |> Maybe.map (getCredentials region)
                    |> Maybe.withDefault (Task.fail (AWS.Http.HttpError (Http.BadBody "IdentityId is null")))
            )


{-| Get identityId
-}
getId : AWS.Config.Region -> String -> Task (AWS.Http.Error AWS.Http.AWSAppError) CognitoIdentity.GetIdResponse
getId region identityPoolId =
    AWS.Http.sendUnsigned (CognitoIdentity.service region)
        (CognitoIdentity.getId
            { accountId = Nothing
            , identityPoolId = identityPoolId
            , logins = Nothing
            }
        )


{-| Get credentials for identityId
-}
getCredentials : AWS.Config.Region -> String -> Task (AWS.Http.Error AWS.Http.AWSAppError) Identity
getCredentials region identityId =
    AWS.Http.sendUnsigned (CognitoIdentity.service region)
        (CognitoIdentity.getCredentialsForIdentity
            { customRoleArn = Nothing
            , identityId = identityId
            , logins = Nothing
            }
        )
        |> Task.andThen
            (\{ credentials } ->
                credentials
                    |> Maybe.andThen
                        (\{ accessKeyId, secretKey, sessionToken, expiration } ->
                            Maybe.map3
                                (\accessKeyId_ secretAccessKey_ expiration_ ->
                                    let
                                        credentials_ =
                                            { accessKeyId = accessKeyId_
                                            , secretAccessKey = secretAccessKey_
                                            , sessionToken = sessionToken
                                            , expiration = expiration_
                                            }
                                    in
                                    Task.succeed
                                        { identityId = identityId
                                        , credentials = credentials_
                                        }
                                )
                                accessKeyId
                                secretKey
                                expiration
                        )
                    |> Maybe.withDefault (Task.fail (AWS.Http.HttpError (Http.BadBody "Missing identityId or credentials")))
            )
