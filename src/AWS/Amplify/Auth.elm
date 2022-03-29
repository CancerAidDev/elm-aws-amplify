module AWS.Amplify.Auth exposing
    ( Identity
    , Config, configure
    )

{-| Configure authentication using Amazon Cognito.


# Identity

@docs Identity


# Configure

@docs Config, configure

-}

import AWS.CognitoIdentity as CognitoIdentity
import AWS.Config
import AWS.Credentials exposing (Credentials)
import AWS.Http
import Http
import Maybe.Extra as MaybeExtra
import Task exposing (Task)



-- IDENTITY


{-| IdentityId
-}
type alias Identity =
    { identityId : String
    , credentials : Credentials
    }



-- CONFIGURE


{-| Auth config
-}
type alias Config =
    { region : AWS.Config.Region
    , identityPoolId : String
    }


{-| Configure authentication.

Fetches identity id from identity pool and the fetches credentials for identity id.

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
        |> Task.andThen
            (\{ credentials, identityId } ->
                MaybeExtra.andThen2
                    (\id { accessKeyId, secretKey, sessionToken } ->
                        Maybe.map2
                            (\accessKeyId_ secretAccessKey_ ->
                                let
                                    credentials_ =
                                        { accessKeyId = accessKeyId_
                                        , secretAccessKey = secretAccessKey_
                                        , sessionToken = sessionToken
                                        }
                                in
                                Task.succeed
                                    { identityId = id
                                    , credentials = credentials_
                                    }
                            )
                            accessKeyId
                            secretKey
                    )
                    identityId
                    credentials
                    |> Maybe.withDefault (Task.fail (AWS.Http.HttpError (Http.BadBody "Missing identityId or credentials")))
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
getCredentials : AWS.Config.Region -> String -> Task (AWS.Http.Error AWS.Http.AWSAppError) CognitoIdentity.GetCredentialsForIdentityResponse
getCredentials region identityId =
    AWS.Http.sendUnsigned (CognitoIdentity.service region)
        (CognitoIdentity.getCredentialsForIdentity
            { customRoleArn = Nothing
            , identityId = identityId
            , logins = Nothing
            }
        )
