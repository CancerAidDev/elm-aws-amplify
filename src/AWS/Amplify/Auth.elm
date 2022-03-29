module AWS.Amplify.Auth exposing (Config, Identity, configure)

import AWS.CognitoIdentity as CognitoIdentity
import AWS.Config
import AWS.Credentials as Credentials
import AWS.Http
import Http
import Maybe.Extra as MaybeExtra
import Task exposing (Task)


type alias Config =
    { region : AWS.Config.Region
    , identityPoolId : String
    }


type alias Identity =
    { identityId : String
    , credentials : Credentials.Credentials
    }


configure : Config -> Task (AWS.Http.Error AWS.Http.AWSAppError) Identity
configure { identityPoolId, region } =
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


getId : AWS.Config.Region -> String -> Task (AWS.Http.Error AWS.Http.AWSAppError) CognitoIdentity.GetIdResponse
getId region identityPoolId =
    AWS.Http.sendUnsigned (CognitoIdentity.service region)
        (CognitoIdentity.getId
            { accountId = Nothing
            , identityPoolId = identityPoolId
            , logins = Nothing
            }
        )


getCredentials : AWS.Config.Region -> String -> Task (AWS.Http.Error AWS.Http.AWSAppError) CognitoIdentity.GetCredentialsForIdentityResponse
getCredentials region identityId =
    AWS.Http.sendUnsigned (CognitoIdentity.service region)
        (CognitoIdentity.getCredentialsForIdentity
            { customRoleArn = Nothing
            , identityId = identityId
            , logins = Nothing
            }
        )
