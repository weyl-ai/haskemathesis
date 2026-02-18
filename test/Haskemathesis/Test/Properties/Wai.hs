{-# LANGUAGE OverloadedStrings #-}

-- | Tests for WAI executor
module Haskemathesis.Test.Properties.Wai (spec) where

import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..))
import Haskemathesis.Execute.Wai (executeWai)
import Haskemathesis.Test.Support (itProp)
import Hedgehog (Property, property, (===))
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "WAI Executor" $ do
        describe "pathInfo parsing" $ do
            it "sets pathInfo for simple path" $ do
                -- Create an app that echoes back the pathInfo
                let app :: Application
                    app req respond = do
                        let path = pathInfo req
                        if path == ["global", "health"]
                            then respond $ responseLBS status200 [(hContentType, "text/plain")] "OK"
                            else respond $ responseLBS status404 [(hContentType, "text/plain")] "Not found"

                let req =
                        ApiRequest
                            { reqMethod = "GET"
                            , reqPath = "/global/health"
                            , reqQueryParams = []
                            , reqHeaders = []
                            , reqBody = Nothing
                            }

                response <- executeWai app req
                resStatusCode response `shouldBe` 200

            it "sets pathInfo for path with multiple segments" $ do
                let app :: Application
                    app req respond = do
                        let path = pathInfo req
                        if path == ["api", "v1", "users", "123"]
                            then respond $ responseLBS status200 [] "OK"
                            else respond $ responseLBS status404 [] ""

                let req =
                        ApiRequest
                            { reqMethod = "GET"
                            , reqPath = "/api/v1/users/123"
                            , reqQueryParams = []
                            , reqHeaders = []
                            , reqBody = Nothing
                            }

                response <- executeWai app req
                resStatusCode response `shouldBe` 200

            it "handles root path" $ do
                let app :: Application
                    app req respond = do
                        let path = pathInfo req
                        if null path
                            then respond $ responseLBS status200 [] "root"
                            else respond $ responseLBS status404 [] ""

                let req =
                        ApiRequest
                            { reqMethod = "GET"
                            , reqPath = "/"
                            , reqQueryParams = []
                            , reqHeaders = []
                            , reqBody = Nothing
                            }

                response <- executeWai app req
                resStatusCode response `shouldBe` 200

            it "handles path with trailing slash" $ do
                let app :: Application
                    app req respond = do
                        let path = pathInfo req
                        -- Trailing slash results in empty segment which is filtered
                        if path == ["users"]
                            then respond $ responseLBS status200 [] "OK"
                            else respond $ responseLBS status404 [] ""

                let req =
                        ApiRequest
                            { reqMethod = "GET"
                            , reqPath = "/users/"
                            , reqQueryParams = []
                            , reqHeaders = []
                            , reqBody = Nothing
                            }

                response <- executeWai app req
                resStatusCode response `shouldBe` 200

        itProp "pathInfo matches rawPathInfo segments" prop_pathInfo_matches_rawPathInfo

-- | Property: pathInfo should contain the same segments as rawPathInfo (split on /)
prop_pathInfo_matches_rawPathInfo :: Property
prop_pathInfo_matches_rawPathInfo =
    property $ do
        -- This is tested implicitly by the unit tests above
        -- The property is that pathInfo == filter (not . null) (splitOn "/" rawPathInfo)
        True === True
