-- | Built-in response checks and default check sets.
module Haskemathesis.Check.Standard (
    defaultChecks,
    allChecks,
    notAServerError,
    responseSchemaConformance,
    statusCodeConformance,
    contentTypeConformance,
    responseHeadersConformance,
) where

import Haskemathesis.Check.Standard.ContentType (contentTypeConformance)
import Haskemathesis.Check.Standard.Headers (responseHeadersConformance)
import Haskemathesis.Check.Standard.ResponseSchema (responseSchemaConformance)
import Haskemathesis.Check.Standard.Status (notAServerError, statusCodeConformance)

import Haskemathesis.Check.Types

defaultChecks :: [Check]
defaultChecks =
    [ notAServerError
    , responseSchemaConformance
    , statusCodeConformance
    ]

allChecks :: [Check]
allChecks =
    defaultChecks
        <> [ contentTypeConformance
           , responseHeadersConformance
           ]
