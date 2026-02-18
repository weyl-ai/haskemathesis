{- | Built-in response checks and default check sets.

This module provides a collection of standard checks for validating
API responses against OpenAPI specifications. These checks cover
common validation scenarios like status codes, response schemas,
content types, and headers.

=== Using Default Check Sets

For most use cases, you can use one of the predefined check sets:

@
import Haskemathesis.Check.Standard (defaultChecks)
import Haskemathesis.Property (propertiesForSpec)

props = propertiesForSpec Nothing defaultChecks executor ops
@

=== Available Check Sets

* 'defaultChecks' - Essential checks (server errors, schema conformance, status codes)
* 'allChecks' - All available checks including content type and header validation

=== Creating Custom Check Sets

You can combine individual checks to create custom check sets:

@
myChecks :: [Check]
myChecks =
    [ notAServerError
    , responseSchemaConformance
    , statusCodeConformance
    ]
@
-}
module Haskemathesis.Check.Standard (
    -- * Check Sets
    defaultChecks,
    allChecks,

    -- * Individual Checks
    notAServerError,
    responseSchemaConformance,
    statusCodeConformance,
    contentTypeConformance,
    responseHeadersConformance,
)
where

import Haskemathesis.Check.Standard.ContentType (contentTypeConformance)
import Haskemathesis.Check.Standard.Headers (responseHeadersConformance)
import Haskemathesis.Check.Standard.ResponseSchema (responseSchemaConformance)
import Haskemathesis.Check.Standard.Status (notAServerError, statusCodeConformance)
import Haskemathesis.Check.Types

{- | Default set of essential checks.

This check set includes the most important validations that every
API should pass:

* 'notAServerError' - Ensures no 5xx status codes
* 'responseSchemaConformance' - Validates response bodies against schemas
* 'statusCodeConformance' - Ensures status codes are documented

=== When to Use

Use this as a starting point for most API testing scenarios. It provides
good coverage without being overly strict.
-}
defaultChecks :: [Check]
defaultChecks =
    [ notAServerError
    , responseSchemaConformance
    , statusCodeConformance
    ]

{- | Complete set of all available standard checks.

This check set includes all validations from 'defaultChecks' plus:

* 'contentTypeConformance' - Validates Content-Type headers
* 'responseHeadersConformance' - Validates response headers against schemas

=== When to Use

Use this when you want maximum validation coverage. Note that some
APIs may need to relax certain checks (e.g., if they intentionally
return non-standard content types).
-}
allChecks :: [Check]
allChecks =
    defaultChecks
        <> [ contentTypeConformance
           , responseHeadersConformance
           ]
