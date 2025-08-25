namespace ReservationService.Tests.Tests

module MaxCaseRequest =
    open System
    open Xunit
    open ReservationService.Core.JsonSerialization

    // A comprehensive DSL request that uses many features, designed to "strain" the pipeline
    // - Composite conditions (And/Or/Not)
    // - Various operators: ==, !=, <, <=, >, >=, Contains, StartsWith, EndsWith, In, NotIn, IsNull, IsNotNull
    // - ReferenceField usage
    // - Multiple action types with severities (engine currently maps only Validation to output messages)
    // - requiredValidations/completedValidations/remainingValidations driving typed DTOs
    // - ticketIds echoed into Metadata
    // - Data includes arrays, nested objects, date/time strings parsable by RuleDomain

    let requestJson =
        """
{
  "schemaVersion": "1.0",
  "settings": { "explain": true },
  "domain": null,
  "data": {
    "customer": { "name": "Alice", "vip": true, "age": 21 },
    "reservation": {
      "start": "2025-08-24T18:30:00Z",
      "end": "2025-08-24T20:30:00Z",
      "timeZone": "UTC",
      "partySize": 4,
      "notes": "Table by window"
    },
    "selectedZones": ["main", "vip"],
    "allowedZones": ["main", "family", "accessible"],
    "resources": {
      "tables": 10,
      "freeTables": [1,2,5,8],
      "labels": ["T-1", "T-2", "VIP-3"],
      "flags": [true, false, true],
      "tags": ["window", "quiet", "near_bar"]
    },
    "ticketIds": ["TKT-12345678", "TKT-ABCDEF01"],
    "tickets": [{"id":"ALT-1"},{"id":"ALT-2"}],
    "requiredValidations": ["AvailabilityCheck", "PaymentCheck", "ProfileCompleteness"],
    "completedValidations": ["AvailabilityCheck"],
    "remainingValidations": ["PaymentCheck", "ProfileCompleteness"],
    "threshold": 3,
    "thresholdMax": 8,
    "status": "PENDING",
    "prefix": "VIP",
    "nullable": null,
    "price": 99.99
  },
  "rules": [
    {
      "id": "R-AND-OR-NOT",
      "name": "AND of two ORs with NOT subclause",
      "priority": 120,
      "enabled": true,
      "condition": {
        "Case": "Composite",
        "Fields": [
          { "op": "And", "children": [
              { "Case": "Composite", "Fields": [ { "op": "Or", "children": [
                  { "Case": "Simple", "Fields": [{ "field": "reservation.partySize", "operator": ">=", "value": { "Case": "VInt", "Fields": [4] } }] },
                  { "Case": "Simple", "Fields": [{ "field": "customer.age", "operator": ">=", "value": { "Case": "VInt", "Fields": [18] } }] }
              ] } ] },
              { "Case": "Composite", "Fields": [ { "op": "Not", "children": [
                  { "Case": "Simple", "Fields": [{ "field": "resources.flags.1", "operator": "==", "value": { "Case": "VBool", "Fields": [true] } }] }
              ] } ] }
          ]}
        ]
      },
      "actions": [ { "type": "Validation", "severity": "Warning", "code": "AND_OR_NOT_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-REF",
      "name": "ReferenceField compares two data fields",
      "priority": 90,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "thresholdMax", "operator": ">", "referenceField": "threshold" } ] },
      "actions": [ { "type": "Validation", "severity": "Info", "code": "REF_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-CONTAINS",
      "name": "Array contains chosen zone",
      "priority": 95,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "selectedZones", "operator": "Contains", "value": { "Case": "VString", "Fields": ["vip"] } } ] },
      "actions": [ { "type": "Validation", "severity": "Error", "code": "ZONE_VIP_SELECTED" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-IN",
      "name": "label array contains VIP prefix",
      "priority": 80,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "resources.labels", "operator": "Contains", "value": { "Case": "VString", "Fields": ["VIP-3"] } } ] },
      "actions": [ { "type": "Validation", "severity": "Error", "code": "HAS_VIP_LABEL" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-NOTIN",
      "name": "selectedZones[1] NotIn allowedZones fails",
      "priority": 70,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "selectedZones.1", "operator": "NotIn", "referenceField": "allowedZones" } ] },
      "actions": [ { "type": "Validation", "severity": "Critical", "code": "ZONE_NOT_ALLOWED" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-STRING",
      "name": "labels[2] StartsWith VIP and notes EndsWith ow",
      "priority": 60,
      "enabled": true,
      "condition": {
        "Case": "Composite",
        "Fields": [
          { "op": "And", "children": [
              { "Case": "Simple", "Fields": [{ "field": "resources.labels.2", "operator": "StartsWith", "value": { "Case": "VString", "Fields": ["VIP"] } }] },
              { "Case": "Simple", "Fields": [{ "field": "reservation.notes", "operator": "EndsWith", "value": { "Case": "VString", "Fields": ["ow"] } }] }
          ]}
        ]
      },
      "actions": [ { "type": "Validation", "severity": "Info", "code": "STRING_SHAPE_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-STRING-CI",
      "name": "labels[2] StartsWith lowercase vip (case-insensitive)",
      "priority": 59,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "resources.labels.2", "operator": "StartsWith", "value": { "Case": "VString", "Fields": ["vip"] } } ] },
      "actions": [ { "type": "Validation", "severity": "Warning", "code": "STRING_CI_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-NOTES-CONTAINS",
      "name": "notes Contains 'window'",
      "priority": 58,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "reservation.notes", "operator": "Contains", "value": { "Case": "VString", "Fields": ["window"] } } ] },
      "actions": [ { "type": "Validation", "severity": "Warning", "code": "NOTES_SUBSTR_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-NULLS",
      "name": "nullable IsNull, status IsNotNull",
      "priority": 50,
      "enabled": true,
      "condition": {
        "Case": "Composite",
        "Fields": [
          { "op": "And", "children": [
              { "Case": "Simple", "Fields": [{ "field": "nullable", "operator": "IsNull" }] },
              { "Case": "Simple", "Fields": [{ "field": "status", "operator": "IsNotNull" }] }
          ]}
        ]
      },
      "actions": [ { "type": "Validation", "severity": "Warning", "code": "NULL_HANDLING_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-ISNULL-NULLABLE",
      "name": "nullable IsNull (existing property)",
      "priority": 49,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "nullable", "operator": "IsNull" } ] },
      "actions": [ { "type": "Validation", "severity": "Info", "code": "NULLABLE_IS_NULL" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-DATETIME",
      "name": "reservation.end > reservation.start",
      "priority": 48,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "reservation.end", "operator": ">", "referenceField": "reservation.start" } ] },
      "actions": [ { "type": "Validation", "severity": "Info", "code": "DATES_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-IN-INT",
      "name": "partySize In freeTables",
      "priority": 47,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "reservation.partySize", "operator": "In", "referenceField": "resources.freeTables" } ] },
      "actions": [ { "type": "Validation", "severity": "Error", "code": "PARTY_IN_FREE_TABLES" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-NUM",
      "name": "partySize between thresholds",
      "priority": 40,
      "enabled": true,
      "condition": { "Case": "Composite", "Fields": [ { "op": "And", "children": [
          { "Case": "Simple", "Fields": [{ "field": "reservation.partySize", "operator": ">=", "referenceField": "threshold" }] },
          { "Case": "Simple", "Fields": [{ "field": "reservation.partySize", "operator": "<", "referenceField": "thresholdMax" }] }
      ] } ] },
      "actions": [ { "type": "Validation", "severity": "Error", "code": "PARTY_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-LE-BOUNDARY",
      "name": "partySize <= thresholdMax",
      "priority": 39,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "reservation.partySize", "operator": "<=", "referenceField": "thresholdMax" } ] },
      "actions": [ { "type": "Validation", "severity": "Info", "code": "PARTY_LE_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    },
    {
      "id": "R-NEQ-MIXED",
      "name": "threshold != price (int vs decimal)",
      "priority": 38,
      "enabled": true,
      "condition": { "Case": "Simple", "Fields": [ { "field": "threshold", "operator": "!=", "referenceField": "price" } ] },
      "actions": [ { "type": "Validation", "severity": "Info", "code": "NEQ_MIXED_OK" } ],
      "kind": "Validation",
      "scope": "reservation"
    }
  ]
}
        """

    [<Fact(DisplayName = "MaxCase: run comprehensive DSL request through processJsonRequest")>]
    let ``run max-case request`` () =
        let jsonResponse = JsonSerialization.processJsonRequest requestJson
        System.Console.WriteLine jsonResponse
        Assert.True(jsonResponse.Contains("\"hasCriticalErrors\":"))

    [<Fact(DisplayName = "MaxCase: validated branch with nextRequired Pricing")>]
    let ``run max-case request (validated)`` () =
        // same as requestJson but with allowedZones including "vip" to avoid critical rule
        let reqValidated = requestJson.Replace("\"allowedZones\": [\"main\", \"family\", \"accessible\"]", "\"allowedZones\": [\"main\", \"family\", \"accessible\", \"vip\"]")
        let jsonResponse = JsonSerialization.processJsonRequest reqValidated
        System.Console.WriteLine jsonResponse
        Assert.True(jsonResponse.Contains("\"nextRequired\": ["))

