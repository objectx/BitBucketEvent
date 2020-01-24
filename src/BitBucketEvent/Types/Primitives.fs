//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open System
open Thoth.Json.Net


[<Struct>]
type NonNullString = NonNullString of string

[<AutoOpen>]
module NonNullString =
    let empty = NonNullString ""

    let create (s: string): NonNullString =
        (if isNull s then ""
         else s)
        |> NonNullString

    let value (NonNullString s) = s
    let decoder: Decoder<NonNullString> = Decode.map create Decode.string
    let toJsonValue (NonNullString s) = s |> Encode.string

type NonNullString with
    member inline self.AsString = self |> value
    member inline self.AsJsonValue = self |> toJsonValue

[<Struct>]
type Timestamp = Timestamp of DateTimeOffset

[<AutoOpen>]
module Timestamp =
    let def = DateTimeOffset.FromUnixTimeMilliseconds 0L |> Timestamp
    let create (t: DateTimeOffset) = t |> Timestamp
    let value (Timestamp t) = t
    let toInt (Timestamp t) = t.ToUnixTimeMilliseconds()
    let decoder: Decoder<Timestamp> =
        Decode.map (DateTimeOffset.FromUnixTimeMilliseconds >> create) Decode.int64
    let toJsonValue (Timestamp t) =
        t.ToUnixTimeMilliseconds() |> Encode.int64

type Timestamp with
    member inline self.AsString = self |> value
    member inline self.AsInt = self |> toInt
    member inline self.AsJsonValue = self |> toJsonValue
