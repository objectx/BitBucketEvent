//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Primitives

open System
open Thoth.Json.Net

[<Struct>]
type NonNullString =
    | NonNullString of string
    static member empty = NonNullString ""

    static member create (s: string): NonNullString =
        (if isNull s then ""
         else s)
        |> NonNullString

    static member value (NonNullString s) = s
    static member decoder: Decoder<NonNullString> = Decode.map NonNullString.create Decode.string
    static member toJsonValue (NonNullString s) = s |> Encode.string

    member inline self.asString = self |> NonNullString.value
    override self.ToString() = self.asString
    member inline self.asJsonValue = self |> NonNullString.toJsonValue

[<Struct>]
type Timestamp =
    | Timestamp of DateTimeOffset
    static member def = DateTimeOffset.FromUnixTimeMilliseconds 0L |> Timestamp
    static member create (t: DateTimeOffset) = t |> Timestamp
    static member value (Timestamp t) = t
    static member toInt (Timestamp t) = t.ToUnixTimeMilliseconds()
    static member decoder: Decoder<Timestamp> =
        Decode.map (DateTimeOffset.FromUnixTimeMilliseconds >> Timestamp.create) Decode.int64
    static member toJsonValue (Timestamp t) =
        t.ToUnixTimeMilliseconds() |> Encode.int64
    member inline self.asDateTimeOffset = self |> Timestamp.value
    member inline self.asJsonValue = self |> Timestamp.toJsonValue
    member inline self.asInt = self |> Timestamp.toInt
