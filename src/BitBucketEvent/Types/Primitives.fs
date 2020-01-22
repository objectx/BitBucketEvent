//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Primitives

open System
open Thoth.Json.Net

[<RequireQualifiedAccess>]
module NonNullString =
    type [<Struct>] T = NonNullString of string

    let empty = NonNullString ""

    let create (s: string) =
        let s' = if isNull s then "" else s
        NonNullString (s')

    let value (NonNullString s) =
        s

    let decoder: Decoder<T> =
        Decode.map create Decode.string

    let toJsonValue (NonNullString s) =
        s |> Encode.string

[<RequireQualifiedAccess>]
module Timestamp =
    type [<Struct>] T = Timestamp of DateTimeOffset
    let def = DateTimeOffset.FromUnixTimeSeconds 0L |> Timestamp

    let create (t: DateTimeOffset) =
        Timestamp t

    let value (Timestamp t) =
        t

    let decoder: Decoder<T> =
        Decode.map (DateTimeOffset.FromUnixTimeMilliseconds >> create) Decode.int64

    let toJsonValue (Timestamp t) =
        t.ToUnixTimeMilliseconds () |> Encode.int64
