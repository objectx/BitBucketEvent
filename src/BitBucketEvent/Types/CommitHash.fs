//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open System
open System.Text
open Thoth.Json.Net


[<Struct>]
type CommitHash = CommitHash of byte array

module CommitHash =
    let def = CommitHash [||]

    let create (x: byte array): CommitHash =
        x |> CommitHash

    let value (CommitHash h): byte array =
        h

    let toString (CommitHash h): string =
        let sb = StringBuilder(2 * h.Length)
        for v in h do
            sb.AppendFormat("{0:x2}", v) |> ignore
        sb.ToString()

    let fromString (s: string): CommitHash =
        let toHex (x: Char) =
            match x with
            | '0' -> 0uy
            | '1' -> 1uy
            | '2' -> 2uy
            | '3' -> 3uy
            | '4' -> 4uy
            | '5' -> 5uy
            | '6' -> 6uy
            | '7' -> 7uy
            | '8' -> 8uy
            | '9' -> 9uy
            | 'a'
            | 'A' -> 10uy
            | 'b'
            | 'B' -> 11uy
            | 'c'
            | 'C' -> 12uy
            | 'd'
            | 'D' -> 13uy
            | 'e'
            | 'E' -> 14uy
            | 'f'
            | 'F' -> 15uy
            | _ -> failwithf "'%c' is not a hex digit" x

        let combine u l: byte =
            (u <<< 4) ||| (l <<< 0)
        let len = s.Length
        if (len % 2) <> 0 then failwithf "odd length (%d)" len
        let result = ResizeArray<byte>(s.Length / 2)
        for i in 0 .. 2 .. (s.Length - 2) do
            combine (s.[i + 0] |> toHex) (s.[i + 1] |> toHex) |> result.Add
        result.ToArray() |> CommitHash

    let decoder: Decoder<CommitHash> = Decode.map fromString Decode.string

    let toJsonValue (h: CommitHash): JsonValue =
        h
        |> toString
        |> Encode.string

type CommitHash with
    member inline self.AsString = self |> CommitHash.toString
    member inline self.AsJsonValue = self |> CommitHash.toJsonValue
