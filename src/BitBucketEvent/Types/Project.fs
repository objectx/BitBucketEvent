//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Project

open BitBucketEvent.Types.Literals
open Thoth.Json.Net

type Project =
    { Key: string
      Id: int
      Name: string
      Public: bool
      Type: string }

let def: Project =
    { Key = ""
      Id = -1
      Name = ""
      Public = false
      Type = "" }

let decoder: Decoder<Project> =
    Decode.object <| fun get ->
        { Key = get.Required.Field _Key Decode.string
          Id = get.Required.Field _Id Decode.int
          Name = get.Required.Field _Name Decode.string
          Public = get.Required.Field _Public Decode.bool
          Type = get.Required.Field _Type Decode.string }

let toJsonValue (x: Project): JsonValue =
    Encode.object
        [ _Key, x.Key |> Encode.string
          _Id, x.Id |> Encode.int
          _Name, x.Name |> Encode.string
          _Public, x.Public |> Encode.bool
          _Type, x.Type |> Encode.string ]
