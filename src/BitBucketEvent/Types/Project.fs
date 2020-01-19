//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Project

open Thoth.Json.Net

[<Literal>]
let private _Key = "key"

[<Literal>]
let private _Id = "id"

[<Literal>]
let private _Name = "name"

[<Literal>]
let private _Public = "public"

[<Literal>]
let private _Type = "type"

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
        [ _Key, Encode.string x.Key
          _Id, Encode.int x.Id
          _Name, Encode.string x.Name
          _Public, Encode.bool x.Public
          _Type, Encode.string x.Type ]
