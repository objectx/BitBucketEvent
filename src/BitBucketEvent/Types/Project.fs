//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Project

open BitBucketEvent.Types.Literals
open Thoth.Json.Net

type Ownership =
    | Public
    | Owned of User.User

type Project =
    { Key: string
      Id: int
      Name: string
      Type: string
      Owner: Ownership }

let def: Project =
    { Key = ""
      Id = -1
      Name = ""
      Type = ""
      Owner = Public }

let decoder: Decoder<Project> =
    Decode.object <| fun get ->
        match get.Optional.Field _Owner User.decoder with
        | None ->
            { Key = get.Required.Field _Key Decode.string
              Id = get.Required.Field _Id Decode.int
              Name = get.Required.Field _Name Decode.string
              Owner = Public
              Type = get.Required.Field _Type Decode.string }
        | Some (owner) ->
            { Key = get.Required.Field _Key Decode.string
              Id = get.Required.Field _Id Decode.int
              Name = get.Required.Field _Name Decode.string
              Owner = Owned owner
              Type = get.Required.Field _Type Decode.string }

let toJsonValue (x: Project): JsonValue =
    match x.Owner with
    | Public ->
        Encode.object
            [ _Key, x.Key |> Encode.string
              _Id, x.Id |> Encode.int
              _Name, x.Name |> Encode.string
              _Public, true |> Encode.bool
              _Type, x.Type |> Encode.string ]
    | Owned owner ->
        Encode.object
            [ _Key, x.Key |> Encode.string
              _Id, x.Id |> Encode.int
              _Name, x.Name |> Encode.string
              _Owner, owner |> User.toJsonValue
              _Type, x.Type |> Encode.string ]
