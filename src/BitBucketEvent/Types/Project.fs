//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Project

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net

type Ownership =
    | Public
    | Owned of User.User

type Project =
    { Key: NonNullString
      Id: int
      Name: NonNullString
      Type: NonNullString
      Owner: Ownership }

let def: Project =
    { Key = NonNullString.empty
      Id = -1
      Name = NonNullString.empty
      Type = NonNullString.empty
      Owner = Public }

let decoder: Decoder<Project> =
    Decode.object <| fun get ->
        match get.Optional.Field _Owner User.decoder with
        | None ->
            { Key = get.Required.Field _Key NonNullString.decoder
              Id = get.Required.Field _Id Decode.int
              Name = get.Required.Field _Name NonNullString.decoder
              Owner = Public
              Type = get.Required.Field _Type NonNullString.decoder }
        | Some (owner) ->
            { Key = get.Required.Field _Key NonNullString.decoder
              Id = get.Required.Field _Id Decode.int
              Name = get.Required.Field _Name NonNullString.decoder
              Owner = Owned owner
              Type = get.Required.Field _Type NonNullString.decoder }

let toJsonValue (x: Project): JsonValue =
    match x.Owner with
    | Public ->
        Encode.object
            [ _Key, x.Key |> NonNullString.toJsonValue
              _Id, x.Id |> Encode.int
              _Name, x.Name |> NonNullString.toJsonValue
              _Public, true |> Encode.bool
              _Type, x.Type |> NonNullString.toJsonValue ]
    | Owned owner ->
        Encode.object
            [ _Key, x.Key |> NonNullString.toJsonValue
              _Id, x.Id |> Encode.int
              _Name, x.Name |> NonNullString.toJsonValue
              _Owner, owner |> User.toJsonValue
              _Type, x.Type |> NonNullString.toJsonValue ]
