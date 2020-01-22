//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.User

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net

type User =
    { Name: NonNullString.T
      Email: NonNullString.T
      Id: int
      DisplayName: NonNullString.T
      Active: bool
      Slug: NonNullString.T
      Type: NonNullString.T }

/// The default value
let def: User =
    { Name = NonNullString.empty
      Email = NonNullString.empty
      Id = -1
      DisplayName = NonNullString.empty
      Active = false
      Slug = NonNullString.empty
      Type = NonNullString.empty }

let decoder: Decoder<User> =
    Decode.object <| fun get ->
        { Name = get.Required.Field _Name NonNullString.decoder
          Email = get.Required.Field _EmailAddress NonNullString.decoder
          Id = get.Required.Field _Id Decode.int
          DisplayName = get.Required.Field _DisplayName NonNullString.decoder
          Active = get.Required.Field _Active Decode.bool
          Slug = get.Required.Field _Slug NonNullString.decoder
          Type = get.Required.Field _Type NonNullString.decoder }

let toJsonValue (x: User): JsonValue =
    Encode.object
        [ _Name, x.Name |> NonNullString.toJsonValue
          _EmailAddress, x.Email |> NonNullString.toJsonValue
          _Id, x.Id |> Encode.int
          _DisplayName, x.DisplayName |> NonNullString.toJsonValue
          _Active, x.Active |> Encode.bool
          _Slug, x.Slug |> NonNullString.toJsonValue
          _Type, x.Type |> NonNullString.toJsonValue ]
