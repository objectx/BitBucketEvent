//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open Thoth.Json.Net


type User =
    { Name: NonNullString
      Email: NonNullString
      Id: int
      DisplayName: NonNullString
      Active: bool
      Slug: NonNullString
      Type: NonNullString }

module User =
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
            [ _Name, x.Name.AsJsonValue
              _EmailAddress, x.Email.AsJsonValue
              _Id, x.Id |> Encode.int
              _DisplayName, x.DisplayName.AsJsonValue
              _Active, x.Active |> Encode.bool
              _Slug, x.Slug.AsJsonValue
              _Type, x.Type.AsJsonValue ]

type User with
    member inline self.AsJsonValue = self |> User.toJsonValue
