//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net

type User =
    { Name: NonNullString
      Email: NonNullString
      Id: int
      DisplayName: NonNullString
      Active: bool
      Slug: NonNullString
      Type: NonNullString }

    static member def: User =
        { Name = NonNullString.empty
          Email = NonNullString.empty
          Id = -1
          DisplayName = NonNullString.empty
          Active = false
          Slug = NonNullString.empty
          Type = NonNullString.empty }

    static member decoder: Decoder<User> =
        Decode.object <| fun get ->
            { Name = get.Required.Field _Name NonNullString.decoder
              Email = get.Required.Field _EmailAddress NonNullString.decoder
              Id = get.Required.Field _Id Decode.int
              DisplayName = get.Required.Field _DisplayName NonNullString.decoder
              Active = get.Required.Field _Active Decode.bool
              Slug = get.Required.Field _Slug NonNullString.decoder
              Type = get.Required.Field _Type NonNullString.decoder }

    static member toJsonValue (x: User): JsonValue =
        Encode.object
            [ _Name, x.Name |> NonNullString.toJsonValue
              _EmailAddress, x.Email |> NonNullString.toJsonValue
              _Id, x.Id |> Encode.int
              _DisplayName, x.DisplayName |> NonNullString.toJsonValue
              _Active, x.Active |> Encode.bool
              _Slug, x.Slug |> NonNullString.toJsonValue
              _Type, x.Type |> NonNullString.toJsonValue ]

    member self.asJsonValue = self |> User.toJsonValue
