//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open Thoth.Json.Net


[<Struct>]
type Ownership =
    | Public of bool
    | Owned of user: User

type Project =
    { Key: NonNullString
      Id: int
      Name: NonNullString
      Type: NonNullString
      Owner: Ownership }

module Project =
    let def: Project =
        { Key = NonNullString.empty
          Id = -1
          Name = NonNullString.empty
          Type = NonNullString.empty
          Owner = Public false }

    let decoder: Decoder<Project> =
        Decode.object <| fun get ->
            match (get.Optional.Field _Public Decode.bool, get.Optional.Field _Owner User.decoder) with
            | (None, None) ->
                { Key = get.Required.Field _Key NonNullString.decoder
                  Id = get.Required.Field _Id Decode.int
                  Name = get.Required.Field _Name NonNullString.decoder
                  Owner = Public false
                  Type = get.Required.Field _Type NonNullString.decoder }
            | (Some (pub), None) ->
                { Key = get.Required.Field _Key NonNullString.decoder
                  Id = get.Required.Field _Id Decode.int
                  Name = get.Required.Field _Name NonNullString.decoder
                  Owner = Public pub
                  Type = get.Required.Field _Type NonNullString.decoder }
            | (None, Some(owner))
            | (Some (_), Some (owner)) ->
                { Key = get.Required.Field _Key NonNullString.decoder
                  Id = get.Required.Field _Id Decode.int
                  Name = get.Required.Field _Name NonNullString.decoder
                  Owner = Owned owner
                  Type = get.Required.Field _Type NonNullString.decoder }


    let toJsonValue (x: Project): JsonValue =
        match x.Owner with
        | Public f ->
            Encode.object
                [ _Key, x.Key.AsJsonValue
                  _Id, x.Id |> Encode.int
                  _Name, x.Name.AsJsonValue
                  _Public, f |> Encode.bool
                  _Type, x.Type.AsJsonValue ]
        | Owned owner ->
            Encode.object
                [ _Key, x.Key.AsJsonValue
                  _Id, x.Id |> Encode.int
                  _Name, x.Name.AsJsonValue
                  _Owner, owner.AsJsonValue
                  _Type, x.Type.AsJsonValue ]

type Project with
    member inline self.AsJsonValue = self |> Project.toJsonValue
