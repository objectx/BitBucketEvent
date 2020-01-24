//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open Thoth.Json.Net


[<Struct>]
type Ownership =
    | Public
    | Owned of User

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
            | Some(owner) ->
                { Key = get.Required.Field _Key NonNullString.decoder
                  Id = get.Required.Field _Id Decode.int
                  Name = get.Required.Field _Name NonNullString.decoder
                  Owner = Owned owner
                  Type = get.Required.Field _Type NonNullString.decoder }


    let toJsonValue (x: Project): JsonValue =
        match x.Owner with
        | Public ->
            Encode.object
                [ _Key, x.Key.AsJsonValue
                  _Id, x.Id |> Encode.int
                  _Name, x.Name.AsJsonValue
                  _Public, true |> Encode.bool
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
