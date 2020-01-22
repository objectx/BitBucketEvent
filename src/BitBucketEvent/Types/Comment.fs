//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Comment

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open System
open Thoth.Json.Net


type Comment =
    { Id: int
      Version: int
      Text: NonNullString.T
      Author: User.User
      CreatedDate: Timestamp.T
      UpdatedDate: Timestamp.T }

let decoder: Decoder<Comment> =
    Decode.object <| fun get ->
        { Id = get.Required.Field _Id Decode.int
          Version = get.Required.Field _Version Decode.int
          Text = get.Required.Field _Text NonNullString.decoder
          Author = get.Required.Field _Author User.decoder
          CreatedDate = get.Required.Field _CreatedDate Timestamp.decoder
          UpdatedDate = get.Required.Field _UpdatedDate Timestamp.decoder }

let toJsonValue (x: Comment): JsonValue =
    Encode.object
        [ _Id, x.Id |> Encode.int
          _Version, x.Version |> Encode.int
          _Text, x.Text |> NonNullString.toJsonValue
          _Author, x.Author |> User.toJsonValue
          _CreatedDate, x.CreatedDate |> Timestamp.toJsonValue
          _UpdatedDate, x.UpdatedDate |> Timestamp.toJsonValue ]
