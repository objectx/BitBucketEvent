//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open System
open Thoth.Json.Net


type Comment =
    { Id: int
      Version: int
      Text: NonNullString
      Author: User
      CreatedDate: Timestamp
      UpdatedDate: Timestamp }

module Comment =
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
              _Text, x.Text.AsJsonValue
              _Author, x.Author.AsJsonValue
              _CreatedDate, x.CreatedDate.AsJsonValue
              _UpdatedDate, x.UpdatedDate.AsJsonValue ]

type Comment with
    member inline self.AsJsonValue = self |> Comment.toJsonValue
