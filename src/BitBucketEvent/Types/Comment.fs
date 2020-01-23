//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open System
open Thoth.Json.Net


type Comment =
    { Id: int
      Version: int
      Text: NonNullString
      Author: User
      CreatedDate: Timestamp
      UpdatedDate: Timestamp }

    static member decoder: Decoder<Comment> =
        Decode.object <| fun get ->
            { Id = get.Required.Field _Id Decode.int
              Version = get.Required.Field _Version Decode.int
              Text = get.Required.Field _Text NonNullString.decoder
              Author = get.Required.Field _Author User.decoder
              CreatedDate = get.Required.Field _CreatedDate Timestamp.decoder
              UpdatedDate = get.Required.Field _UpdatedDate Timestamp.decoder }

    static member toJsonValue (x: Comment): JsonValue =
        Encode.object
            [ _Id, x.Id |> Encode.int
              _Version, x.Version |> Encode.int
              _Text, x.Text.asJsonValue
              _Author, x.Author.asJsonValue
              _CreatedDate, x.CreatedDate.asJsonValue
              _UpdatedDate, x.UpdatedDate.asJsonValue ]

    member inline self.asJsonValue = self |> Comment.toJsonValue
