//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Comment

open BitBucketEvent.Types.Literals
open System
open Thoth.Json.Net


type Comment =
    { Id: int
      Version: int
      Text: string
      Author: User.User
      CreatedDate: System.DateTimeOffset
      UpdatedDate: System.DateTimeOffset }

let decoder: Decoder<Comment> =
    Decode.object <| fun get ->
        { Id = get.Required.Field _Id Decode.int
          Version = get.Required.Field _Version Decode.int
          Text = get.Required.Field _Text Decode.string
          Author = get.Required.Field _Author User.decoder
          CreatedDate = get.Required.Field _CreatedDate Decode.int64 |> DateTimeOffset.FromUnixTimeMilliseconds
          UpdatedDate = get.Required.Field _UpdatedDate Decode.int64 |> DateTimeOffset.FromUnixTimeMilliseconds }

let toJsonValue (x: Comment): JsonValue =
    Encode.object
        [ _Id, x.Id |> Encode.int
          _Version, x.Version |> Encode.int
          _Text, x.Text |> Encode.string
          _Author, x.Author |> User.toJsonValue
          _CreatedDate, x.CreatedDate.ToUnixTimeMilliseconds() |> Encode.int64
          _UpdatedDate, x.UpdatedDate.ToUnixTimeMilliseconds() |> Encode.int64 ]
