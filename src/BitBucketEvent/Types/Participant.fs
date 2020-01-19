//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Participant

open BitBucketEvent.Types.User
open System.Globalization
open Thoth.Json.Net

[<Literal>]
let _User = "user"

[<Literal>]
let _Role = "role"

[<Literal>]
let _Approved = "approved"

[<Literal>]
let _Status = "status"

[<Literal>]
let _LastReviewedCommit = "lastReviewedCommit"

type Participant =
    { User: User.User
      Role: string
      Approved: bool
      Status: string
      LastReviewedCommit: string option }

let def: Participant =
    { User = User.def
      Role = ""
      Approved = false
      Status = ""
      LastReviewedCommit = None }

let decoder: Decoder<Participant> =
    Decode.object <| fun get ->
        { User = get.Required.Field _User User.decoder
          Role = get.Required.Field _Role Decode.string
          Approved = get.Required.Field _Approved Decode.bool
          Status = get.Required.Field _Status Decode.string
          LastReviewedCommit = get.Optional.Field _LastReviewedCommit Decode.string }

let toJsonValue (x: Participant): JsonValue =
    let slots =
        [ _User, User.toJsonValue x.User
          _Role, Encode.string x.Role
          _Approved, Encode.bool x.Approved
          _Status, Encode.string x.Status
          if x.LastReviewedCommit.IsSome then _LastReviewedCommit, Encode.string x.LastReviewedCommit.Value ]
    Encode.object slots
