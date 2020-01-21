//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Participant

open BitBucketEvent.Types.CommitHash
open BitBucketEvent.Types.Literals
open Thoth.Json.Net


type Participant =
    { User: User.User
      Role: string
      Approved: bool
      Status: string
      LastReviewedCommit: CommitHash option }

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
          LastReviewedCommit = get.Optional.Field _LastReviewedCommit CommitHash.decoder }

let toJsonValue (x: Participant): JsonValue =
    let slots =
        [ _User, User.toJsonValue x.User
          _Role, Encode.string x.Role
          _Approved, Encode.bool x.Approved
          _Status, Encode.string x.Status
          if x.LastReviewedCommit.IsSome then _LastReviewedCommit, x.LastReviewedCommit.Value |> CommitHash.toJsonValue ]
    Encode.object slots
