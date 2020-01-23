//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Participant

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net


type Participant =
    { User: User.User
      Role: NonNullString
      Approved: bool
      Status: NonNullString
      LastReviewedCommit: CommitHash option }

let def: Participant =
    { User = User.def
      Role = NonNullString.empty
      Approved = false
      Status = NonNullString.empty
      LastReviewedCommit = None }

let decoder: Decoder<Participant> =
    Decode.object <| fun get ->
        { User = get.Required.Field _User User.decoder
          Role = get.Required.Field _Role NonNullString.decoder
          Approved = get.Required.Field _Approved Decode.bool
          Status = get.Required.Field _Status NonNullString.decoder
          LastReviewedCommit = get.Optional.Field _LastReviewedCommit CommitHash.decoder }

let toJsonValue (x: Participant): JsonValue =
    let slots =
        [ _User, x.User |> User.toJsonValue
          _Role, (x.Role |> NonNullString.toJsonValue)
          _Approved, x.Approved |> Encode.bool
          _Status, (x.Status |> NonNullString.toJsonValue)
          if x.LastReviewedCommit.IsSome then _LastReviewedCommit, x.LastReviewedCommit.Value |> CommitHash.toJsonValue ]
    Encode.object slots
