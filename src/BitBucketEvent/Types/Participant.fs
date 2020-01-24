//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open Thoth.Json.Net


type Participant =
    { User: User
      Role: NonNullString
      Approved: bool
      Status: NonNullString
      LastReviewedCommit: CommitHash option }

module Participant =
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
        Encode.object
            [ _User, x.User.AsJsonValue
              _Role, x.Role.AsJsonValue
              _Approved, x.Approved |> Encode.bool
              _Status, x.Status.AsJsonValue
              if x.LastReviewedCommit.IsSome then _LastReviewedCommit, x.LastReviewedCommit.Value.AsJsonValue ]

type Participant with
    member inline self.AsJsonValue = self |> Participant.toJsonValue
