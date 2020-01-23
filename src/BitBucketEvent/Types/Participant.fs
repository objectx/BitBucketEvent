//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net


type Participant =
    { User: User
      Role: NonNullString
      Approved: bool
      Status: NonNullString
      LastReviewedCommit: CommitHash option }

    static member def: Participant =
        { User = User.def
          Role = NonNullString.empty
          Approved = false
          Status = NonNullString.empty
          LastReviewedCommit = None }

    static member decoder: Decoder<Participant> =
        Decode.object <| fun get ->
            { User = get.Required.Field _User User.decoder
              Role = get.Required.Field _Role NonNullString.decoder
              Approved = get.Required.Field _Approved Decode.bool
              Status = get.Required.Field _Status NonNullString.decoder
              LastReviewedCommit = get.Optional.Field _LastReviewedCommit CommitHash.decoder }

    static member toJsonValue (x: Participant): JsonValue =
        Encode.object
            [ _User, x.User.asJsonValue
              _Role, x.Role.asJsonValue
              _Approved, x.Approved |> Encode.bool
              _Status, x.Status.asJsonValue
              if x.LastReviewedCommit.IsSome then _LastReviewedCommit, x.LastReviewedCommit.Value.asJsonValue ]

    member inline self.asJsonValue = self |> Participant.toJsonValue
