//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.PullRequest

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open System
open Thoth.Json.Net


type PullRequest =
    { Id: int
      Version: int
      Title: NonNullString
      State: NonNullString
      Open: bool
      Closed: bool
      CreatedDate: System.DateTimeOffset
      UpdatedDate: System.DateTimeOffset
      FromRef: Reference
      ToRef: Reference
      Locked: bool
      Author: Participant.Participant
      Reviewers: Participant.Participant array
      Participants: Participant.Participant array }

let def: PullRequest =
    { Id = -1
      Version = -1
      Title = NonNullString.empty
      State = NonNullString.empty
      Open = false
      Closed = false
      CreatedDate = System.DateTimeOffset.FromUnixTimeMilliseconds(0L)
      UpdatedDate = System.DateTimeOffset.FromUnixTimeMilliseconds(0L)
      FromRef = Reference.def
      ToRef = Reference.def
      Locked = false
      Author = Participant.def
      Reviewers = [||]
      Participants = [||] }

let decoder: Decoder<PullRequest> =
    Decode.object <| fun get ->
        { Id = get.Required.Field _Id Decode.int
          Version = get.Required.Field _Version Decode.int
          Title = get.Required.Field _Title NonNullString.decoder
          State = get.Required.Field _State NonNullString.decoder
          Open = get.Required.Field _Open Decode.bool
          Closed = get.Required.Field _Closed Decode.bool
          CreatedDate = get.Required.Field _CreatedDate Decode.int64 |> DateTimeOffset.FromUnixTimeMilliseconds
          UpdatedDate = get.Required.Field _UpdatedDate Decode.int64 |> DateTimeOffset.FromUnixTimeMilliseconds
          FromRef = get.Required.Field _FromRef Reference.decoder
          ToRef = get.Required.Field _ToRef Reference.decoder
          Locked = get.Required.Field _Locked Decode.bool
          Author = get.Required.Field _Author Participant.decoder
          Reviewers = get.Required.Field _Reviewers (Decode.array Participant.decoder)
          Participants = get.Required.Field _Participants (Decode.array Participant.decoder) }

let toJsonValue (x: PullRequest): JsonValue =
    let encodeParticipants = Array.map Participant.toJsonValue >> Encode.array
    Encode.object
        [ _Id, x.Id |> Encode.int
          _Version, x.Version |> Encode.int
          _Title, (x.Title |> NonNullString.toJsonValue)
          _State, (x.State |> NonNullString.toJsonValue)
          _Open, x.Open |> Encode.bool
          _Closed, x.Closed |> Encode.bool
          _CreatedDate, x.CreatedDate.ToUnixTimeMilliseconds() |> Encode.int64
          _UpdatedDate, x.UpdatedDate.ToUnixTimeMilliseconds() |> Encode.int64
          _FromRef, x.FromRef |> Reference.toJsonValue
          _ToRef, x.ToRef |> Reference.toJsonValue
          _Locked, x.Locked |> Encode.bool
          _Author, x.Author |> Participant.toJsonValue
          _Reviewers, x.Reviewers |> encodeParticipants
          _Participants, x.Participants |> encodeParticipants ]
