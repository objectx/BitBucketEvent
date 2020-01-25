//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Types
open BitBucketEvent.Literals
open System
open Thoth.Json.Net


type PullRequest =
    { Id: int
      Version: int
      Title: NonNullString
      Description: NonNullString option
      State: NonNullString
      Open: bool
      Closed: bool
      CreatedDate: Timestamp
      UpdatedDate: Timestamp
      FromRef: Reference
      ToRef: Reference
      Locked: bool
      Author: Participant
      Reviewers: Participant array
      Participants: Participant array }

module PullRequest =
    let def: PullRequest =
        { Id = -1
          Version = -1
          Title = NonNullString.empty
          Description = None
          State = NonNullString.empty
          Open = false
          Closed = false
          CreatedDate = Timestamp.def
          UpdatedDate = Timestamp.def
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
              Description = get.Optional.Field _Description NonNullString.decoder
              State = get.Required.Field _State NonNullString.decoder
              Open = get.Required.Field _Open Decode.bool
              Closed = get.Required.Field _Closed Decode.bool
              CreatedDate = get.Required.Field _CreatedDate Timestamp.decoder
              UpdatedDate = get.Required.Field _UpdatedDate Timestamp.decoder
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
              _Title, x.Title.AsJsonValue
              if x.Description.IsSome then (_Description, x.Description.Value.AsJsonValue)
              _State, x.State.AsJsonValue
              _Open, x.Open |> Encode.bool
              _Closed, x.Closed |> Encode.bool
              _CreatedDate, x.CreatedDate.AsJsonValue
              _UpdatedDate, x.UpdatedDate.AsJsonValue
              _FromRef, x.FromRef.AsJsonValue
              _ToRef, x.ToRef.AsJsonValue
              _Locked, x.Locked |> Encode.bool
              _Author, x.Author.AsJsonValue
              _Reviewers, x.Reviewers |> encodeParticipants
              _Participants, x.Participants |> encodeParticipants ]

type PullRequest with
    member inline self.AsJsonValue = self |> PullRequest.toJsonValue
