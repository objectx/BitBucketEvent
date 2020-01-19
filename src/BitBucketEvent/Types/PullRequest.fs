//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.PullRequest

open BitBucketEvent.Types.Participant
open BitBucketEvent.Types.Reference
open System
open Thoth.Json.Net


[<Literal>]
let _Id = "id"

[<Literal>]
let _Version = "version"

[<Literal>]
let _Title = "title"

[<Literal>]
let _State = "state"

[<Literal>]
let _Open = "open"

[<Literal>]
let _Closed = "closed"

[<Literal>]
let _CreatedDate = "createdDate"

[<Literal>]
let _UpdatedDate = "updatedDate"

[<Literal>]
let _FromRef = "fromRef"

[<Literal>]
let _ToRef = "toRef"

[<Literal>]
let _Locked = "locked"

[<Literal>]
let _Author = "author"

[<Literal>]
let _Reviewers = "reviewers"

[<Literal>]
let _Participants = "participants"

[<Literal>]
let _Links = "links"

type PullRequest =
    { Id: int
      Version: int
      Title: string
      State: string
      Open: bool
      Closed: bool
      CreatedDate: System.DateTimeOffset
      UpdatedDate: System.DateTimeOffset
      FromRef: Reference.Reference
      ToRef: Reference.Reference
      Locked: bool
      Author: Participant.Participant
      Reviewers: Participant.Participant array
      Participants: Participant.Participant array }

let def: PullRequest =
    { Id = -1
      Version = -1
      Title = ""
      State = ""
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
          Title = get.Required.Field _Title Decode.string
          State = get.Required.Field _State Decode.string
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
    Encode.object
        [ _Id, Encode.int x.Id
          _Version, Encode.int x.Version
          _Title, Encode.string x.Title
          _State, Encode.string x.State
          _Open, Encode.bool x.Open
          _Closed, Encode.bool x.Closed
          _CreatedDate, Encode.int64 (x.CreatedDate.ToUnixTimeMilliseconds())
          _UpdatedDate, Encode.int64 (x.UpdatedDate.ToUnixTimeMilliseconds())
          _FromRef, Reference.toJsonValue x.FromRef
          _ToRef, Reference.toJsonValue x.ToRef
          _Locked, Encode.bool x.Locked
          _Author, Participant.toJsonValue x.Author
          _Reviewers, x.Reviewers |> Array.map Participant.toJsonValue |> Encode.array
          _Participants, x.Participants |> Array.map Participant.toJsonValue |> Encode.array ]
