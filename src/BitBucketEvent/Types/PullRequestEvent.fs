//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.PullRequestEvent

open BitBucketEvent.Types
open System
open Thoth.Json.Net

[<Literal>]
let _EventKey = "eventKey"

[<Literal>]
let _Date = "date"

[<Literal>]
let _Actor = "actor"

[<Literal>]
let _PullRequest = "pullRequest"

[<Literal>]
let _PreviousTitle = "previousTitle"

[<Literal>]
let _PreviousDescription = "previousDescription"

[<Literal>]
let _PreviousTarget = "previousTarget"

[<Literal>]
let _AddedReviewers = "addedReviewers"

[<Literal>]
let _RemovedReviewers = "removedReviewers"

[<Literal>]
let _Participant = "participant"

[<Literal>]
let _PreviousStatus = "previousStatus"

module Target =
    [<Literal>]
    let _Id = "id"

    [<Literal>]
    let _DisplayId = "displayId"

    [<Literal>]
    let _Type = "type"

    [<Literal>]
    let _LatestCommit = "latestCommit"

    [<Literal>]
    let _LatestChangeset = "latestChangeset"

    type Target =
        { Id: string
          DisplayId: string
          Type: string
          LatestCommit: CommitHash.CommitHash
          LatestChangeset: CommitHash.CommitHash }

    let decoder: Decoder<Target> =
        Decode.object <| fun get ->
            { Id = get.Required.Field _Id Decode.string
              DisplayId = get.Required.Field _DisplayId Decode.string
              Type = get.Required.Field _Type Decode.string
              LatestCommit = get.Required.Field _LatestCommit Decode.string |> CommitHash.fromString
              LatestChangeset = get.Required.Field _LatestChangeset Decode.string |> CommitHash.fromString }

    let toJsonValue (x: Target): JsonValue =
        Encode.object
            [ _Id, x.Id |> Encode.string
              (_DisplayId, x.DisplayId |> Encode.string)
              (_Type, x.Type |> Encode.string)
              (_LatestCommit,
               x.LatestCommit
               |> CommitHash.toString
               |> Encode.string)
              (_LatestChangeset,
               x.LatestChangeset
               |> CommitHash.toString
               |> Encode.string) ]

type Common =
    { Date: DateTimeOffset
      Actor: Participant.Participant
      PullRequest: PullRequest.PullRequest }

type PullRequestEvent =
    | Opened of common: Common
    | Modified of common: Common * previousTitle: string * previousDescription: string * previousTarget: Target.Target
    | ReviewersUpdated of common: Common * addedReviewers: User.User array * removedReviewers: User.User array
    | Approved of common: Common * participant: Participant.Participant * previousStatus: string
    | Unapproved of common: Common * participant: Participant.Participant * previousStatus: string
    | NeedsWork of common: Common * participant: Participant.Participant * previousStatus: string
    | Merged of common: Common
    | Declined of common: Common
    | Deleted of common: Common
    | CommentAdded of common: Common * comment: Comment.Comment * commentParentId: int option
    | CommentEdited of common: Common * comment: Comment.Comment * commentParentId: int option * previousComment: string
    | CommentDeleted of common: Common * comment: Comment.Comment

