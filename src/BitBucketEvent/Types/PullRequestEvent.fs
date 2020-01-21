//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.PullRequestEvent

open BitBucketEvent.Types
open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Participant
open BitBucketEvent.Types.PullRequest
open System
open Thoth.Json.Net
open Thoth.Json.Net


module Target =
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
              LatestCommit = get.Required.Field _LatestCommit CommitHash.decoder
              LatestChangeset = get.Required.Field _LatestChangeset CommitHash.decoder }

    let toJsonValue (x: Target): JsonValue =
        Encode.object
            [ _Id, x.Id |> Encode.string
              (_DisplayId, x.DisplayId |> Encode.string)
              (_Type, x.Type |> Encode.string)
              (_LatestCommit, x.LatestCommit |> CommitHash.toJsonValue)
              (_LatestChangeset, x.LatestChangeset |> CommitHash.toJsonValue) ]


type Common =
    { Date: DateTimeOffset
      Actor: User.User
      PullRequest: PullRequest.PullRequest }
    static member decoder: Decoder<Common> =
        Decode.map3 (fun date actor pr -> { Date = date; Actor = actor; PullRequest = pr })
            (Decode.field _Date Decode.datetimeOffset)
            (Decode.field _Actor User.decoder)
            (Decode.field _PullRequest PullRequest.decoder)

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

let decoder: Decoder<PullRequestEvent> =
    let decoder' (key: string): Decoder<PullRequestEvent> =
        match key with
        | _PR.Opened -> Decode.map (fun x -> Opened (x)) Common.decoder
        | _PR.Modified ->
            Decode.map4 (fun common prevTitle prevDesc prevTarget -> Modified (common, prevTitle, prevDesc, prevTarget))
                Common.decoder
                (Decode.field _PreviousTitle Decode.string)
                (Decode.field _PreviousDescription Decode.string)
                (Decode.field _PreviousTarget Target.decoder)
        | _PR.ReviewerUpdated ->
            Decode.map3 (fun common added removed -> ReviewersUpdated (common, added, removed))
                Common.decoder
                (Decode.field _AddedReviewers (Decode.array User.decoder))
                (Decode.field _AddedReviewers (Decode.array User.decoder))
        | _ -> Decode.fail (sprintf "unexpected event key: %s" key)
    (Decode.field _EventKey Decode.string)
        |> Decode.andThen decoder'

let toJsonValue x =
    match x with
    | Opened (common) ->
        Encode.object
            [ _EventKey, _PR.Opened |> Encode.string
              _Date, common.Date |> Encode.datetimeOffset
              _Actor, common.Actor |> User.toJsonValue
              _PullRequest, common.PullRequest |> PullRequest.toJsonValue ]
    | _ -> failwithf "unhandled type: %A" x
