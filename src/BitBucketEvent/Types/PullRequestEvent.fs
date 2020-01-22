//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.PullRequestEvent

open BitBucketEvent.Types
open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Participant
open BitBucketEvent.Types.Primitives
open BitBucketEvent.Types.PullRequest
open System
open Thoth.Json.Net


module Target =
    type Target =
        { Id: NonNullString.T
          DisplayId: NonNullString.T
          Type: NonNullString.T
          LatestCommit: CommitHash.CommitHash
          LatestChangeset: CommitHash.CommitHash }

    let decoder: Decoder<Target> =
        Decode.object <| fun get ->
            { Id = get.Required.Field _Id NonNullString.decoder
              DisplayId = get.Required.Field _DisplayId NonNullString.decoder
              Type = get.Required.Field _Type NonNullString.decoder
              LatestCommit = get.Required.Field _LatestCommit CommitHash.decoder
              LatestChangeset = get.Required.Field _LatestChangeset CommitHash.decoder }

    let toJsonValue (x: Target): JsonValue =
        Encode.object
            [ _Id, x.Id |> NonNullString.toJsonValue
              (_DisplayId, x.DisplayId |> NonNullString.toJsonValue)
              (_Type, x.Type |> NonNullString.toJsonValue)
              (_LatestCommit, x.LatestCommit |> CommitHash.toJsonValue)
              (_LatestChangeset, x.LatestChangeset |> CommitHash.toJsonValue) ]


type Common =
    { Date: DateTimeOffset
      Actor: User.User
      PullRequest: PullRequest.PullRequest }
    static member decoder: Decoder<Common> =
        Decode.map3 (fun date actor pr ->
            { Date = date
              Actor = actor
              PullRequest = pr }) (Decode.field _Date Decode.datetimeOffset) (Decode.field _Actor User.decoder)
            (Decode.field _PullRequest PullRequest.decoder)

type PullRequestEvent =
    | Opened of common: Common
    | Modified of common: Common * previousTitle: NonNullString.T * previousDescription: NonNullString.T * previousTarget: Target.Target
    | ReviewersUpdated of common: Common * addedReviewers: User.User array * removedReviewers: User.User array
//    | Approved of common: Common * participant: Participant.Participant * previousStatus: string
//    | Unapproved of common: Common * participant: Participant.Participant * previousStatus: string
//    | NeedsWork of common: Common * participant: Participant.Participant * previousStatus: string
//    | Merged of common: Common
//    | Declined of common: Common
//    | Deleted of common: Common
//    | CommentAdded of common: Common * comment: Comment.Comment * commentParentId: int option
//    | CommentEdited of common: Common * comment: Comment.Comment * commentParentId: int option * previousComment: string
//    | CommentDeleted of common: Common * comment: Comment.Comment

let decoder: Decoder<PullRequestEvent> =
    let decoder' (key: string): Decoder<PullRequestEvent> =
        match key with
        | _PR.Opened -> Decode.map (fun x -> Opened(x)) Common.decoder
        | _PR.Modified ->
            Decode.map4 (fun common prevTitle prevDesc prevTarget -> Modified(common, prevTitle, prevDesc, prevTarget))
                Common.decoder (Decode.field _PreviousTitle NonNullString.decoder)
                (Decode.field _PreviousDescription NonNullString.decoder) (Decode.field _PreviousTarget Target.decoder)
        | _PR.ReviewerUpdated ->
            Decode.map3 (fun common added removed -> ReviewersUpdated(common, added, removed)) Common.decoder
                (Decode.field _AddedReviewers (Decode.array User.decoder))
                (Decode.field _RemovedReviewers (Decode.array User.decoder))
        | _ -> Decode.fail (sprintf "unexpected event key: %s" key)
    (Decode.field _EventKey Decode.string) |> Decode.andThen decoder'

let toJsonValue x =
    let preamble (typ: string) (common: Common): seq<string * JsonValue> =
        seq {
            yield (_EventKey, typ |> Encode.string)
            yield (_Date, common.Date |> Encode.datetimeOffset)
            yield (_Actor, common.Actor |> User.toJsonValue)
            yield (_PullRequest, common.PullRequest |> PullRequest.toJsonValue)
        }

    let encode (x: seq<string * JsonValue>): JsonValue =
        x
        |> Seq.toList
        |> Encode.object

    match x with
    | Opened(common) ->
        preamble _PR.Opened common |> encode
    | Modified(common, prevTitle, prevDesc, prevTarget) ->
        preamble _PR.Modified common
        |> Seq.append
            [ _PreviousTitle, prevTitle |> NonNullString.toJsonValue
              _PreviousDescription, prevDesc |> NonNullString.toJsonValue
              _PreviousTarget, prevTarget |> Target.toJsonValue ]
        |> encode
    | ReviewersUpdated(common, added, removed) ->
        preamble _PR.ReviewerUpdated common
        |> Seq.append
            [ _AddedReviewers,
              added
              |> Array.map User.toJsonValue
              |> Encode.array
              _RemovedReviewers,
              removed
              |> Array.map User.toJsonValue
              |> Encode.array ]
        |> encode
    | _ -> failwithf "unhandled type: %A" x
