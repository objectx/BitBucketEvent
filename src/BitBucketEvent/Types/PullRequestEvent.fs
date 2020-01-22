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
    | Approved of common: Common * participant: Participant.Participant * previousStatus: NonNullString.T
    | Unapproved of common: Common * participant: Participant.Participant * previousStatus: NonNullString.T
    | NeedsWork of common: Common * participant: Participant.Participant * previousStatus: NonNullString.T
    | Merged of common: Common
    | Declined of common: Common
    | Deleted of common: Common
    | CommentAdded of common: Common * comment: Comment.Comment * commentParentId: int option
    | CommentEdited of common: Common * comment: Comment.Comment * commentParentId: int option * previousComment: NonNullString.T
    | CommentDeleted of common: Common * comment: Comment.Comment * _CommentParentId: int option

let decoder: Decoder<PullRequestEvent> =
    let decoder' (key: string): Decoder<PullRequestEvent> =
        match key with
        | _PR.Opened -> Decode.map (fun x -> Opened(x)) Common.decoder
        | _PR.Modified ->
            Decode.map4 (fun common prevTitle prevDesc prevTarget -> Modified(common, prevTitle, prevDesc, prevTarget))
                Common.decoder (Decode.field _PreviousTitle NonNullString.decoder)
                (Decode.field _PreviousDescription NonNullString.decoder)
                (Decode.field _PreviousTarget Target.decoder)
        | _PR.ReviewerUpdated ->
            Decode.map3 (fun common added removed -> ReviewersUpdated(common, added, removed)) Common.decoder
                (Decode.field _AddedReviewers (Decode.array User.decoder))
                (Decode.field _RemovedReviewers (Decode.array User.decoder))
        | _PR.Approved ->
            Decode.map3 (fun common participant status -> Approved(common, participant, status)) Common.decoder
                (Decode.field _Participant Participant.decoder) (Decode.field _PreviousStatus NonNullString.decoder)
        | _PR.Unapproved ->
            Decode.map3 (fun common participant status -> Unapproved(common, participant, status)) Common.decoder
                (Decode.field _Participant Participant.decoder) (Decode.field _PreviousStatus NonNullString.decoder)
        | _PR.NeedsWork ->
            Decode.map3 (fun common participant status -> NeedsWork(common, participant, status)) Common.decoder
                (Decode.field _Participant Participant.decoder) (Decode.field _PreviousStatus NonNullString.decoder)
        | _PR.Merged -> Decode.map (fun x -> Merged(x)) Common.decoder
        | _PR.Declined -> Decode.map (fun x -> Declined(x)) Common.decoder
        | _PR.Deleted -> Decode.map (fun x -> Deleted(x)) Common.decoder
        | _PR.CommentAdded ->
            Decode.map3 (fun common comment parent -> CommentAdded(common, comment, parent)) Common.decoder
                (Decode.field _Comment Comment.decoder) (Decode.field _CommentParentId (Decode.option Decode.int))
        | _PR.CommentEdited ->
            Decode.map4 (fun common comment parent prevText -> CommentEdited(common, comment, parent, prevText))
                Common.decoder (Decode.field _Comment Comment.decoder)
                (Decode.field _CommentParentId (Decode.option Decode.int))
                (Decode.field _PreviousComment NonNullString.decoder)
        | _PR.CommentDeleted ->
            Decode.map3 (fun common comment parent -> CommentDeleted(common, comment, parent)) Common.decoder
                (Decode.field _Comment Comment.decoder) (Decode.field _CommentParentId (Decode.option Decode.int))
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
    | Approved(common, participant, status) ->
        preamble _PR.Approved common
        |> Seq.append
            [ _Participant, participant |> Participant.toJsonValue
              _PreviousStatus, status |> NonNullString.toJsonValue ]
        |> encode
    | Unapproved(common, participant, status) ->
        preamble _PR.Unapproved common
        |> Seq.append
            [ _Participant, participant |> Participant.toJsonValue
              _PreviousStatus, status |> NonNullString.toJsonValue ]
        |> encode
    | NeedsWork(common, participant, status) ->
        preamble _PR.NeedsWork common
        |> Seq.append
            [ _Participant, participant |> Participant.toJsonValue
              _PreviousStatus, status |> NonNullString.toJsonValue ]
        |> encode
    | Merged(common) ->
        preamble _PR.Merged common |> encode
    | Declined(common) ->
        preamble _PR.Declined common |> encode
    | Deleted(common) ->
        preamble _PR.Deleted common |> encode
    | CommentAdded(common, comment, parent) ->
        preamble _PR.CommentAdded common
        |> Seq.append
            [ _Comment, comment |> Comment.toJsonValue
              _CommentParentId, parent |> Encode.option Encode.int ]
        |> encode
    | CommentEdited(common, comment, parent, prevText) ->
        preamble _PR.CommentEdited common
        |> Seq.append
            [ _Comment, comment |> Comment.toJsonValue
              _PreviousComment, prevText |> NonNullString.toJsonValue
              _CommentParentId, parent |> Encode.option Encode.int ]
        |> encode
    | CommentDeleted(common, comment, parent) ->
        preamble _PR.CommentDeleted common
        |> Seq.append
            [ _Comment, comment |> Comment.toJsonValue
              _CommentParentId, parent |> Encode.option Encode.int ]
        |> encode
    | _ -> failwithf "unhandled type: %A" x
