//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open BitBucketEvent.Types
open System
open Thoth.Json.Net


type EventCommon =
    { Date: DateTimeOffset
      Actor: User
      PullRequest: PullRequest }

type Event =
    | Opened of common: EventCommon
    | Modified of common: EventCommon * previousTitle: NonNullString * previousDescription: NonNullString * previousTarget: Target
    | ReviewersUpdated of common: EventCommon * addedReviewers: User array * removedReviewers: User array
    | Approved of common: EventCommon * participant: Participant * previousStatus: NonNullString
    | Unapproved of common: EventCommon * participant: Participant * previousStatus: NonNullString
    | NeedsWork of common: EventCommon * participant: Participant * previousStatus: NonNullString
    | Merged of common: EventCommon
    | Declined of common: EventCommon
    | Deleted of common: EventCommon
    | CommentAdded of common: EventCommon * comment: Comment * commentParentId: int option
    | CommentEdited of common: EventCommon * comment: Comment * commentParentId: int option * previousComment: NonNullString
    | CommentDeleted of common: EventCommon * comment: Comment * _CommentParentId: int option

module Event =
    let private commonDecoder: Decoder<EventCommon> =
        Decode.map3 (fun date actor pr ->
            { Date = date
              Actor = actor
              PullRequest = pr }) (Decode.field _Date Decode.datetimeOffset) (Decode.field _Actor User.decoder)
            (Decode.field _PullRequest PullRequest.decoder)

    let decoder: Decoder<Event> =
        let decoder' (key: string): Decoder<Event> =
            match key with
            | _PR.Opened -> Decode.map (fun x -> Opened(x)) commonDecoder
            | _PR.Modified ->
                Decode.map4
                    (fun common prevTitle prevDesc prevTarget -> Modified(common, prevTitle, prevDesc, prevTarget))
                    commonDecoder (Decode.field _PreviousTitle NonNullString.decoder)
                    (Decode.field _PreviousDescription NonNullString.decoder)
                    (Decode.field _PreviousTarget Target.decoder)
            | _PR.ReviewerUpdated ->
                Decode.map3 (fun common added removed -> ReviewersUpdated(common, added, removed)) commonDecoder
                    (Decode.field _AddedReviewers (Decode.array User.decoder))
                    (Decode.field _RemovedReviewers (Decode.array User.decoder))
            | _PR.Approved ->
                Decode.map3 (fun common participant status -> Approved(common, participant, status)) commonDecoder
                    (Decode.field _Participant Participant.decoder)
                    (Decode.field _PreviousStatus NonNullString.decoder)
            | _PR.Unapproved ->
                Decode.map3 (fun common participant status -> Unapproved(common, participant, status)) commonDecoder
                    (Decode.field _Participant Participant.decoder)
                    (Decode.field _PreviousStatus NonNullString.decoder)
            | _PR.NeedsWork ->
                Decode.map3 (fun common participant status -> NeedsWork(common, participant, status)) (commonDecoder)
                    (Decode.field _Participant Participant.decoder)
                    (Decode.field _PreviousStatus NonNullString.decoder)
            | _PR.Merged -> Decode.map (fun x -> Merged(x)) commonDecoder
            | _PR.Declined -> Decode.map (fun x -> Declined(x)) commonDecoder
            | _PR.Deleted -> Decode.map (fun x -> Deleted(x)) commonDecoder
            | _PR.CommentAdded ->
                Decode.map3 (fun common comment parent -> CommentAdded(common, comment, parent)) (commonDecoder)
                    (Decode.field _Comment Comment.decoder) (Decode.optional _CommentParentId Decode.int)
            | _PR.CommentEdited ->
                Decode.map4 (fun common comment parent prevText -> CommentEdited(common, comment, parent, prevText))
                    (commonDecoder) (Decode.field _Comment Comment.decoder)
                    (Decode.optional _CommentParentId Decode.int)
                    (Decode.field _PreviousComment NonNullString.decoder)
            | _PR.CommentDeleted ->
                Decode.map3 (fun common comment parent -> CommentDeleted(common, comment, parent)) (commonDecoder)
                    (Decode.field _Comment Comment.decoder) (Decode.optional _CommentParentId Decode.int)
            | _ -> Decode.fail (sprintf "unexpected event key: %s" key)
        (Decode.field _EventKey Decode.string) |> Decode.andThen decoder'

    let toJsonValue (x: Event): JsonValue =
        let preamble (typ: string) (common: EventCommon): seq<string * JsonValue> =
            seq {
                yield (_EventKey, typ |> Encode.string)
                yield (_Date, common.Date |> Encode.datetimeOffset)
                yield (_Actor, (common.Actor.AsJsonValue))
                yield (_PullRequest, common.PullRequest.AsJsonValue)
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
                [ _PreviousTitle, prevTitle.AsJsonValue
                  _PreviousDescription, prevDesc.AsJsonValue
                  _PreviousTarget, prevTarget.AsJsonValue ]
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
                [ _Participant, participant.AsJsonValue
                  _PreviousStatus, status.AsJsonValue ]
            |> encode
        | Unapproved(common, participant, status) ->
            preamble _PR.Unapproved common
            |> Seq.append
                [ _Participant, participant.AsJsonValue
                  _PreviousStatus, status.AsJsonValue ]
            |> encode
        | NeedsWork(common, participant, status) ->
            preamble _PR.NeedsWork common
            |> Seq.append
                [ _Participant, participant.AsJsonValue
                  _PreviousStatus, status.AsJsonValue ]
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
                [ _Comment, comment.AsJsonValue
                  if parent.IsSome then (_CommentParentId, parent.Value |> Encode.int) ]
            |> encode
        | CommentEdited(common, comment, parent, prevText) ->
            preamble _PR.CommentEdited common
            |> Seq.append
                [ _Comment, comment.AsJsonValue
                  _PreviousComment, prevText.AsJsonValue
                  if parent.IsSome then (_CommentParentId, parent.Value |> Encode.int) ]
            |> encode
        | CommentDeleted(common, comment, parent) ->
            preamble _PR.CommentDeleted common
            |> Seq.append
                [ _Comment, comment.AsJsonValue
                  if parent.IsSome then (_CommentParentId, parent.Value |> Encode.int) ]
            |> encode

type Event with
    member inline self.AsJsonValue = self |> Event.toJsonValue
