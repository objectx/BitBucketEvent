//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module internal BitBucketEvent.Types.Literals


[<Literal>]
let _Name = "name"

[<Literal>]
let _EmailAddress = "emailAddress"

[<Literal>]
let _Id = "id"

[<Literal>]
let _DisplayName = "displayName"

[<Literal>]
let _Active = "active"

[<Literal>]
let _Slug = "slug"

[<Literal>]
let _Type = "type"

[<Literal>]
let _Key = "key"

[<Literal>]
let _Public = "public"

[<Literal>]
let _Owner = "owner"

[<Literal>]
let _ScmId = "scmId"

[<Literal>]
let _State = "state"

[<Literal>]
let _StatusMessage = "statusMessage"

[<Literal>]
let _Forkable = "forkable"

[<Literal>]
let _Project = "project"

[<Literal>]
let _DisplayId = "displayId"

[<Literal>]
let _LatestCommit = "latestCommit"

[<Literal>]
let _Repository = "repository"

[<Literal>]
let _User = "user"

[<Literal>]
let _Role = "role"

[<Literal>]
let _Approved = "approved"

[<Literal>]
let _Status = "status"

[<Literal>]
let _LastReviewedCommit = "lastReviewedCommit"

[<Literal>]
let _Version = "version"

[<Literal>]
let _Title = "title"

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

[<Literal>]
let _Text = "text"

[<Literal>]
let _Comments = "comments"

[<Literal>]
let _Tasks = "tasks"

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

[<Literal>]
let _LatestChangeset = "latestChangeset"

module _PR =
    [<Literal>]
    let Opened = "pr:opened"

    [<Literal>]
    let Modified = "pr:modified"

    [<Literal>]
    let ReviewerUpdated = "pr:reviewer:updated"

    [<Literal>]
    let Approved = "pr:reviewer:approved"

    [<Literal>]
    let Unapproved = "pr:reviewer:unapproved"

    [<Literal>]
    let NeedsWork = "pr:reviewer:needs_work"

    [<Literal>]
    let Merged = "pr:merged"

    [<Literal>]
    let Declined = "pr:declined"

    [<Literal>]
    let Deleted = "pr:deleted"

    [<Literal>]
    let CommentAdded = "pr:comment:added"

    [<Literal>]
    let CommentEdited = "pr:comment:edited"

    [<Literal>]
    let CommentDeleted = "pr:comment:deleted"
