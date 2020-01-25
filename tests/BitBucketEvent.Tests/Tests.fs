//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Tests

open BitBucketEvent.Types
open Expecto
open FsCheck
open Swensen.Unquote
open System
open System.IO
open Thoth.Json.Net


module Generator =
    type UserGen() =

        static member NonNullString(): Arbitrary<NonNullString> =
            Arb.generate<string>
            |> Gen.map NonNullString.create
            |> Arb.fromGen

        static member CommitHash(): Arbitrary<CommitHash> =
            Arb.generate<FixedLengthArray<byte>>
            |> Gen.map (fun x -> x.Get |> CommitHash.create)
            |> Arb.fromGen

        static member Ownership(): Arbitrary<Ownership> =
            let gu = Arb.generate<User>
            Gen.oneof
                [ Gen.constant Ownership.Public
                  gu |> Gen.map Ownership.Owned ]
            |> Arb.fromGen

        static member Timestamp(): Arbitrary<Timestamp> =
            Gen.map Timestamp.create Arb.generate<DateTimeOffset> |> Arb.fromGen

let config =
    { FsCheckConfig.defaultConfig with
          arbitrary = [ typeof<Generator.UserGen> ]
          maxTest = 2000
          endSize = 200 }

let private check x = (sprintf "expr: %A" (x |> unquote)) @| (x |> eval)


[<Tests>]
let testCommitHash =
    testList "Test CommitHash"
        [ testCase "1 byte" <| fun () ->
            let v = CommitHash.fromString "AB"
            test <@ v = ([| 0xABuy |] |> CommitHash.create) @>
          testCase "2 bytes" <| fun () ->
            let v = CommitHash.fromString "AbCd"
            test <@ v = ([| 0xABuy; 0xCDuy |] |> CommitHash.create) @>
          testCase "bad character" <| fun () -> raises<exn> <@ CommitHash.fromString "AbCdE_" |> ignore @>
          testCase "odd length input" <| fun () -> raises<exn> <@ CommitHash.fromString "AbCdE" |> ignore @>
          testPropertyWithConfig config "roundtrip" <| fun (x: CommitHash) ->
            let s = x |> CommitHash.toString
            let actual = s |> CommitHash.fromString
            // eprintfn "s = %s" s
            Expect.equal actual x "should match" ]

[<Tests>]
let testIsomorphism =
    let inline run decoder (x: ^T) =
        let s = (^T: (member AsJsonValue: JsonValue) x)
        match s
              |> Encode.toString 4
              |> Decode.fromString decoder with
        | Ok(actual) -> actual
        | Error(s) -> failtestNoStackf "error: %s" s
    testList "Check isomorphism"
        [ testPropertyWithConfig config "user" <| fun (x: User) ->
            let actual = x |> run User.decoder
            Expect.equal actual x "should match"
          testPropertyWithConfig config "project" <| fun (x: Project) ->
            let actual = x |> run Project.decoder
            Expect.equal actual x "should match"
          testPropertyWithConfig config "repository" <| fun (x: Repository) ->
            let actual = x |> run Repository.decoder
            Expect.equal actual x "should match"
          testPropertyWithConfig config "reference" <| fun (x: Reference) ->
            let actual = x |> run Reference.decoder
            Expect.equal actual x "should match"
          testPropertyWithConfig config "participant" <| fun (x: Participant) ->
            let actual = x |> run Participant.decoder
            Expect.equal actual x "should match"
          testPropertyWithConfig config "comment" <| fun (x: Comment) ->
            let actual = x |> run Comment.decoder
            Expect.equal actual x "should match"
          testPropertyWithConfig config "pull-request" <| fun (x: PullRequest) ->
            let actual = x |> run PullRequest.decoder
            Expect.equal actual x "should match"
          testPropertyWithConfig config "pull-request event" <| fun (x: Event) ->
            let actual = x |> run Event.decoder
            Expect.equal actual x "should match" ]

[<Tests>]
let atlassianExamples =
    let run file =
        let src = File.ReadAllText file
        match Decode.fromString Event.decoder src with
        | Ok(result) -> result
        | Error(s) -> failtestNoStackf "error: %s" s

    testList "Atlassian Examples"
        [ testCase "PR open" <| fun () ->
            match "testdata/pr-open.json" |> run with
            | Event.Opened(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T09:58:11+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> NonNullString.create
                      Email = "admin@example.com" |> NonNullString.create
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 1
                      Version = 0
                      Title = "a new file added" |> NonNullString.create
                      Description = None
                      State = "OPEN" |> NonNullString
                      Open = true
                      Closed = false
                      CreatedDate = 1505779091796L |> Timestamp.fromInt
                      UpdatedDate = 1505779091796L |> Timestamp.fromInt
                      FromRef = {
                          Id = "refs/heads/a-branch" |> NonNullString.create
                          DisplayId = "a-branch" |> NonNullString.create
                          LatestCommit = "ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString
                          Repository = {
                              Slug = "repository" |> NonNullString.create
                              Id = 84
                              Name = "repository" |> NonNullString.create
                              ScmId = "git" |> NonNullString.create
                              State = "AVAILABLE" |> NonNullString.create
                              StatusMessage = "Available" |> NonNullString.create
                              Forkable = true
                              Project = {
                                  Key = "PROJ" |> NonNullString.create
                                  Id = 84
                                  Name = "project" |> NonNullString.create
                                  Owner = Ownership.Public
                                  Type = "NORMAL" |> NonNullString.create
                              }
                              Public = false
                          }
                      }
                      ToRef = {
                          Id = "refs/heads/master" |> NonNullString.create
                          DisplayId = "master" |> NonNullString.create
                          LatestCommit = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString
                          Repository = {
                              Slug = "repository" |> NonNullString.create
                              Id = 84
                              Name = "repository" |> NonNullString.create
                              ScmId = "git" |> NonNullString.create
                              State = "AVAILABLE" |> NonNullString.create
                              StatusMessage = "Available" |> NonNullString.create
                              Forkable = true
                              Project = {
                                  Key = "PROJ" |> NonNullString.create
                                  Id = 84
                                  Name = "project" |> NonNullString.create
                                  Owner = Ownership.Public
                                  Type = "NORMAL" |> NonNullString.create
                              }
                              Public = false
                          }
                      }
                      Locked = false
                      Author = {
                          User = {
                              Name = "admin" |> NonNullString
                              Email = "admin@example.com" |> NonNullString
                              Id = 1
                              DisplayName = "Administrator" |> NonNullString
                              Active = true
                              Slug = "admin" |> NonNullString
                              Type = "NORMAL" |> NonNullString
                          }
                          Role = "AUTHOR" |> NonNullString
                          Approved = false
                          Status = "UNAPPROVED" |> NonNullString
                          LastReviewedCommit = None
                      }
                      Reviewers = [||]
                      Participants = [||] }
                Expect.equal common.PullRequest pr "should match"
            | x -> failtestNoStackf "should be a open event but got %A" x
          testCase "PR modified" <| fun () ->
            match "testdata/pr-modified.json" |> run with
            | Event.Modified(common, prevTitle, prevDesc, prevTarget) ->
                test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:15:30+1000")) @>
                let actor: User =
                    { Name = "Administrator" |> NonNullString.create
                      Email = "example@atlassian.com" |> NonNullString.create
                      Id = 110653
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "pathompson" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                let owner: User =
                    { Name = "Administrator" |> NonNullString
                      Email = "example@atlassian.com" |> NonNullString
                      Id = 110653
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                let pr: PullRequest =
                    { Id = 1
                      Version = 1
                      Title = "A new title" |> NonNullString.create
                      Description = Some ("A new description" |> NonNullString.create)
                      State = "OPEN" |> NonNullString
                      Open = true
                      Closed = false
                      CreatedDate = 1524528879329L |> Timestamp.fromInt
                      UpdatedDate = 1524528930110L |> Timestamp.fromInt
                      FromRef = {
                          Id = "refs/heads/new-branch" |> NonNullString.create
                          DisplayId = "new-branch" |> NonNullString.create
                          LatestCommit = "5a705e60111a4213da46839d9cbf4fc43639b771" |> CommitHash.fromString
                          Repository = {
                              Slug = "example" |> NonNullString.create
                              Id = 12087
                              Name = "example" |> NonNullString.create
                              ScmId = "git" |> NonNullString.create
                              State = "AVAILABLE" |> NonNullString.create
                              StatusMessage = "Available" |> NonNullString.create
                              Forkable = true
                              Project = {
                                  Key = "~ADMIN" |> NonNullString.create
                                  Id = 8504
                                  Name = "Administrator" |> NonNullString.create
                                  Owner = Ownership.Owned owner
                                  Type = "PERSONAL" |> NonNullString.create
                              }
                              Public = false
                          }
                      }
                      ToRef = {
                          Id = "refs/heads/master" |> NonNullString.create
                          DisplayId = "master" |> NonNullString.create
                          LatestCommit = "860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString
                          Repository = {
                              Slug = "example" |> NonNullString.create
                              Id = 12087
                              Name = "example" |> NonNullString.create
                              ScmId = "git" |> NonNullString.create
                              State = "AVAILABLE" |> NonNullString.create
                              StatusMessage = "Available" |> NonNullString.create
                              Forkable = true
                              Project = {
                                  Key = "~ADMIN" |> NonNullString.create
                                  Id = 8504
                                  Name = "Administrator" |> NonNullString.create
                                  Owner = Ownership.Owned owner
                                  Type = "PERSONAL" |> NonNullString.create
                              }
                              Public = false
                          }
                      }
                      Locked = false
                      Author = {
                          User = {
                              Name = "Administrator" |> NonNullString
                              Email = "example@atlassian.com" |> NonNullString
                              Id = 110653
                              DisplayName = "Administrator" |> NonNullString
                              Active = true
                              Slug = "admin" |> NonNullString
                              Type = "NORMAL" |> NonNullString
                          }
                          Role = "AUTHOR" |> NonNullString
                          Approved = false
                          Status = "UNAPPROVED" |> NonNullString
                          LastReviewedCommit = None
                      }
                      Reviewers =
                          [| { User =
                                { Name = "User" |> NonNullString.create
                                  Email = "user@atlassian.com" |> NonNullString.create
                                  Id = 36303
                                  DisplayName = "User" |> NonNullString.create
                                  Active = true
                                  Slug = "user" |> NonNullString.create
                                  Type = "NORMAL" |> NonNullString.create }
                               Role = "REVIEWER" |> NonNullString.create
                               Approved = false
                               Status = "UNAPPROVED" |> NonNullString.create
                               LastReviewedCommit = None } |]
                      Participants = [||] }
                Expect.equal common.PullRequest pr "should match"
                test
                    <@ prevTitle
                       |> NonNullString.value = "A cool PR" @>
                test
                    <@ prevDesc
                       |> NonNullString.value = "A neat description" @>
                test <@ prevTarget.LatestCommit.AsString = "860c4eb4ed0f969b47144234ba13c31c498cca69" @>
            | x -> failtestNoStackf "should be a modified event but got %A" x
          testCase "PR reviewer updated" <| fun () ->
            match "testdata/pr-reviewers-updated.json" |> run with
            | Event.ReviewersUpdated(common, added, removed) ->
                test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:20:07+1000")) @>
                let actor: User =
                    { Name = "Administrator" |> NonNullString.create
                      Email = "admin@atlassian.com" |> NonNullString.create
                      Id = 110653
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                let owner: User =
                    { Name = "admin" |> NonNullString
                      Email = "example@atlassian.com" |> NonNullString
                      Id = 110653
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                let pr: PullRequest =
                    { Id = 1
                      Version = 2
                      Title = "A title" |> NonNullString.create
                      Description = Some ("A description" |> NonNullString.create)
                      State = "OPEN" |> NonNullString
                      Open = true
                      Closed = false
                      CreatedDate = 1524528879329L |> Timestamp.fromInt
                      UpdatedDate = 1524529207598L |> Timestamp.fromInt
                      FromRef = {
                          Id = "refs/heads/new-branch" |> NonNullString.create
                          DisplayId = "new-branch" |> NonNullString.create
                          LatestCommit = "5a705e60111a4213da46839d9cbf4fc43639b771" |> CommitHash.fromString
                          Repository = {
                              Slug = "example" |> NonNullString.create
                              Id = 12087
                              Name = "example" |> NonNullString.create
                              ScmId = "git" |> NonNullString.create
                              State = "AVAILABLE" |> NonNullString.create
                              StatusMessage = "Available" |> NonNullString.create
                              Forkable = true
                              Project = {
                                  Key = "~ADMIN" |> NonNullString.create
                                  Id = 8504
                                  Name = "Administrator" |> NonNullString.create
                                  Owner = Ownership.Owned owner
                                  Type = "PERSONAL" |> NonNullString.create
                              }
                              Public = false
                          }
                      }
                      ToRef = {
                          Id = "refs/heads/master" |> NonNullString.create
                          DisplayId = "master" |> NonNullString.create
                          LatestCommit = "860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString
                          Repository = {
                              Slug = "example" |> NonNullString.create
                              Id = 12087
                              Name = "example" |> NonNullString.create
                              ScmId = "git" |> NonNullString.create
                              State = "AVAILABLE" |> NonNullString.create
                              StatusMessage = "Available" |> NonNullString.create
                              Forkable = true
                              Project = {
                                  Key = "~ADMIN" |> NonNullString.create
                                  Id = 8504
                                  Name = "Administrator" |> NonNullString.create
                                  Owner = Ownership.Owned
                                            {owner with
                                                Name = "Administrator" |> NonNullString.create
                                                Email = "admin@atlassian.com" |> NonNullString.create }
                                  Type = "PERSONAL" |> NonNullString.create
                              }
                              Public = false
                          }
                      }
                      Locked = false
                      Author = {
                          User = {
                              Name = "Administrator" |> NonNullString
                              Email = "admin@atlassian.com" |> NonNullString
                              Id = 110653
                              DisplayName = "Administrator" |> NonNullString
                              Active = true
                              Slug = "admin" |> NonNullString
                              Type = "NORMAL" |> NonNullString
                          }
                          Role = "AUTHOR" |> NonNullString
                          Approved = false
                          Status = "UNAPPROVED" |> NonNullString
                          LastReviewedCommit = None
                      }
                      Reviewers =
                          [| { User =
                                { Name = "pathompson_admin" |> NonNullString.create
                                  Email = "pathompson@atlassian.com" |> NonNullString.create
                                  Id = 129659
                                  DisplayName = "Paul Thompson Admin" |> NonNullString.create
                                  Active = true
                                  Slug = "pathompson_admin" |> NonNullString.create
                                  Type = "NORMAL" |> NonNullString.create }
                               Role = "REVIEWER" |> NonNullString.create
                               Approved = false
                               Status = "UNAPPROVED" |> NonNullString.create
                               LastReviewedCommit = None } |]
                      Participants = [||] }
                Expect.equal common.PullRequest pr "should match"
                test <@ added.Length = 1 @>
                Expect.equal
                    added.[0]
                    { Name = "new user" |> NonNullString.create
                      Email = "user2@atlassian.com" |> NonNullString.create
                      Id = 129659
                      DisplayName = "New User" |> NonNullString.create
                      Active = true
                      Slug = "new_user" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                    "should match"
                test <@ removed.Length = 1 @>
                Expect.equal
                    removed.[0]
                    { Name = "user" |> NonNullString.create
                      Email = "user@atlassian.com" |> NonNullString.create
                      Id = 36303
                      DisplayName = "User" |> NonNullString.create
                      Active = true
                      Slug = "user" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                    "should match"
            | x -> failtestNoStackf "should be a reviewer updated event but got %A" x
          testCase "PR approved" <| fun () ->
            match "testdata/pr-approved.json" |> run with
            | Event.Approved(common, participant, status) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:10:01+1000")) @>
                let actor: User =
                    { Name = "user" |> NonNullString.create
                      Email = "user@example.com" |> NonNullString.create
                      Id = 2
                      DisplayName = "User" |> NonNullString.create
                      Active = true
                      Slug = "user" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                let pr = common.PullRequest
                test <@ pr.ToRef.LatestCommit.AsString = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" @>
                test <@ participant.User.Id = 2 @>
                test <@ participant.Approved = true @>
                test
                    <@ participant.LastReviewedCommit =
                        Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) @>
                test <@ participant.Status.AsString = "APPROVED" @>
                test
                    <@ status
                       |> NonNullString.value = "UNAPPROVED" @>
            | x -> failtestNoStackf "should be an approved event but got %A" x
          testCase "PR unapproved" <| fun () ->
            match "testdata/pr-unapproved.json" |> run with
            | Event.Unapproved(common, participant, status) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:13:43+1000")) @>
                let actor: User =
                    { Name = "user" |> NonNullString.create
                      Email = "user@example.com" |> NonNullString.create
                      Id = 2
                      DisplayName = "User" |> NonNullString.create
                      Active = true
                      Slug = "user" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                let pr = common.PullRequest
                test <@ pr.Author.User.DisplayName.AsString = "Administrator" @>
                test <@ pr.ToRef.LatestCommit.AsString = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" @>
                test <@ participant.User.Id = 2 @>
                test <@ participant.Approved = false @>
                test
                    <@ participant.LastReviewedCommit =
                        Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) @>
                test <@ participant.Status.AsString = "UNAPPROVED" @>
                test
                    <@ status
                       |> NonNullString.value = "APPROVED" @>
            | x -> failtestNoStackf "should be an unapproved event but got %A" x
          testCase "PR needs work" <| fun () ->
            match "testdata/pr-needs-work.json" |> run with
            | Event.NeedsWork(common, participant, status) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:14:47+1000")) @>
                let actor: User =
                    { Name = "user" |> NonNullString.create
                      Email = "user@example.com" |> NonNullString.create
                      Id = 2
                      DisplayName = "User" |> NonNullString.create
                      Active = true
                      Slug = "user" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                let pr = common.PullRequest
                test <@ pr.Author.User.DisplayName.AsString = "Administrator" @>
                test <@ pr.ToRef.LatestCommit.AsString = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" @>
                test <@ pr.Reviewers.[0].Status.AsString = "NEEDS_WORK" @>
                test <@ participant.User.Id = 2 @>
                test <@ participant.Approved = false @>
                test
                    <@ participant.LastReviewedCommit =
                        Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) @>
                test <@ participant.Status.AsString = "NEEDS_WORK" @>

                test <@ status |> NonNullString.value = "UNAPPROVED" @>
            | x -> failtestNoStackf "should be a needs-work event but got %A" x
          testCase "PR merged" <| fun () ->
            match "testdata/pr-merged.json" |> run with
            | Event.Merged(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:39:36+1000")) @>
                let actor: User =
                    { Name = "user" |> NonNullString.create
                      Email = "user@example.com" |> NonNullString.create
                      Id = 2
                      DisplayName = "User" |> NonNullString.create
                      Active = true
                      Slug = "user" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                test <@ common.PullRequest.State.AsString = "MERGED" @>
                test <@ common.PullRequest.Author.User.DisplayName.AsString = "Administrator" @>
                test
                    <@ common.PullRequest.ToRef.LatestCommit.AsString = "8d2ad38c918fa6943859fca2176c89ea98b92a21" @>
            | x -> failtestNoStackf "should be a merged event but got %A" x
          testCase "PR declined" <| fun () ->
            match "testdata/pr-declined.json" |> run with
            | Event.Declined(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:14:43+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> NonNullString.create
                      Email = "admin@example.com" |> NonNullString.create
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                test <@ common.PullRequest.State.AsString = "DECLINED" @>
                test <@ common.PullRequest.Author.User.DisplayName.AsString = "Administrator" @>
                test
                    <@ common.PullRequest.ToRef.LatestCommit.AsString = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" @>
            | x -> failtestNoStackf "should be a declined event but got %A" x
          testCase "PR deleted" <| fun () ->
            match "testdata/pr-deleted.json" |> run with
            | Event.Deleted(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:16:17+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> NonNullString.create
                      Email = "admin@example.com" |> NonNullString.create
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                test <@ common.PullRequest.State.AsString = "OPEN" @>
                test <@ common.PullRequest.Author.User.DisplayName.AsString = "Administrator" @>
                test
                    <@ common.PullRequest.ToRef.LatestCommit.AsString = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" @>
            | x -> failtestNoStackf "should be a deleted event but got %A" x
          testCase "PR comment added" <| fun () ->
            match "testdata/pr-comment-added.json" |> run with
            | Event.CommentAdded(common, comment, parent) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:21:06+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> NonNullString.create
                      Email = "admin@example.com" |> NonNullString.create
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                test <@ common.PullRequest.State.AsString = "OPEN" @>
                test <@ common.PullRequest.Author.User.DisplayName.AsString = "Administrator" @>
                test
                    <@ common.PullRequest.ToRef.LatestCommit.AsString = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" @>
                test <@ comment.Id = 62 @>
                test <@ comment.Text.AsString = "I am a PR comment" @>
                test <@ comment.CreatedDate.AsInt = 1505784066751L @>
                test <@ parent = Some(43) @>
            | x -> failtestNoStackf "should be a comment added event but got %A" x
          testCase "PR comment edited" <| fun () ->
            match "testdata/pr-comment-edited.json" |> run with
            | Event.CommentEdited(common, comment, parent, prevText) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:24:19+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> NonNullString.create
                      Email = "admin@example.com" |> NonNullString.create
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                test <@ common.PullRequest.State.AsString = "OPEN" @>
                test <@ common.PullRequest.Author.User.DisplayName.AsString = "Administrator" @>
                test
                    <@ common.PullRequest.ToRef.LatestCommit.AsString = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" @>
                test <@ comment.Id = 62 @>
                test <@ comment.Text.AsString = "I am a PR comment that was edited" @>
                test <@ comment.CreatedDate.AsInt = 1505784066751L @>
                test <@ parent = Some(43) @>
                test <@ prevText |> NonNullString.value = "I am a PR comment" @>
            | x -> failtestNoStackf "should be a comment deleted event but got %A" x
          testCase "PR comment deleted" <| fun () ->
            match "testdata/pr-comment-deleted.json" |> run with
            | Event.CommentDeleted(common, comment, parent) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:25:47+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> NonNullString.create
                      Email = "admin@example.com" |> NonNullString.create
                      DisplayName = "Administrator" |> NonNullString.create
                      Active = true
                      Slug = "admin" |> NonNullString.create
                      Type = "NORMAL" |> NonNullString.create }
                Expect.equal common.Actor actor "should match"
                test <@ common.PullRequest.State.AsString = "OPEN" @>
                test <@ common.PullRequest.Author.User.DisplayName.AsString = "Administrator" @>
                test
                    <@ common.PullRequest.ToRef.LatestCommit.AsString = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" @>
                test <@ comment.Id = 62 @>
                test <@ comment.Text.AsString = "I am a PR comment that was edited" @>
                test <@ comment.CreatedDate.AsInt = 1505784066751L @>
                test <@ parent = Some(43) @>
            | x -> failtestNoStackf "should be a comment deleted event but got %A" x ]
