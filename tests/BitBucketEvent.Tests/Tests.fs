//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Tests

open BitBucketEvent.Types
open BitBucketEvent.Types.Primitives
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
            |> Gen.map (fun x -> NonNullString.create x)
            |> Arb.fromGen

        static member Ownership(): Arbitrary<Project.Ownership> =
            let gu = Arb.generate<User.User>
            Gen.oneof
                [ Gen.constant Project.Ownership.Public
                  gu |> Gen.map Project.Ownership.Owned ]
            |> Arb.fromGen

        static member Timestamp(): Arbitrary<Primitives.Timestamp> =
            Gen.map Timestamp.create Arb.generate<DateTimeOffset> |> Arb.fromGen

let config = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Generator.UserGen> ] }

let private check x =
    (sprintf "expr: %A" (x |> unquote)) @| (x |> eval)

[<Tests>]
let commitHashTest =
    testList "Test CommitHash"
        [ testCase "1 byte" <| fun () ->
            let v = CommitHash.fromString "AB"
            test <@ v = [| 0xABuy |] @>
          testCase "2 byte" <| fun () ->
              let v = CommitHash.fromString "AbCd"
              test <@ v = [| 0xABuy; 0xCDuy |] @>
          testCase "bad character" <| fun () -> raises<exn> <@ CommitHash.fromString "AbCdE_" |> ignore @>
          testCase "odd length input" <| fun () -> raises<exn> <@ CommitHash.fromString "AbCdE" |> ignore @>
          testPropertyWithConfig config "roundtrip" <| fun (x: byte array) ->
              let s = CommitHash.toString x
              let actual = CommitHash.fromString s
              // eprintfn "s = %s" s
              check <@ actual = x @> ]

[<Tests>]
let tests =
    testList "isomorphism"
        [ testPropertyWithConfig config "user" <| fun (x: User.User) ->
            let v =
                x
                |> User.toJsonValue
                |> Encode.toString 4
            // eprintfn "v = %s" v
            match v |> Decode.fromString User.decoder with
            | Ok(actual) ->
                check <@ actual = x @>
            | Error(s) ->
                failtestNoStackf "error: %s" s
          testPropertyWithConfig config "project" <| fun (x: Project.Project) ->
              let v =
                  x
                  |> Project.toJsonValue
                  |> Encode.toString 4
              // eprintfn "v = %s" v
              match v |> Decode.fromString Project.decoder with
              | Ok(actual) ->
                  check <@ actual = x @>
              | Error(s) ->
                  failtestNoStackf "error: %s" s
          testPropertyWithConfig config "repository" <| fun (x: Repository.Repository) ->
              let v =
                  x
                  |> Repository.toJsonValue
                  |> Encode.toString 4
              // eprintfn "v = %s" v
              match v |> Decode.fromString Repository.decoder with
              | Ok(actual) ->
                  check <@ actual = x @>
              | Error(s) ->
                  failtestNoStackf "error: %s" s
          testPropertyWithConfig config "reference" <| fun (x: Reference.Reference) ->
              let v =
                  x
                  |> Reference.toJsonValue
                  |> Encode.toString 4
              // eprintfn "v = %s" v
              match v |> Decode.fromString Reference.decoder with
              | Ok(actual) ->
                  check <@ actual = x @>
              | Error(s) ->
                  failtestNoStackf "error: %s" s
          testPropertyWithConfig config "participant" <| fun (x: Participant.Participant) ->
              let v =
                  x
                  |> Participant.toJsonValue
                  |> Encode.toString 4
              // eprintfn "v = %s" v
              match v |> Decode.fromString Participant.decoder with
              | Ok(actual) ->
                  check <@ actual = x @>
              | Error(s) ->
                  failtestNoStackf "error: %s" s
          testPropertyWithConfig config "comment" <| fun (x: Comment.Comment) ->
              let v =
                  x
                  |> Comment.toJsonValue
                  |> Encode.toString 4
              // eprintfn "v = %s" v
              match v |> Decode.fromString Comment.decoder with
              | Ok(actual) ->
                  check <@ actual = x @>
              | Error(s) ->
                  failtestNoStackf "error: %s" s
          testPropertyWithConfig config "pull-request" <| fun (x: PullRequest.PullRequest) ->
              let v =
                  x
                  |> PullRequest.toJsonValue
                  |> Encode.toString 4
              // eprintfn "v = %s" v
              match v |> Decode.fromString PullRequest.decoder with
              | Ok(actual) ->
                  check <@ actual = x @>
              | Error(s) ->
                  failtestNoStackf "error: %s" s
          testPropertyWithConfig config "pull-request event" <| fun (x: PullRequestEvent.PullRequestEvent) ->
              let v =
                  x
                  |> PullRequestEvent.toJsonValue
                  |> Encode.toString 4
              match v |> Decode.fromString PullRequestEvent.decoder with
              | Ok(actual) ->
                  check <@ actual = x @>
              // Expect.equal actual x "should be equal"
              | Error(s) ->
                  failtestNoStackf "error: %s" s ]

[<Tests>]
let atlassianExamples =
    testList "Atlassian Examples"
        [ testCase "PR open" <| fun () ->
            let src = File.ReadAllText "testdata/pr-open.json"
            match Decode.fromString PullRequestEvent.decoder src with
            | Ok(actual) ->
                match actual with
                | PullRequestEvent.Opened(common) ->
                    test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T09:58:11+1000")) @>
                    test
                        <@ common.Actor.Email
                           |> NonNullString.value = "admin@example.com" @>
                    test
                        <@ common.PullRequest.Author.User.DisplayName
                           |> NonNullString.value = "Administrator" @>
                    test
                        <@ common.PullRequest.ToRef.LatestCommit =
                            ("178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString) @>
                | x -> failtestNoStackf "should be a open event but got %A" x
            | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR modified" <| fun () ->
              let src = File.ReadAllText "testdata/pr-modified.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.Modified(common, prevTitle, prevDesc, prevTarget) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:15:30+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "example@atlassian.com" @>
                      test
                          <@ common.PullRequest.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString) @>
                      test
                          <@ prevTitle
                             |> NonNullString.value = "A cool PR" @>
                      test
                          <@ prevDesc
                             |> NonNullString.value = "A neat description" @>
                      test
                          <@ prevTarget.LatestCommit =
                              (CommitHash.fromString "860c4eb4ed0f969b47144234ba13c31c498cca69") @>
                  | x -> failtestNoStackf "should be a modified event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR reviewer updated" <| fun () ->
              let src = File.ReadAllText "testdata/pr-reviewers-updated.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.ReviewersUpdated(common, added, removed) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:20:07+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "admin@atlassian.com" @>
                      let pr = common.PullRequest
                      test
                          <@ pr.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ pr.ToRef.LatestCommit =
                              ("860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString) @>
                      test <@ added.Length = 1 @>
                      test <@ added.[0].Id = 129659 @>
                      test <@ removed.Length = 1 @>
                      test
                          <@ removed.[0].Email
                             |> NonNullString.value = "user@atlassian.com" @>
                  | x -> failtestNoStackf "should be a reviewer updated event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR approved" <| fun () ->
              let src = File.ReadAllText "testdata/pr-approved.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.Approved(common, participant, status) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:10:01+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "user@example.com" @>
                      let pr = common.PullRequest
                      test
                          <@ pr.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ pr.ToRef.LatestCommit =
                              ("178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString) @>
                      test <@ participant.User.Id = 2 @>
                      test <@ participant.Approved = true @>
                      test
                          <@ participant.LastReviewedCommit =
                              Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) @>
                      test
                          <@ participant.Status
                             |> NonNullString.value = "APPROVED" @>
                      test
                          <@ status
                             |> NonNullString.value = "UNAPPROVED" @>
                  | x -> failtestNoStackf "should be an approved event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR unapproved" <| fun () ->
              let src = File.ReadAllText "testdata/pr-unapproved.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.Unapproved(common, participant, status) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:13:43+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "user@example.com" @>
                      let pr = common.PullRequest
                      test
                          <@ pr.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ pr.ToRef.LatestCommit =
                              ("178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString) @>
                      test <@ participant.User.Id = 2 @>
                      test <@ participant.Approved = false @>
                      test
                          <@ participant.LastReviewedCommit =
                              Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) @>
                      test
                          <@ participant.Status
                             |> NonNullString.value = "UNAPPROVED" @>

                      test
                          <@ status
                             |> NonNullString.value = "APPROVED" @>
                  | x -> failtestNoStackf "should be an unapproved event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR needs work" <| fun () ->
              let src = File.ReadAllText "testdata/pr-needs-work.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.NeedsWork(common, participant, status) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:14:47+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "user@example.com" @>
                      let pr = common.PullRequest
                      test
                          <@ pr.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ pr.ToRef.LatestCommit =
                              ("178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString) @>
                      test
                          <@ pr.Reviewers.[0].Status
                             |> NonNullString.value = "NEEDS_WORK" @>
                      test <@ participant.User.Id = 2 @>
                      test <@ participant.Approved = false @>
                      test
                          <@ participant.LastReviewedCommit =
                              Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) @>
                      test
                          <@ participant.Status
                             |> NonNullString.value = "NEEDS_WORK" @>

                      test
                          <@ status
                             |> NonNullString.value = "UNAPPROVED" @>
                  | x -> failtestNoStackf "should be a needs-work event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR merged" <| fun () ->
              let src = File.ReadAllText "testdata/pr-merged.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.Merged(common) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:39:36+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "user@example.com" @>
                      test
                          <@ common.PullRequest.State
                             |> NonNullString.value = "MERGED" @>
                      test
                          <@ common.PullRequest.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("8d2ad38c918fa6943859fca2176c89ea98b92a21" |> CommitHash.fromString) @>
                  | x -> failtestNoStackf "should be a merged event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR declined" <| fun () ->
              let src = File.ReadAllText "testdata/pr-declined.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.Declined(common) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:14:43+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "admin@example.com" @>
                      test
                          <@ common.PullRequest.State
                             |> NonNullString.value = "DECLINED" @>
                      test
                          <@ common.PullRequest.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString) @>
                  | x -> failtestNoStackf "should be a declined event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR deleted" <| fun () ->
              let src = File.ReadAllText "testdata/pr-deleted.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.Deleted(common) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:16:17+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "admin@example.com" @>
                      test
                          <@ common.PullRequest.State
                             |> NonNullString.value = "OPEN" @>
                      test
                          <@ common.PullRequest.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString) @>
                  | x -> failtestNoStackf "should be a deleted event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR comment added" <| fun () ->
              let src = File.ReadAllText "testdata/pr-comment-added.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.CommentAdded(common, comment, parent) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:21:06+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "admin@example.com" @>
                      test
                          <@ common.PullRequest.State
                             |> NonNullString.value = "OPEN" @>
                      test
                          <@ common.PullRequest.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString) @>
                      test <@ comment.Id = 62 @>
                      test
                          <@ comment.Text
                             |> NonNullString.value = "I am a PR comment" @>
                      test
                          <@ comment.CreatedDate
                             |> Timestamp.toInt = 1505784066751L @>
                      test <@ parent = Some(43) @>
                  | x -> failtestNoStackf "should be a comment added event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR comment edited" <| fun () ->
              let src = File.ReadAllText "testdata/pr-comment-edited.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.CommentEdited(common, comment, parent, prevText) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:24:19+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "admin@example.com" @>
                      test
                          <@ common.PullRequest.State
                             |> NonNullString.value = "OPEN" @>
                      test
                          <@ common.PullRequest.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString) @>
                      test <@ comment.Id = 62 @>
                      test
                          <@ comment.Text
                             |> NonNullString.value = "I am a PR comment that was edited" @>
                      test
                          <@ comment.CreatedDate
                             |> Timestamp.toInt = 1505784066751L @>
                      test <@ parent = Some(43) @>
                      test
                          <@ prevText
                             |> NonNullString.value = "I am a PR comment" @>
                  | x -> failtestNoStackf "should be a comment deleted event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR comment deleted" <| fun () ->
              let src = File.ReadAllText "testdata/pr-comment-deleted.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | PullRequestEvent.CommentDeleted(common, comment, parent) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:25:47+1000")) @>
                      test
                          <@ common.Actor.Email
                             |> NonNullString.value = "admin@example.com" @>
                      test
                          <@ common.PullRequest.State
                             |> NonNullString.value = "OPEN" @>
                      test
                          <@ common.PullRequest.Author.User.DisplayName
                             |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString) @>
                      test <@ comment.Id = 62 @>
                      test
                          <@ comment.Text
                             |> NonNullString.value = "I am a PR comment that was edited" @>
                      test
                          <@ comment.CreatedDate
                             |> Timestamp.toInt = 1505784066751L @>
                      test <@ parent = Some(43) @>
                  | x -> failtestNoStackf "should be a comment deleted event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s ]
