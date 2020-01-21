//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Tests

open BitBucketEvent.Types
open BitBucketEvent.Types.PullRequestEvent
open Expecto
open FsCheck
open Swensen.Unquote
open System
open System.IO
open Thoth.Json.Net

module Generator =
    let gStr = Arb.generate<NonEmptyString>
    let gId = Gen.choose (0, 1000)
    let gBool = Arb.generate<bool>

    type UserGen() =

        static member User(): Arbitrary<User.User> =
            let makeUser (name: NonEmptyString) (email: NonEmptyString) id (display: NonEmptyString) active
                (slug: NonEmptyString) (typ: NonEmptyString): User.User =
                { Name = name.Get
                  Email = email.Get
                  Id = id
                  DisplayName = display.Get
                  Active = active
                  Slug = slug.Get
                  Type = typ.Get }

            let gUser = makeUser <!> gStr <*> gStr <*> gId <*> gStr <*> gBool <*> gStr <*> gStr
            gUser |> Arb.fromGen

        static member Project(): Arbitrary<Project.Project> =
            let makeProject (key: NonEmptyString) id (name: NonEmptyString) owner (typ: NonEmptyString): Project.Project =
                { Key = key.Get
                  Id = id
                  Name = name.Get
                  Owner =
                      match owner with
                      | None -> Project.Ownership.Public
                      | Some(o) -> o |> Project.Ownership.Owned
                  Type = typ.Get }

            let gProject = makeProject <!> gStr <*> gId <*> gStr <*> (Arb.generate<User.User> |> Gen.optionOf) <*> gStr
            gProject |> Arb.fromGen

        static member Repository(): Arbitrary<Repository.Repository> =
            let makeRepo (slug: NonEmptyString) (id: int) (name: NonEmptyString) (scmId: NonEmptyString)
                (state: NonEmptyString) (statusMessage: NonEmptyString) (forkable: bool) (project: Project.Project)
                (pub: bool): Repository.Repository =
                { Slug = slug.Get
                  Id = id
                  Name = name.Get
                  ScmId = scmId.Get
                  State = state.Get
                  StatusMessage = statusMessage.Get
                  Forkable = forkable
                  Project = project
                  Public = pub }

            let gProject = Arb.generate<Project.Project>
            let gRepo = makeRepo <!> gStr <*> gId <*> gStr <*> gStr <*> gStr <*> gStr <*> gBool <*> gProject <*> gBool
            gRepo |> Arb.fromGen

        static member Reference(): Arbitrary<Reference.Reference> =
            let makeRef (id: NonEmptyString) (displayId: NonEmptyString) (latestCommit: CommitHash.CommitHash)
                (repo: Repository.Repository): Reference.Reference =
                { Id = id.Get
                  DisplayId = displayId.Get
                  LatestCommit = latestCommit
                  Repository = repo }

            let gRepo = Arb.generate<Repository.Repository>
            let gRef = makeRef <!> gStr <*> gStr <*> Arb.generate<CommitHash.CommitHash> <*> gRepo
            gRef |> Arb.fromGen

        static member Participant(): Arbitrary<Participant.Participant> =
            let makeParticipant user (role: NonEmptyString) approved (status: NonEmptyString) lastReviewed: Participant.Participant =
                { User = user
                  Role = role.Get
                  Approved = approved
                  Status = status.Get
                  LastReviewedCommit = lastReviewed }

            let gUser = Arb.generate<User.User>
            let gParticipant =
                makeParticipant <!> gUser <*> gStr <*> gBool <*> gStr
                <*> (Arb.generate<CommitHash.CommitHash> |> Gen.optionOf)
            gParticipant |> Arb.fromGen

        static member PullRequest(): Arbitrary<PullRequest.PullRequest> =
            let makePR id version (title: NonEmptyString) (state: NonEmptyString) opened closed cDate uDate fromRef
                toRef locked author reviewers participants: PullRequest.PullRequest =
                let toDataTimeOffset (off: int64) =
                    DateTimeOffset.FromUnixTimeMilliseconds(off)
                { Id = id
                  Version = version
                  Title = title.Get
                  State = state.Get
                  Open = opened
                  Closed = closed
                  CreatedDate = cDate |> toDataTimeOffset
                  UpdatedDate = uDate |> toDataTimeOffset
                  FromRef = fromRef
                  ToRef = toRef
                  Locked = locked
                  Author = author
                  Reviewers = reviewers
                  Participants = participants }

            let gPR =
                makePR <!> gId <*> Gen.choose (0, 12345) <*> gStr <*> gStr <*> gBool <*> gBool <*> Arb.generate<int64>
                <*> Arb.generate<int64> <*> Arb.generate<Reference.Reference> <*> Arb.generate<Reference.Reference>
                <*> gBool <*> Arb.generate<Participant.Participant>
                <*> (Arb.generate<Participant.Participant> |> Gen.arrayOf)
                <*> (Arb.generate<Participant.Participant> |> Gen.arrayOf)
            gPR |> Arb.fromGen

        static member Comment(): Arbitrary<Comment.Comment> =
            let makeComment id version (text: NonEmptyString) author cDate uDate: Comment.Comment =
                { Id = id
                  Version = version
                  Text = text.Get
                  Author = author
                  CreatedDate = DateTimeOffset.FromUnixTimeMilliseconds(cDate)
                  UpdatedDate = DateTimeOffset.FromUnixTimeMilliseconds(uDate) }

            let gComment =
                makeComment <!> gId <*> gId <*> gStr <*> Arb.generate<User.User> <*> Arb.generate<int64>
                <*> Arb.generate<int64>
            gComment |> Arb.fromGen

let config = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Generator.UserGen> ] }

let private check x =
    (sprintf "expr: %A" (x |> unquote)) @| (x |> eval)


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
                  failtestNoStackf "error: %s" s ]

[<Tests>]
let commitHashTest =
    testList "Test CommitHash"
        [ testCase "1 byte" <| fun _ ->
            let v = CommitHash.fromString "AB"
            test <@ v = [| 0xABuy |] @>
          testCase "2 byte" <| fun _ ->
              let v = CommitHash.fromString "AbCd"
              test <@ v = [| 0xABuy; 0xCDuy |] @>
          testCase "bad character" <| fun _ -> raises<exn> <@ CommitHash.fromString "AbCdE_" |> ignore @>
          testCase "odd length input" <| fun _ -> raises<exn> <@ CommitHash.fromString "AbCdE" |> ignore @>
          testPropertyWithConfig config "roundtrip" <| fun (x: byte array) ->
              let s = CommitHash.toString x
              let actual = CommitHash.fromString s
              // eprintfn "s = %s" s
              check <@ actual = x @> ]

[<Tests>]
let atlassianExamples =
    testList "Atlassian Examples"
        [ testCase "PR open" <| fun _ ->
            let src = File.ReadAllText "testdata/pr-open.json"
            match Decode.fromString PullRequestEvent.decoder src with
            | Ok(actual) ->
                match actual with
                | Opened(common) ->
                    test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T09:58:11+1000")) @>
                    test <@ common.Actor.Email = "admin@example.com" @>
                    test <@ common.PullRequest.Author.User.DisplayName = "Administrator" @>
                    test
                        <@ common.PullRequest.ToRef.LatestCommit =
                            ("178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString) @>
                | x -> failtestNoStackf "should be a open event but got %A" x
            | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR modified" <| fun _ ->
              let src = File.ReadAllText "testdata/pr-modified.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | Modified(common, prevTitle, prevDesc, prevTarget) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:15:30+1000")) @>
                      test <@ common.Actor.Email = "example@atlassian.com" @>
                      test <@ common.PullRequest.Author.User.DisplayName = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString) @>
                      test <@ prevTitle = "A cool PR" @>
                      test <@ prevDesc = "A neat description" @>
                      test
                          <@ prevTarget.LatestCommit =
                              (CommitHash.fromString "860c4eb4ed0f969b47144234ba13c31c498cca69") @>
                  | x -> failtestNoStackf "should be a modified event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s
          testCase "PR reviewer updated" <| fun _ ->
              let src = File.ReadAllText "testdata/pr-reviewers-updated.json"
              match Decode.fromString PullRequestEvent.decoder src with
              | Ok(actual) ->
                  match actual with
                  | ReviewersUpdated(common, added, removed) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:20:07+1000")) @>
                      test <@ common.Actor.Email = "admin@atlassian.com" @>
                      test <@ common.PullRequest.Author.User.DisplayName = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString) @>
                      test <@ added.Length = 1 @>
                      test <@ added.[0].Id = 129659 @>
                      test <@ removed.Length = 1 @>
                      test <@ removed.[0].Email = "user2@atlassian.com" @>
                  | x -> failtestNoStackf "should be a reviewer updated event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s ]
