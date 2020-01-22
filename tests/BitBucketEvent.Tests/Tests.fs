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
    let gStr = Arb.generate<NonEmptyString>
    let gId = Gen.choose (0, 1000)
    let gBool = Arb.generate<bool>

    type UserGen() =

        static member NonNullString (): Arbitrary<Primitives.NonNullString.T> =
            (fun (x: NonEmptyString) -> x.Get |> Primitives.NonNullString.create) <!> gStr
            |> Arb.fromGen

        static member Ownership(): Arbitrary<Project.Ownership> =
            let gu = Arb.generate<User.User>
            Gen.oneof
                [ Gen.constant Project.Ownership.Public
                  gu |> Gen.map Project.Ownership.Owned ]
            |> Arb.fromGen

        static member Timestamp(): Arbitrary<Primitives.Timestamp.T> =
            Primitives.Timestamp.create <!> Arb.generate<DateTimeOffset>
            |> Arb.fromGen

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
                  failtestNoStackf "error: %s" s
          testPropertyWithConfig config "pull-request event" <| fun (x: PullRequestEvent.PullRequestEvent) ->
              let v =
                  x
                  |> PullRequestEvent.toJsonValue
                  |> Encode.toString 4
              match v |> Decode.fromString PullRequestEvent.decoder with
              | Ok(actual) ->
                  // check <@ actual = x @>
                  Expect.equal actual x "should be equal"
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
                | PullRequestEvent.Opened(common) ->
                    test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T09:58:11+1000")) @>
                    test <@ common.Actor.Email |> NonNullString.value = "admin@example.com" @>
                    test <@ common.PullRequest.Author.User.DisplayName |> NonNullString.value = "Administrator" @>
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
                  | PullRequestEvent.Modified(common, prevTitle, prevDesc, prevTarget) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:15:30+1000")) @>
                      test <@ common.Actor.Email |> NonNullString.value = "example@atlassian.com" @>
                      test <@ common.PullRequest.Author.User.DisplayName |> NonNullString.value = "Administrator" @>
                      test
                          <@ common.PullRequest.ToRef.LatestCommit =
                              ("860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString) @>
                      test <@ prevTitle |> NonNullString.value = "A cool PR" @>
                      test <@ prevDesc |> NonNullString.value = "A neat description" @>
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
                  | PullRequestEvent.ReviewersUpdated(common, added, removed) ->
                      test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:20:07+1000")) @>
                      test <@ common.Actor.Email |> NonNullString.value = "admin@atlassian.com" @>
                      let pr = common.PullRequest
                      test <@ pr.Author.User.DisplayName |> NonNullString.value = "Administrator" @>
                      test
                          <@ pr.ToRef.LatestCommit =
                              ("860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString) @>
                      test <@ added.Length = 1 @>
                      test <@ added.[0].Id = 129659 @>
                      test <@ removed.Length = 1 @>
                      test <@ removed.[0].Email |> NonNullString.value = "user@atlassian.com" @>
                  | x -> failtestNoStackf "should be a reviewer updated event but got %A" x
              | Error(s) -> failtestNoStackf "error: %A" s ]
