//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Tests

open BitBucketEvent.Types
open BitBucketEvent.Types.PullRequestEvent
open Expecto
open FsCheck
open System
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
            let makeProject (key: NonEmptyString) id (name: NonEmptyString) pub (typ: NonEmptyString): Project.Project =
                { Key = key.Get
                  Id = id
                  Name = name.Get
                  Public = pub
                  Type = typ.Get }

            let gProject = makeProject <!> gStr <*> gId <*> gStr <*> gBool <*> gStr
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
                Expect.equal actual x "Should be equal"
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
                  Expect.equal actual x "Should be equal"
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
                  Expect.equal actual x "Should be equal"
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
                  Expect.equal actual x "Should be equal"
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
                  Expect.equal actual x "Should be equal"
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
                  Expect.equal actual x "Should be equal"
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
                  Expect.equal actual x "Should be equal"
              | Error(s) ->
                  failtestNoStackf "error: %s" s ]

[<Tests>]
let commitHashTest =
    testList "Test CommitHash"
        [ testCase "1 byte" <| fun _ ->
            let v = CommitHash.fromString "AB"
            Expect.equal v [| 0xABuy |] "should equal"
          testCase "2 byte" <| fun _ ->
              let v = CommitHash.fromString "AbCd"
              Expect.equal v [| 0xABuy; 0xCDuy |] "should equal"
          testCase "bad character"
              (fun _ -> Expect.throws (fun () -> CommitHash.fromString "AbCdE_" |> ignore) "throw error")
          testCase "odd length input"
              (fun _ -> Expect.throws (fun () -> CommitHash.fromString "AbCdE" |> ignore) "throw error")
          testPropertyWithConfig config "roundtrip" <| fun (x: byte array) ->
              let s = CommitHash.toString x
              let actual = CommitHash.fromString s
              // eprintfn "s = %s" s
              Expect.equal actual x "should equal" ]

[<Tests>]
let atlassianExamples =
    testList "Atlassian Examples"
        [ testCase "PR open" <| fun _ ->
            let src = """
{
  "eventKey":"pr:opened",
  "date":"2017-09-19T09:58:11+1000",
  "actor":{
    "name":"admin",
    "emailAddress":"admin@example.com",
    "id":1,
    "displayName":"Administrator",
    "active":true,
    "slug":"admin",
    "type":"NORMAL"
  },
  "pullRequest":{
    "id":1,
    "version":0,
    "title":"a new file added",
    "state":"OPEN",
    "open":true,
    "closed":false,
    "createdDate":1505779091796,
    "updatedDate":1505779091796,
    "fromRef":{
      "id":"refs/heads/a-branch",
      "displayId":"a-branch",
      "latestCommit":"ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca",
      "repository":{
        "slug":"repository",
        "id":84,
        "name":"repository",
        "scmId":"git",
        "state":"AVAILABLE",
        "statusMessage":"Available",
        "forkable":true,
        "project":{
          "key":"PROJ",
          "id":84,
          "name":"project",
          "public":false,
          "type":"NORMAL"
        },
        "public":false
      }
    },
    "toRef":{
      "id":"refs/heads/master",
      "displayId":"master",
      "latestCommit":"178864a7d521b6f5e720b386b2c2b0ef8563e0dc",
      "repository":{
        "slug":"repository",
        "id":84,
        "name":"repository",
        "scmId":"git",
        "state":"AVAILABLE",
        "statusMessage":"Available",
        "forkable":true,
        "project":{
          "key":"PROJ",
          "id":84,
          "name":"project",
          "public":false,
          "type":"NORMAL"
        },
        "public":false
      }
    },
    "locked":false,
    "author":{
      "user":{
        "name":"admin",
        "emailAddress":"admin@example.com",
        "id":1,
        "displayName":"Administrator",
        "active":true,
        "slug":"admin",
        "type":"NORMAL"
      },
      "role":"AUTHOR",
      "approved":false,
      "status":"UNAPPROVED"
    },
    "reviewers":[
    ],
    "participants":[
    ],
    "links":{
      "self":[
        null
      ]
    }
  }
}"""

            match Decode.fromString PullRequestEvent.decoder src with
            | Ok(actual) ->
                match actual with
                | Opened(common) ->
                    Expect.equal common.Date (DateTimeOffset.Parse("2017-09-19T09:58:11+1000")) "should match"
                    Expect.equal common.Actor.Email "admin@example.com" "should match"
                    Expect.equal common.PullRequest.Author.User.DisplayName "Administrator" "should match"
                    Expect.equal common.PullRequest.ToRef.LatestCommit
                        ("178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString) "should match"
                | _ -> failtestNoStack "should be a open event"
            | Error(s) -> failtestNoStackf "error: %A" s ]
