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


let private check x = (sprintf "expr: %A" (x |> unquote)) @| (x |> eval)

let private toStr = NonNullString.create

module Generator =
    type UserGen() =
        static member NonNullString(): Arbitrary<NonNullString> =
            Arb.generate<string>
            |> Gen.map toStr
            |> Arb.fromGen

        static member CommitHash(): Arbitrary<CommitHash> =
            Arb.generate<FixedLengthArray<byte>>
            |> Gen.map (fun x -> x.Get |> CommitHash.create)
            |> Arb.fromGen

        static member Ownership(): Arbitrary<Ownership> =
            let gu = Arb.generate<User>
            Gen.oneof
                [ Gen.map Ownership.Public Arb.generate<bool>
                  gu |> Gen.map Ownership.Owned ]
            |> Arb.fromGen

        static member Timestamp(): Arbitrary<Timestamp> =
            Gen.map Timestamp.create Arb.generate<DateTimeOffset> |> Arb.fromGen


let config =
    { FsCheckConfig.defaultConfig with
          arbitrary = [ typeof<Generator.UserGen> ]
          maxTest = 2000
          endSize = 200 }

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


module private AtlassianExamples =
    let run file =
        let src = File.ReadAllText file
        match Decode.fromString Event.decoder src with
        | Ok(result) -> result
        | Error(s) -> failtestNoStackf "error: %s" s
    let prOpen =
        testCase "PR open" <| fun () ->
            match "testdata/pr-open.json" |> run with
            | Event.Opened(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T09:58:11+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> toStr
                      Email = "admin@example.com" |> toStr
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 1
                      Version = 0
                      Title = "a new file added" |> toStr
                      Description = None
                      State = "OPEN" |> NonNullString
                      Open = true
                      Closed = false
                      CreatedDate = 1505779091796L |> Timestamp.fromInt
                      UpdatedDate = 1505779091796L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/a-branch" |> toStr
                            DisplayId = "a-branch" |> toStr
                            LatestCommit = "ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> NonNullString
                                  Email = "admin@example.com" |> NonNullString
                                  Id = 1
                                  DisplayName = "Administrator" |> NonNullString
                                  Active = true
                                  Slug = "admin" |> NonNullString
                                  Type = "NORMAL" |> NonNullString }
                            Role = "AUTHOR" |> NonNullString
                            Approved = false
                            Status = "UNAPPROVED" |> NonNullString
                            LastReviewedCommit = None }
                      Reviewers = [||]
                      Participants = [||] }
                Expect.equal common.PullRequest pr "should match"
            | x -> failtestNoStackf "should be a open event but got %A" x

    let prModified =
        testCase "PR modified" <| fun () ->
            match "testdata/pr-modified.json" |> run with
            | Event.Modified(common, prevTitle, prevDesc, prevTarget) ->
                test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:15:30+1000")) @>
                let actor: User =
                    { Name = "Administrator" |> toStr
                      Email = "example@atlassian.com" |> toStr
                      Id = 110653
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "pathompson" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let owner: User =
                    { Name = "Administrator" |> NonNullString
                      Email = "example@atlassian.com" |> NonNullString
                      Id = 110653
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }

                let pr: PullRequest =
                    { Id = 1
                      Version = 1
                      Title = "A new title" |> toStr
                      Description = Some("A new description" |> toStr)
                      State = "OPEN" |> NonNullString
                      Open = true
                      Closed = false
                      CreatedDate = 1524528879329L |> Timestamp.fromInt
                      UpdatedDate = 1524528930110L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/new-branch" |> toStr
                            DisplayId = "new-branch" |> toStr
                            LatestCommit = "5a705e60111a4213da46839d9cbf4fc43639b771" |> CommitHash.fromString
                            Repository =
                                { Slug = "example" |> toStr
                                  Id = 12087
                                  Name = "example" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "~ADMIN" |> toStr
                                        Id = 8504
                                        Name = "Administrator" |> toStr
                                        Owner = Ownership.Owned owner
                                        Type = "PERSONAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString
                            Repository =
                                { Slug = "example" |> toStr
                                  Id = 12087
                                  Name = "example" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "~ADMIN" |> toStr
                                        Id = 8504
                                        Name = "Administrator" |> toStr
                                        Owner = Ownership.Owned owner
                                        Type = "PERSONAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "Administrator" |> NonNullString
                                  Email = "example@atlassian.com" |> NonNullString
                                  Id = 110653
                                  DisplayName = "Administrator" |> NonNullString
                                  Active = true
                                  Slug = "admin" |> NonNullString
                                  Type = "NORMAL" |> NonNullString }
                            Role = "AUTHOR" |> NonNullString
                            Approved = false
                            Status = "UNAPPROVED" |> NonNullString
                            LastReviewedCommit = None }
                      Reviewers =
                          [| { User =
                                   { Name = "User" |> toStr
                                     Email = "user@atlassian.com" |> toStr
                                     Id = 36303
                                     DisplayName = "User" |> toStr
                                     Active = true
                                     Slug = "user" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "REVIEWER" |> toStr
                               Approved = false
                               Status = "UNAPPROVED" |> toStr
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

    let prReviewerUpdated =
        testCase "PR reviewer updated" <| fun () ->
            match "testdata/pr-reviewers-updated.json" |> run with
            | Event.ReviewersUpdated(common, added, removed) ->
                test <@ common.Date = (DateTimeOffset.Parse("2018-04-24T10:20:07+1000")) @>
                let actor: User =
                    { Name = "Administrator" |> toStr
                      Email = "admin@atlassian.com" |> toStr
                      Id = 110653
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let owner: User =
                    { Name = "admin" |> NonNullString
                      Email = "example@atlassian.com" |> NonNullString
                      Id = 110653
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }

                let pr: PullRequest =
                    { Id = 1
                      Version = 2
                      Title = "A title" |> toStr
                      Description = Some("A description" |> toStr)
                      State = "OPEN" |> NonNullString
                      Open = true
                      Closed = false
                      CreatedDate = 1524528879329L |> Timestamp.fromInt
                      UpdatedDate = 1524529207598L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/new-branch" |> toStr
                            DisplayId = "new-branch" |> toStr
                            LatestCommit = "5a705e60111a4213da46839d9cbf4fc43639b771" |> CommitHash.fromString
                            Repository =
                                { Slug = "example" |> toStr
                                  Id = 12087
                                  Name = "example" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "~ADMIN" |> toStr
                                        Id = 8504
                                        Name = "Administrator" |> toStr
                                        Owner = Ownership.Owned owner
                                        Type = "PERSONAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "860c4eb4ed0f969b47144234ba13c31c498cca69" |> CommitHash.fromString
                            Repository =
                                { Slug = "example" |> toStr
                                  Id = 12087
                                  Name = "example" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "~ADMIN" |> toStr
                                        Id = 8504
                                        Name = "Administrator" |> toStr
                                        Owner =
                                            Ownership.Owned
                                                { owner with
                                                      Name = "Administrator" |> toStr
                                                      Email = "admin@atlassian.com" |> toStr }
                                        Type = "PERSONAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "Administrator" |> NonNullString
                                  Email = "admin@atlassian.com" |> NonNullString
                                  Id = 110653
                                  DisplayName = "Administrator" |> NonNullString
                                  Active = true
                                  Slug = "admin" |> NonNullString
                                  Type = "NORMAL" |> NonNullString }
                            Role = "AUTHOR" |> NonNullString
                            Approved = false
                            Status = "UNAPPROVED" |> NonNullString
                            LastReviewedCommit = None }
                      Reviewers =
                          [| { User =
                                   { Name = "pathompson_admin" |> toStr
                                     Email = "pathompson@atlassian.com" |> toStr
                                     Id = 129659
                                     DisplayName = "Paul Thompson Admin" |> toStr
                                     Active = true
                                     Slug = "pathompson_admin" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "REVIEWER" |> toStr
                               Approved = false
                               Status = "UNAPPROVED" |> toStr
                               LastReviewedCommit = None } |]
                      Participants = [||] }

                Expect.equal common.PullRequest pr "should match"
                test <@ added.Length = 1 @>
                Expect.equal added.[0]
                    { Name = "new user" |> toStr
                      Email = "user2@atlassian.com" |> toStr
                      Id = 129659
                      DisplayName = "New User" |> toStr
                      Active = true
                      Slug = "new_user" |> toStr
                      Type = "NORMAL" |> toStr } "should match"
                test <@ removed.Length = 1 @>
                Expect.equal removed.[0]
                    { Name = "user" |> toStr
                      Email = "user@atlassian.com" |> toStr
                      Id = 36303
                      DisplayName = "User" |> toStr
                      Active = true
                      Slug = "user" |> toStr
                      Type = "NORMAL" |> toStr } "should match"
            | x -> failtestNoStackf "should be a reviewer updated event but got %A" x

    let prApproved =
        testCase "PR approved" <| fun () ->
            match "testdata/pr-approved.json" |> run with
            | Event.Approved(common, participant, status) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:10:01+1000")) @>
                let actor: User =
                    { Name = "user" |> toStr
                      Email = "user@example.com" |> toStr
                      Id = 2
                      DisplayName = "User" |> toStr
                      Active = true
                      Slug = "user" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"

                let pr: PullRequest =
                    { Id = 1
                      Version = 1
                      Title = "a new file added" |> toStr
                      Description = Some("A new description, added a user" |> toStr)
                      State = "OPEN" |> NonNullString
                      Open = true
                      Closed = false
                      CreatedDate = 1505779091796L |> Timestamp.fromInt
                      UpdatedDate = 1505779257496L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/a-branch" |> toStr
                            DisplayId = "a-branch" |> toStr
                            LatestCommit = "ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [| { User =
                                   { Name = "user" |> toStr
                                     Email = "user@example.com" |> toStr
                                     Id = 2
                                     DisplayName = "User" |> toStr
                                     Active = true
                                     Slug = "user" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "REVIEWER" |> toStr
                               Approved = true
                               Status = "APPROVED" |> toStr
                               LastReviewedCommit = Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) } |]
                      Participants = [||] }
                Expect.equal common.PullRequest pr "should match"
                Expect.equal
                    participant
                    { User =
                        { Name = "user" |> toStr
                          Email = "user@example.com" |> toStr
                          Id = 2
                          DisplayName = "User" |> toStr
                          Active = true
                          Slug = "user" |> toStr
                          Type = "NORMAL" |> toStr }
                      LastReviewedCommit = Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString)
                      Role = "REVIEWER" |> toStr
                      Approved = true
                      Status = "APPROVED" |> toStr }
                    "should match"
                test
                    <@ status
                       |> NonNullString.value = "UNAPPROVED" @>
            | x -> failtestNoStackf "should be an approved event but got %A" x

    let prUnapproved =
        testCase "PR unapproved" <| fun () ->
            match "testdata/pr-unapproved.json" |> run with
            | Event.Unapproved(common, participant, status) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:13:43+1000")) @>
                let actor: User =
                    { Name = "user" |> toStr
                      Email = "user@example.com" |> toStr
                      Id = 2
                      DisplayName = "User" |> toStr
                      Active = true
                      Slug = "user" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"

                let pr: PullRequest =
                    { Id = 1
                      Version = 1
                      Title = "a new file added" |> toStr
                      Description = Some("A new description, added a user" |> toStr)
                      State = "OPEN" |> toStr
                      Open = true
                      Closed = false
                      CreatedDate = 1505779091796L |> Timestamp.fromInt
                      UpdatedDate = 1505779257496L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/a-branch" |> toStr
                            DisplayId = "a-branch" |> toStr
                            LatestCommit = "ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [| { User =
                                   { Name = "user" |> toStr
                                     Email = "user@example.com" |> toStr
                                     Id = 2
                                     DisplayName = "User" |> toStr
                                     Active = true
                                     Slug = "user" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "REVIEWER" |> toStr
                               Approved = false
                               Status = "UNAPPROVED" |> toStr
                               LastReviewedCommit = Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) } |]
                      Participants = [||] }
                Expect.equal common.PullRequest pr "should match"
                Expect.equal
                    participant
                    { User =
                        { Name = "user" |> toStr
                          Email = "user@example.com" |> toStr
                          Id = 2
                          DisplayName = "User" |> toStr
                          Active = true
                          Slug = "user" |> toStr
                          Type = "NORMAL" |> toStr }
                      LastReviewedCommit = Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString)
                      Role = "REVIEWER" |> toStr
                      Approved = false
                      Status = "UNAPPROVED" |> toStr }
                    "should match"
                test
                    <@ status
                       |> NonNullString.value = "APPROVED" @>
            | x -> failtestNoStackf "should be an unapproved event but got %A" x

    let prNeedsWork =
        testCase "PR needs work" <| fun () ->
            match "testdata/pr-needs-work.json" |> run with
            | Event.NeedsWork(common, participant, status) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:14:47+1000")) @>
                let actor: User =
                    { Name = "user" |> toStr
                      Email = "user@example.com" |> toStr
                      Id = 2
                      DisplayName = "User" |> toStr
                      Active = true
                      Slug = "user" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"

                let pr: PullRequest =
                    { Id = 1
                      Version = 1
                      Title = "a new file added" |> toStr
                      Description = Some("A new description, added a user" |> toStr)
                      State = "OPEN" |> toStr
                      Open = true
                      Closed = false
                      CreatedDate = 1505779091796L |> Timestamp.fromInt
                      UpdatedDate = 1505779257496L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/a-branch" |> toStr
                            DisplayId = "a-branch" |> toStr
                            LatestCommit = "ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "178864a7d521b6f5e720b386b2c2b0ef8563e0dc" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [| { User =
                                   { Name = "user" |> toStr
                                     Email = "user@example.com" |> toStr
                                     Id = 2
                                     DisplayName = "User" |> toStr
                                     Active = true
                                     Slug = "user" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "REVIEWER" |> toStr
                               Approved = false
                               Status = "NEEDS_WORK" |> toStr
                               LastReviewedCommit = Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString) } |]
                      Participants = [||] }
                Expect.equal common.PullRequest pr "should match"
                Expect.equal
                    participant
                    { User =
                        { Name = "user" |> toStr
                          Email = "user@example.com" |> toStr
                          Id = 2
                          DisplayName = "User" |> toStr
                          Active = true
                          Slug = "user" |> toStr
                          Type = "NORMAL" |> toStr }
                      LastReviewedCommit = Some("ef8755f06ee4b28c96a847a95cb8ec8ed6ddd1ca" |> CommitHash.fromString)
                      Role = "REVIEWER" |> toStr
                      Approved = false
                      Status = "NEEDS_WORK" |> toStr }
                    "should match"
                test
                    <@ status
                       |> NonNullString.value = "UNAPPROVED" @>
            | x -> failtestNoStackf "should be a needs-work event but got %A" x

    let prMerged =
        testCase "PR merged" <| fun () ->
            match "testdata/pr-merged.json" |> run with
            | Event.Merged(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T10:39:36+1000")) @>
                let actor: User =
                    { Name = "user" |> toStr
                      Email = "user@example.com" |> toStr
                      Id = 2
                      DisplayName = "User" |> toStr
                      Active = true
                      Slug = "user" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 9
                      Version = 2
                      Title = "file edited online with Bitbucket" |> toStr
                      Description = None
                      State = "MERGED" |> toStr
                      Open = false
                      Closed = true
                      CreatedDate = 1505781560908L |> Timestamp.fromInt
                      UpdatedDate = 1505781576361L |> Timestamp.fromInt
                      ClosedDate = Some (1505781576361L |> Timestamp.fromInt)
                      FromRef =
                          { Id = "refs/heads/admin/file-1505781548644" |> toStr
                            DisplayId = "admin/file-1505781548644" |> toStr
                            LatestCommit = "45f9690c928915a5e1c4366d5ee1985eea03f05d" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "8d2ad38c918fa6943859fca2176c89ea98b92a21" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [||]
                      Participants =
                          [| { User =
                                   { Name = "user" |> toStr
                                     Email = "user@example.com" |> toStr
                                     Id = 2
                                     DisplayName = "User" |> toStr
                                     Active = true
                                     Slug = "user" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "PARTICIPANT" |> toStr
                               Approved = false
                               Status = "UNAPPROVED" |> toStr
                               LastReviewedCommit = None } |] }
                Expect.equal common.PullRequest pr "should match"
            | x -> failtestNoStackf "should be a merged event but got %A" x

    let prDeclined =
        testCase "PR declined" <| fun () ->
            match "testdata/pr-declined.json" |> run with
            | Event.Declined(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:14:43+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> toStr
                      Email = "admin@example.com" |> toStr
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 10
                      Version = 2
                      Title = "Commit message" |> toStr
                      Description = None
                      State = "DECLINED" |> toStr
                      Open = false
                      Closed = true
                      CreatedDate = 1505783668760L |> Timestamp.fromInt
                      UpdatedDate = 1505783683969L |> Timestamp.fromInt
                      ClosedDate = Some (1505783683969L |> Timestamp.fromInt)
                      FromRef =
                          { Id = "refs/heads/decline-me" |> toStr
                            DisplayId = "decline-me" |> toStr
                            LatestCommit = "2d9fb6b9a46eafb1dcef7b008d1a429d45ca742c" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [| { User =
                                   { Name = "user" |> toStr
                                     Email = "user@example.com" |> toStr
                                     Id = 2
                                     DisplayName = "User" |> toStr
                                     Active = true
                                     Slug = "user" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "REVIEWER" |> toStr
                               Approved = false
                               Status = "UNAPPROVED" |> toStr
                               LastReviewedCommit = None } |]
                      Participants =
                          [||] }
                Expect.equal common.PullRequest pr "should match"
            | x -> failtestNoStackf "should be a declined event but got %A" x

    let prDeleted =
        testCase "PR deleted" <| fun () ->
            match "testdata/pr-deleted.json" |> run with
            | Event.Deleted(common) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:16:17+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> toStr
                      Email = "admin@example.com" |> toStr
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 10
                      Version = 3
                      Title = "Commit message" |> toStr
                      Description = None
                      State = "OPEN" |> toStr
                      Open = true
                      Closed = false
                      CreatedDate = 1505783668760L |> Timestamp.fromInt
                      UpdatedDate = 1505783750704L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/decline-me" |> toStr
                            DisplayId = "decline-me" |> toStr
                            LatestCommit = "2d9fb6b9a46eafb1dcef7b008d1a429d45ca742c" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [| { User =
                                   { Name = "user" |> toStr
                                     Email = "user@example.com" |> toStr
                                     Id = 2
                                     DisplayName = "User" |> toStr
                                     Active = true
                                     Slug = "user" |> toStr
                                     Type = "NORMAL" |> toStr }
                               Role = "REVIEWER" |> toStr
                               Approved = false
                               Status = "UNAPPROVED" |> toStr
                               LastReviewedCommit = None } |]
                      Participants =
                          [||] }
                Expect.equal common.PullRequest pr "should match"
            | x -> failtestNoStackf "should be a deleted event but got %A" x

    let prCommentAdded =
        testCase "PR comment added" <| fun () ->
            match "testdata/pr-comment-added.json" |> run with
            | Event.CommentAdded(common, comment, parent) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:21:06+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> toStr
                      Email = "admin@example.com" |> toStr
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 11
                      Version = 1
                      Title = "A cool PR" |> toStr
                      Description = None
                      State = "OPEN" |> toStr
                      Open = true
                      Closed = false
                      CreatedDate = 1505783860548L |> Timestamp.fromInt
                      UpdatedDate = 1505783878981L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/comment-pr" |> toStr
                            DisplayId = "comment-pr" |> toStr
                            LatestCommit = "ddc19f786996396d57e17c8f6d1d05d00318ad10" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [||]
                      Participants =
                          [||] }
                Expect.equal common.PullRequest pr "should match"
                Expect.equal
                    comment
                    { Id = 62
                      Version = 0
                      Text = "I am a PR comment" |> toStr
                      Author =
                          { Name = "admin" |> toStr
                            Email = "admin@example.com" |> toStr
                            Id = 1
                            DisplayName = "Administrator" |> toStr
                            Active = true
                            Slug = "admin" |> toStr
                            Type = "NORMAL" |> toStr }
                      CreatedDate = 1505784066751L |> Timestamp.fromInt
                      UpdatedDate = 1505784066751L |> Timestamp.fromInt }
                    "should match"
                test <@ parent = Some(43) @>
            | x -> failtestNoStackf "should be a comment added event but got %A" x

    let prCommentEdited =
        testCase "PR comment edited" <| fun () ->
            match "testdata/pr-comment-edited.json" |> run with
            | Event.CommentEdited(common, comment, parent, prevText) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:24:19+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> toStr
                      Email = "admin@example.com" |> toStr
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 11
                      Version = 1
                      Title = "A cool PR" |> toStr
                      Description = None
                      State = "OPEN" |> toStr
                      Open = true
                      Closed = false
                      CreatedDate = 1505783860548L |> Timestamp.fromInt
                      UpdatedDate = 1505783878981L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/comment-pr" |> toStr
                            DisplayId = "comment-pr" |> toStr
                            LatestCommit = "ddc19f786996396d57e17c8f6d1d05d00318ad10" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [||]
                      Participants =
                          [||] }
                Expect.equal common.PullRequest pr "should match"
                Expect.equal
                    comment
                    { Id = 62
                      Version = 1
                      Text = "I am a PR comment that was edited" |> toStr
                      Author =
                          { Name = "admin" |> toStr
                            Email = "admin@example.com" |> toStr
                            Id = 1
                            DisplayName = "Administrator" |> toStr
                            Active = true
                            Slug = "admin" |> toStr
                            Type = "NORMAL" |> toStr }
                      CreatedDate = 1505784066751L |> Timestamp.fromInt
                      UpdatedDate = 1505784259446L |> Timestamp.fromInt }
                    "should match"
                test <@ parent = Some(43) @>
                test
                    <@ prevText
                       |> NonNullString.value = "I am a PR comment" @>
            | x -> failtestNoStackf "should be a comment deleted event but got %A" x

    let prCommentDeleted =
        testCase "PR comment deleted" <| fun () ->
            match "testdata/pr-comment-deleted.json" |> run with
            | Event.CommentDeleted(common, comment, parent) ->
                test <@ common.Date = (DateTimeOffset.Parse("2017-09-19T11:25:47+1000")) @>
                let actor: User =
                    { Id = 1
                      Name = "admin" |> toStr
                      Email = "admin@example.com" |> toStr
                      DisplayName = "Administrator" |> toStr
                      Active = true
                      Slug = "admin" |> toStr
                      Type = "NORMAL" |> toStr }
                Expect.equal common.Actor actor "should match"
                let pr: PullRequest =
                    { Id = 11
                      Version = 1
                      Title = "A cool PR" |> toStr
                      Description = None
                      State = "OPEN" |> toStr
                      Open = true
                      Closed = false
                      CreatedDate = 1505783860548L |> Timestamp.fromInt
                      UpdatedDate = 1505783878981L |> Timestamp.fromInt
                      ClosedDate = None
                      FromRef =
                          { Id = "refs/heads/comment-pr" |> toStr
                            DisplayId = "comment-pr" |> toStr
                            LatestCommit = "ddc19f786996396d57e17c8f6d1d05d00318ad10" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      ToRef =
                          { Id = "refs/heads/master" |> toStr
                            DisplayId = "master" |> toStr
                            LatestCommit = "7e48f426f0a6e47c5b5e862c31be6ca965f82c9c" |> CommitHash.fromString
                            Repository =
                                { Slug = "repository" |> toStr
                                  Id = 84
                                  Name = "repository" |> toStr
                                  ScmId = "git" |> toStr
                                  State = "AVAILABLE" |> toStr
                                  StatusMessage = "Available" |> toStr
                                  Forkable = true
                                  Project =
                                      { Key = "PROJ" |> toStr
                                        Id = 84
                                        Name = "project" |> toStr
                                        Owner = Ownership.Public false
                                        Type = "NORMAL" |> toStr }
                                  Public = false } }
                      Locked = false
                      Author =
                          { User =
                                { Name = "admin" |> toStr
                                  Email = "admin@example.com" |> toStr
                                  Id = 1
                                  DisplayName = "Administrator" |> toStr
                                  Active = true
                                  Slug = "admin" |> toStr
                                  Type = "NORMAL" |> toStr }
                            Role = "AUTHOR" |> toStr
                            Approved = false
                            Status = "UNAPPROVED" |> toStr
                            LastReviewedCommit = None }
                      Reviewers =
                          [||]
                      Participants =
                          [||] }
                Expect.equal common.PullRequest pr "should match"
                Expect.equal
                    comment
                    { Id = 62
                      Version = 1
                      Text = "I am a PR comment that was edited" |> toStr
                      Author =
                          { Name = "admin" |> toStr
                            Email = "admin@example.com" |> toStr
                            Id = 1
                            DisplayName = "Administrator" |> toStr
                            Active = true
                            Slug = "admin" |> toStr
                            Type = "NORMAL" |> toStr }
                      CreatedDate = 1505784066751L |> Timestamp.fromInt
                      UpdatedDate = 1505784259446L |> Timestamp.fromInt }
                    "should match"
                test <@ parent = Some(43) @>
            | x -> failtestNoStackf "should be a comment deleted event but got %A" x

[<Tests>]
let atlassianExamples =
    testList "Atlassian Examples"
        [ AtlassianExamples.prOpen
          AtlassianExamples.prModified
          AtlassianExamples.prReviewerUpdated
          AtlassianExamples.prApproved
          AtlassianExamples.prUnapproved
          AtlassianExamples.prNeedsWork
          AtlassianExamples.prMerged
          AtlassianExamples.prDeclined
          AtlassianExamples.prDeleted
          AtlassianExamples.prCommentAdded
          AtlassianExamples.prCommentEdited
          AtlassianExamples.prCommentDeleted ]
