module Tests

open BitBucketEvent.Types
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
            let makeRef (id: NonEmptyString) (displayId: NonEmptyString) (latestCommit: NonEmptyString)
                (repo: Repository.Repository): Reference.Reference =
                { Id = id.Get
                  DisplayId = displayId.Get
                  LatestCommit = latestCommit.Get
                  Repository = repo }

            let gRepo = Arb.generate<Repository.Repository>
            let gRef = makeRef <!> gStr <*> gStr <*> gStr <*> gRepo
            gRef |> Arb.fromGen

        static member Participant(): Arbitrary<Participant.Participant> =
            let makeParticipant user (role: NonEmptyString) approved (status: NonEmptyString) (lastReviewed: NonEmptyString option): Participant.Participant =
                { User = user
                  Role = role.Get
                  Approved = approved
                  Status = status.Get
                  LastReviewedCommit =
                      match lastReviewed with
                      | None -> None
                      | Some x -> Some (x.Get) }
            let gUser = Arb.generate<User.User>
            let gParticipant =
                makeParticipant <!> gUser <*> gStr <*> gBool <*> gStr <*> (Gen.optionOf gStr)
            gParticipant |> Arb.fromGen

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
    ]
