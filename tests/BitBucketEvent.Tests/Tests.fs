
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
        static member Project (): Arbitrary<Project.Project> =
            let makeProject (key: NonEmptyString) id (name: NonEmptyString) pub (typ: NonEmptyString): Project.Project =
                { Key = key.Get
                  Id = id
                  Name = name.Get
                  Public = pub
                  Type = typ.Get }
            let gProject = makeProject <!> gStr <*> gId <*> gStr <*> gBool <*> gStr
            gProject |> Arb.fromGen

let config = { FsCheckConfig.defaultConfig with
                    arbitrary = [ typeof<Generator.UserGen> ] }

[<Tests>]
let tests =
    testList "isomorphism" [
        testPropertyWithConfig config "user" <| fun (x: User.User) ->
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
    ]
