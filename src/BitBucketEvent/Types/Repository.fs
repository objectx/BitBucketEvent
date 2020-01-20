//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Repository

open BitBucketEvent.Types.Project
open Thoth.Json.Net

[<Literal>]
let private _Slug = "slug"

[<Literal>]
let private _Id = "id"

[<Literal>]
let private _Name = "name"

[<Literal>]
let private _ScmId = "scmId"

[<Literal>]
let private _State = "state"

[<Literal>]
let private _StatusMessage = "statusMessage"

[<Literal>]
let private _Forkable = "forkable"

[<Literal>]
let private _Project = "project"

[<Literal>]
let private _Public = "public"

type Repository =
    { Slug: string
      Id: int
      Name: string
      ScmId: string
      State: string
      StatusMessage: string
      Forkable: bool
      Project: Project
      Public: bool }

let def: Repository =
    { Slug = ""
      Id = -1
      Name = ""
      ScmId = ""
      State = ""
      StatusMessage = ""
      Forkable = false
      Project = Project.def
      Public = false }

let decoder: Decoder<Repository> =
    Decode.object <| fun get ->
        { Slug = get.Required.Field _Slug Decode.string
          Id = get.Required.Field _Id Decode.int
          Name = get.Required.Field _Name Decode.string
          ScmId = get.Required.Field _ScmId Decode.string
          State = get.Required.Field _State Decode.string
          StatusMessage = get.Required.Field _StatusMessage Decode.string
          Forkable = get.Required.Field _Forkable Decode.bool
          Project = get.Required.Field _Project Project.decoder
          Public = get.Required.Field _Public Decode.bool }

let toJsonValue (x: Repository): JsonValue =
    Encode.object
        [ _Slug, x.Slug |> Encode.string
          _Id, x.Id |> Encode.int
          _Name, x.Name |> Encode.string
          _ScmId, x.ScmId |> Encode.string
          _State, x.State |> Encode.string
          _StatusMessage, x.StatusMessage |> Encode.string
          _Forkable, x.Forkable |> Encode.bool
          _Project, x.Project |> Project.toJsonValue
          _Public, x.Public |> Encode.bool ]
