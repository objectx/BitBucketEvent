//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Reference

open BitBucketEvent.Types.Repository
open Thoth.Json.Net


[<Literal>]
let private _Id = "id"

[<Literal>]
let private _DisplayId = "displayId"

[<Literal>]
let private _LatestCommit = "latestCommit"

[<Literal>]
let private _Repository = "repository"

type Reference =
    { Id: string
      DisplayId: string
      LatestCommit: CommitHash.CommitHash
      Repository: Repository.Repository }

let def: Reference =
    { Id = ""
      DisplayId = ""
      LatestCommit = [||]
      Repository = Repository.def }

let decoder: Decoder<Reference> =
    Decode.object <| fun get ->
        { Id = get.Required.Field _Id Decode.string
          DisplayId = get.Required.Field _DisplayId Decode.string
          LatestCommit = get.Required.Field _LatestCommit Decode.string |> CommitHash.fromString
          Repository = get.Required.Field _Repository Repository.decoder }

let toJsonValue (x: Reference): JsonValue =
    Encode.object
        [ _Id, x.Id |> Encode.string
          _DisplayId, x.DisplayId |> Encode.string
          (_LatestCommit,
           x.LatestCommit
           |> CommitHash.toString
           |> Encode.string)
          _Repository, x.Repository |> Repository.toJsonValue ]
