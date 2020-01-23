//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Reference

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net


type Reference =
    { Id: NonNullString
      DisplayId: NonNullString
      LatestCommit: CommitHash.CommitHash
      Repository: Repository.Repository }

let def: Reference =
    { Id = NonNullString.empty
      DisplayId = NonNullString.empty
      LatestCommit = [||]
      Repository = Repository.def }

let decoder: Decoder<Reference> =
    Decode.object <| fun get ->
        { Id = get.Required.Field _Id NonNullString.decoder
          DisplayId = get.Required.Field _DisplayId NonNullString.decoder
          LatestCommit = get.Required.Field _LatestCommit CommitHash.decoder
          Repository = get.Required.Field _Repository Repository.decoder }

let toJsonValue (x: Reference): JsonValue =
    Encode.object
        [ _Id, (x.Id |> NonNullString.toJsonValue)
          _DisplayId, x.DisplayId.asJsonValue
          _LatestCommit, x.LatestCommit |> CommitHash.toJsonValue
          _Repository, x.Repository |> Repository.toJsonValue ]
