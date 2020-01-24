//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open Thoth.Json.Net


type Reference =
    { Id: NonNullString
      DisplayId: NonNullString
      LatestCommit: CommitHash
      Repository: Repository }

module Reference =
    let def: Reference =
        { Id = NonNullString.empty
          DisplayId = NonNullString.empty
          LatestCommit = CommitHash.def
          Repository = Repository.def }

    let decoder: Decoder<Reference> =
        Decode.object <| fun get ->
            { Id = get.Required.Field _Id NonNullString.decoder
              DisplayId = get.Required.Field _DisplayId NonNullString.decoder
              LatestCommit = get.Required.Field _LatestCommit CommitHash.decoder
              Repository = get.Required.Field _Repository Repository.decoder }

    let toJsonValue (x: Reference): JsonValue =
        Encode.object
            [ _Id, x.Id.AsJsonValue
              _DisplayId, x.DisplayId.AsJsonValue
              _LatestCommit, x.LatestCommit.AsJsonValue
              _Repository, x.Repository.AsJsonValue ]

type Reference with
    member inline self.AsJsonValue = self |> Reference.toJsonValue
