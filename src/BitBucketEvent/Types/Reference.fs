//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net


type Reference =
    { Id: NonNullString
      DisplayId: NonNullString
      LatestCommit: CommitHash
      Repository: Repository }

    static member def: Reference =
        { Id = NonNullString.empty
          DisplayId = NonNullString.empty
          LatestCommit = CommitHash.def
          Repository = Repository.def }

    static member decoder: Decoder<Reference> =
        Decode.object <| fun get ->
            { Id = get.Required.Field _Id NonNullString.decoder
              DisplayId = get.Required.Field _DisplayId NonNullString.decoder
              LatestCommit = get.Required.Field _LatestCommit CommitHash.decoder
              Repository = get.Required.Field _Repository Repository.decoder }

    static member toJsonValue (x: Reference): JsonValue =
        Encode.object
            [ _Id, (x.Id |> NonNullString.toJsonValue)
              _DisplayId, x.DisplayId.asJsonValue
              _LatestCommit, x.LatestCommit |> CommitHash.toJsonValue
              _Repository, x.Repository.asJsonValue ]

    member inline self.asJsonValue =
        self |> Reference.toJsonValue
