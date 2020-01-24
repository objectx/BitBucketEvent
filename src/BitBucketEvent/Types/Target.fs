//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
open Thoth.Json.Net


type Target =
    { Id: NonNullString
      DisplayId: NonNullString
      Type: NonNullString
      LatestCommit: CommitHash
      LatestChangeset: CommitHash }

module Target =
    let decoder: Decoder<Target> =
        Decode.object <| fun get ->
            { Id = get.Required.Field _Id NonNullString.decoder
              DisplayId = get.Required.Field _DisplayId NonNullString.decoder
              Type = get.Required.Field _Type NonNullString.decoder
              LatestCommit = get.Required.Field _LatestCommit CommitHash.decoder
              LatestChangeset = get.Required.Field _LatestChangeset CommitHash.decoder }

    let toJsonValue (x: Target): JsonValue =
        Encode.object
            [ _Id, x.Id.AsJsonValue
              _DisplayId, x.DisplayId.AsJsonValue
              _Type, x.Type.AsJsonValue
              _LatestCommit, x.LatestCommit.AsJsonValue
              _LatestChangeset, x.LatestChangeset.AsJsonValue ]

type Target with
    member inline self.AsJsonValue = self |> Target.toJsonValue
