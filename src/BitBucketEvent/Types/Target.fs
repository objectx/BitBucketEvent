//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net


type Target =
    { Id: NonNullString
      DisplayId: NonNullString
      Type: NonNullString
      LatestCommit: CommitHash
      LatestChangeset: CommitHash }

    static member decoder: Decoder<Target> =
        Decode.object <| fun get ->
            { Id = get.Required.Field _Id NonNullString.decoder
              DisplayId = get.Required.Field _DisplayId NonNullString.decoder
              Type = get.Required.Field _Type NonNullString.decoder
              LatestCommit = get.Required.Field _LatestCommit CommitHash.decoder
              LatestChangeset = get.Required.Field _LatestChangeset CommitHash.decoder }

    static member toJsonValue (x: Target): JsonValue =
        Encode.object
            [ _Id, x.Id.asJsonValue
              _DisplayId, x.DisplayId.asJsonValue
              _Type, x.Type.asJsonValue
              _LatestCommit, x.LatestCommit.asJsonValue
              _LatestChangeset, x.LatestChangeset.asJsonValue ]

    member inline self.asJsonValue = self |> Target.toJsonValue
