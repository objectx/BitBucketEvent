//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Types.Literals
open BitBucketEvent.Types.Primitives
open Thoth.Json.Net


type Repository =
    { Slug: NonNullString
      Id: int
      Name: NonNullString
      ScmId: NonNullString
      State: NonNullString
      StatusMessage: NonNullString
      Forkable: bool
      Project: Project
      Public: bool }

    static member def: Repository =
        { Slug = NonNullString.empty
          Id = -1
          Name = NonNullString.empty
          ScmId = NonNullString.empty
          State = NonNullString.empty
          StatusMessage = NonNullString.empty
          Forkable = false
          Project = Project.def
          Public = false }

    static member decoder: Decoder<Repository> =
        Decode.object <| fun get ->
            { Slug = get.Required.Field _Slug NonNullString.decoder
              Id = get.Required.Field _Id Decode.int
              Name = get.Required.Field _Name NonNullString.decoder
              ScmId = get.Required.Field _ScmId NonNullString.decoder
              State = get.Required.Field _State NonNullString.decoder
              StatusMessage = get.Required.Field _StatusMessage NonNullString.decoder
              Forkable = get.Required.Field _Forkable Decode.bool
              Project = get.Required.Field _Project Project.decoder
              Public = get.Required.Field _Public Decode.bool }

    static member toJsonValue (x: Repository): JsonValue =
        Encode.object
            [ _Slug, x.Slug |> NonNullString.toJsonValue
              _Id, x.Id |> Encode.int
              _Name, x.Name |> NonNullString.toJsonValue
              _ScmId, x.ScmId |> NonNullString.toJsonValue
              _State, x.State |> NonNullString.toJsonValue
              _StatusMessage, x.StatusMessage |> NonNullString.toJsonValue
              _Forkable, x.Forkable |> Encode.bool
              _Project, x.Project.asJsonValue
              _Public, x.Public |> Encode.bool ]

    member inline self.asJsonValue = self |> Repository.toJsonValue
