//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
namespace BitBucketEvent.Types

open BitBucketEvent.Literals
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

module Repository =
    let def: Repository =
        { Slug = NonNullString.empty
          Id = -1
          Name = NonNullString.empty
          ScmId = NonNullString.empty
          State = NonNullString.empty
          StatusMessage = NonNullString.empty
          Forkable = false
          Project = Project.def
          Public = false }

    let decoder: Decoder<Repository> =
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

    let toJsonValue (x: Repository): JsonValue =
        Encode.object
            [ _Slug, x.Slug.AsJsonValue
              _Id, x.Id |> Encode.int
              _Name, x.Name.AsJsonValue
              _ScmId, x.ScmId.AsJsonValue
              _State, x.State.AsJsonValue
              _StatusMessage, x.StatusMessage.AsJsonValue
              _Forkable, x.Forkable |> Encode.bool
              _Project, x.Project.AsJsonValue
              _Public, x.Public |> Encode.bool ]

type Repository with
    member inline self.AsJsonValue = self |> Repository.toJsonValue
