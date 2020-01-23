//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types.Repository

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
        [ _Slug, x.Slug |> NonNullString.toJsonValue
          _Id, x.Id |> Encode.int
          _Name, x.Name |> NonNullString.toJsonValue
          _ScmId, x.ScmId |> NonNullString.toJsonValue
          _State, x.State |> NonNullString.toJsonValue
          _StatusMessage, x.StatusMessage |> NonNullString.toJsonValue
          _Forkable, x.Forkable |> Encode.bool
          _Project, x.Project |> Project.toJsonValue
          _Public, x.Public |> Encode.bool ]
