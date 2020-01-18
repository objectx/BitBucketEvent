//
// Copyright (c) 2020  Masashi Fujita  All rights reserved.
//
module BitBucketEvent.Types

open System
open Thoth.Json.Net

module User =
    [<Literal>]
    let private _Name = "name"

    [<Literal>]
    let private _EmailAddress = "emailAddress"

    [<Literal>]
    let private _Id = "id"

    [<Literal>]
    let private _DisplayName = "displayName"

    [<Literal>]
    let private _Active = "active"

    [<Literal>]
    let private _Slug = "slug"

    [<Literal>]
    let private _Type = "type"

    type User =
        { Name: string
          Email: string
          Id: int
          DisplayName: string
          Active: bool
          Slug: string
          Type: string }

    let def =
        { Name = ""
          Email = ""
          Id = -1
          DisplayName = ""
          Active = false
          Slug = ""
          Type = "" }

    let decoder: Decoder<User> =
        Decode.object <| fun get ->
            { Name = get.Required.Field _Name Decode.string
              Email = get.Required.Field _EmailAddress Decode.string
              Id = get.Required.Field _Id Decode.int
              DisplayName = get.Required.Field _DisplayName Decode.string
              Active = get.Required.Field _Active Decode.bool
              Slug = get.Required.Field _Slug Decode.string
              Type = get.Required.Field _Type Decode.string }

    let toJsonValue (x: User) =
        Encode.object
            [ _Name, Encode.string x.Name
              _EmailAddress, Encode.string x.Email
              _Id, Encode.int x.Id
              _DisplayName, Encode.string x.DisplayName
              _Active, Encode.bool x.Active
              _Slug, Encode.string x.Slug
              _Type, Encode.string x.Type ]

module Project =
    [<Literal>]
    let private _Key = "key"

    [<Literal>]
    let private _Id = "id"

    [<Literal>]
    let private _Name = "name"

    [<Literal>]
    let private _Public = "public"

    [<Literal>]
    let private _Type = "type"

    type Project =
        { Key: string
          Id: int
          Name: string
          Public: bool
          Type: string }

    let def =
        { Key = ""
          Id = -1
          Name = ""
          Public = false
          Type = "" }

    let decoder: Decoder<Project> =
        Decode.object <| fun get ->
            { Key = get.Required.Field _Key Decode.string
              Id = get.Required.Field _Id Decode.int
              Name = get.Required.Field _Name Decode.string
              Public = get.Required.Field _Public Decode.bool
              Type = get.Required.Field _Type Decode.string }

    let toJsonValue x =
        Encode.object
            [ _Key, Encode.string x.Key
              _Id, Encode.int x.Id
              _Name, Encode.string x.Name
              _Public, Encode.bool x.Public
              _Type, Encode.string x.Type ]
