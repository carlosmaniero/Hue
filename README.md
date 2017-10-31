# Helm
[![Build Status](https://travis-ci.org/carlosmaniero/Helm.svg?branch=master)](https://travis-ci.org/carlosmaniero/Helm)

Helm is a simple architecture elm-based for Haskell programs.

## Helm's architecture

The application is created with a `model` and start `command` and a `updater`.
It will return a Loop is that execute concurrently the update
calls and delegate `commands` to `command processor` to run the
given `command` parallelly.

All `command` returns a `msg`. This `msg` will be passed as parameter
to the given `updater`. The `updater` receives the current model
(aka: application state) and a `msg` and return the new model to be
used in next iterations.

### The external world

The central idea of Helm is to work with the external world with some
configurations and allow developers to focuses in the business logical.

To work with external world is needed some `adapters` (*ie*: The HTTP
interface). This `adapters` always send a `msg` to the application with
a context. A `context` is a important part of the external communication
because most of results of an external call is based in a scope, not in
the global `model`.

![Helm Architecture](https://rawcdn.githack.com/carlosmaniero/Helm/master/docs/architeture.svg)
