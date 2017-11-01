# Helm
[![Build Status](https://travis-ci.org/carlosmaniero/Helm.svg?branch=master)](https://travis-ci.org/carlosmaniero/Helm)

Helm is a simple architecture elm-based for Haskell programs.

## Helm's architecture

The Helm's architecture consists in three two important things:

- A model that represents the application state
- A updated that receives messages and update the model


A Helm application is created with a `model`, a start `command`
and a `updater`.

It will return a Loop where the update calls are invoked concurrently
and all IO operations (`commands`) are delegated to `command processor`
that runs the given `command` parallelly.

All `command` resultes in a `msg`. Each `msg` is passed as parameter
to the `updater` that returns the new model and a command to be
processed.

### The external world

The central idea of Helm is to work with the external world with some
configurations and allow developers to focuses in the business logical.

To work with external world is always needed a `adapters` (*ie*: The HTTP
interface). This `adapters` send a `msg` to the application with a context.

A `context` is a important part of the external communication
because most of results of an external call is based in a scope, not in
the global `model`.

![Helm Architecture](https://rawcdn.githack.com/carlosmaniero/Helm/master/docs/architeture.svg)
